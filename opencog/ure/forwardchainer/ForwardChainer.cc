/*
 * ForwardChainer.cc
 *
 * Copyright (C) 2014,2015 OpenCog Foundation
 * Copyright (C) 2019,2020 SingularityNET Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com>
 *          Nil Geisweiller <ngeiswei@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <future>
#include <thread>
#include <chrono>

#include <boost/range/adaptor/reversed.hpp>

#include <opencog/util/random.h>
#include <opencog/util/pool.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atoms/pattern/PatternUtils.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/ure/Rule.h>

#include "ForwardChainer.h"
#include "../URELogger.h"
#include "../backwardchainer/ControlPolicy.h"
#include "../ThompsonSampling.h"

using namespace opencog;

ForwardChainer::ForwardChainer(AtomSpace& kb_as,
                               AtomSpace& rb_as,
                               const Handle& rbs,
                               const Handle& source,
                               const Handle& vardecl,
                               AtomSpace* trace_as,
                               const HandleSeq& focus_set)
	: _kb_as(kb_as),
	  _rb_as(rb_as),
	  _config(rb_as, rbs),
	  _thread_count(0),
	  _sources(_config, source, vardecl),
	  _fcstat(trace_as),
	  _srpi(true)
{
	init(source, vardecl, focus_set);
}

ForwardChainer::ForwardChainer(AtomSpace& kb_as,
                               const Handle& rbs,
                               const Handle& source,
                               const Handle& vardecl,
                               AtomSpace* trace_as,
                               const HandleSeq& focus_set)
	: ForwardChainer(kb_as,
	                 rbs->getAtomSpace() ? *rbs->getAtomSpace() : kb_as,
	                 rbs, source, vardecl, trace_as, focus_set)
{
}

ForwardChainer::~ForwardChainer()
{
}

void ForwardChainer::init(const Handle& source,
                          const Handle& vardecl,
                          const HandleSeq& focus_set)
{
	validate(source);

	_focus_set_as = createAtomSpace();
	_search_focus_set = not focus_set.empty();

	// Add focus set atoms and sources to focus_set atomspace
	if (_search_focus_set) {
		for (const Handle& h : focus_set)
			_focus_set_as->add_atom(h);
		for (const SourcePtr& src : _sources.sources)
			_focus_set_as->add_atom(src->body);
	}

	// Set rules.
	_rules = _config.get_rules();
	// TODO: For now the FC follows the old standard. We may move to
	// the new standard when all rules have been ported to the new one.
	for (RulePtr rule : _rules)
		rule->premises_as_clauses = true;

	// Reset the iteration count
	_iteration = 0;
}

UREConfig& ForwardChainer::get_config()
{
	return _config;
}

const UREConfig& ForwardChainer::get_config() const
{
	return _config;
}

void ForwardChainer::do_chain()
{
	ure_logger().debug("Start forward chaining");
	LAZY_URE_LOG_DEBUG << "With rule set:" << std::endl << oc_to_string(_rules);

	// Relex2Logic uses this. TODO make a separate class to handle
	// this robustly.
	if(_sources.empty())
	{
		apply_all_rules();
		return;
	}

	if (_srpi)
	{
		do_steps_srpi();
	}
	else if (_config.get_jobs() <= 1)
	{
		// Do steps single-threadedly till termination
		do_steps_singlethread();
	} else
	{
		// Set log thread ID if multi-threaded
		bool prev_thread_id = ure_logger().get_thread_id_flag();
		ure_logger().set_thread_id_flag(true);

		// Do steps multi-threadedly till termination
		do_steps_multithread();

		// Restore logging thread ID flag
		ure_logger().set_thread_id_flag(prev_thread_id);
	}

	// Log termination messages
	termination_log();
	LAZY_URE_LOG_DEBUG << "Finished forward chaining with results:"
	                   << std::endl << oc_to_string(get_results_set());
}

void ForwardChainer::do_steps_singlethread()
{
	while (not termination()) do_step(_iteration++);
}

// TODO: if creating/destroying threads is too expensive, use a thread
// pool (see boost::asio::thread_pool).
void ForwardChainer::do_steps_multithread()
{
	// Create a pool of iterations
	opencog::pool<int> itrpool;

	// Run steps in parallel
	while (not termination())
	{
		if (_thread_count < _config.get_jobs())
			itrpool.give_back(_iteration++);

		int local_iteration = itrpool.borrow();
		if (local_iteration < 0)
			// A negative iteration resource is freed to unstuck the main
			// thread and indicates that the process has terminated.
			break;

		_thread_count++;
		auto do_step_manage = [=,&itrpool]() {
			do_step(local_iteration);
			itrpool.give_back(termination() ? -1 : _iteration++);
			_thread_count--; };
		auto thrd = std::thread(do_step_manage);
		thrd.detach();
	}

	// Wait for the remaining threads to terminate
	using namespace std::chrono_literals;
	while (0 < _thread_count) std::this_thread::sleep_for(1ms);
}

void ForwardChainer::do_steps_srpi()
{
	while (not termination()) do_step_srpi(_iteration++);
}

void ForwardChainer::do_step(int iteration)
{
	int lipo = iteration + 1;
	std::string msgprfx = std::string("[I-") + std::to_string(lipo) + "] ";
	ure_logger().debug() << msgprfx << "Start iteration (" << lipo
	                     << "/" << _config.get_maximum_iterations_str() << ")";

	// Expand meta rules. This should probably be done on-the-fly in
	// the select_rule method, but for now it's here
	expand_meta_rules(msgprfx);

	// Select source
	SourcePtr source = select_source(msgprfx);
	if (source) {
		LAZY_URE_LOG_DEBUG << msgprfx << "Selected source:" << std::endl
		                   << source->to_string();
	} else {
		LAZY_URE_LOG_DEBUG << msgprfx << "No selected source, abort iteration";
		return;
	}

	// Select rule
	RuleProbabilityPair rule_prob = select_rule(*source, msgprfx);
	RulePtr rule = rule_prob.first;
	double prob(rule_prob.second);
	if (not rule->is_valid()) {
		ure_logger().debug() << msgprfx << "No selected rule, abort iteration";
		return;
	} else {
		LAZY_URE_LOG_DEBUG << msgprfx << "Selected rule, with probability " << prob
		                   << " of success:" << std::endl << rule->to_string();
	}

	bool success = source->insert_rule(rule);
	if (success) {
		// Apply rule on source
		HandleSet products = apply_rule(*rule);

		// Insert the produced sources in the population of sources
		_sources.insert(products, *source, prob, msgprfx);

		// The rule has been applied, we can set the exhausted flag
		source->set_rule_exhausted(rule);

		// Save trace and results
		_fcstat.add_inference_record(iteration, source->body, *rule, products);
	} else {
		LAZY_URE_LOG_DEBUG << msgprfx << "Rule " << rule->to_short_string()
		                   << " is probably being applied on source "
		                   << source->body->id_to_string()
		                   << " in another thread, abort iteration";
	}
}

void ForwardChainer::do_step_srpi(int iteration)
{
	int lipo = iteration + 1;
	std::string msgprfx = std::string("[I-") + std::to_string(lipo) + "] ";
	ure_logger().debug() << msgprfx << "Start iteration (" << lipo
	                     << "/" << _config.get_maximum_iterations_str() << ")";

	// Expand meta rules. This should probably be done on-the-fly in
	// the select_rule method, but for now it's here.
	expand_meta_rules(msgprfx);

	// Populate the source rule set
	populate_source_rule_set(msgprfx);

	// Select source rule pair for application
	auto [slc_sr, slc_tv] = select_source_rule(msgprfx);
	if (slc_sr.is_valid()) {
		double success_plty = BetaDistribution(slc_tv).mean();
		LAZY_URE_LOG_DEBUG << msgprfx
		                   << "Selected source rule pair with probability "
								 << success_plty << " of success:" << std::endl
		                   << oc_to_string(slc_sr);

		// Apply selected source rule pair
		HandleSet products = apply_rule(slc_sr);

		// Insert the produced sources in the population of sources
		//
		// The probability of success is renormalized by the weight
		// before being passed to the new source constructor, as this
		// one will take it into account.
		//
		// TODO: This can be simplified but is let here until do_step is
		// replaced by do_step_srpi.
		double weight = std::min(1.0, slc_sr.source->weight);
		double prob = success_plty / weight;
		_sources.insert(products, *slc_sr.source, prob, msgprfx);

		// The rule has been applied, we can set the exhausted flag
		slc_sr.source->set_rule_exhausted(slc_sr.rule);

		// Save trace and results
		_fcstat.add_inference_record(iteration, slc_sr.source->body,
		                             *slc_sr.rule, products);
	} else {
		LAZY_URE_LOG_DEBUG << msgprfx
		                   << "Failed to select a source rule pair, "
		                   << "abort iteration";
	}
}

bool ForwardChainer::termination()
{
	bool terminate = false;

	// Terminate if all source rule pairs have been tried
	if (_sources.is_exhausted() and _source_rule_set.empty()) {
		terminate = true;
	}
	// Terminate if max iterations has been reached
	else if (0 <= _config.get_maximum_iterations() and
	         _config.get_maximum_iterations() <= _iteration) {
		terminate = true;
	}

	return terminate;
}

void ForwardChainer::termination_log()
{
	std::string msg;

	// Terminate if all sources have been tried
	if (_sources.is_exhausted() and _source_rule_set.empty()) {
		msg = "all source rule pairs have been exhausted";
	}
	// Terminate if max iterations has been reached
	else if (0 <= _config.get_maximum_iterations() and
	         _config.get_maximum_iterations() <= _iteration) {
		msg = "reach maximum number of iterations";
	}

	ure_logger().debug() << "Terminate: " << msg;
}

/**
 * Applies all rules in the rule base.
 */
void ForwardChainer::apply_all_rules()
{
	for (const RulePtr& rule : _rules) {
		ure_logger().debug("Apply rule %s", rule->get_name().c_str());
		HandleSet uhs = apply_rule(*rule);

		// Update
		_fcstat.add_inference_record(_iteration,
		                             _kb_as.add_node(CONCEPT_NODE, "dummy-source"),
		                             *rule, uhs);
	}
}

Handle ForwardChainer::get_results() const
{
	HandleSet rs = get_results_set();
	HandleSeq results(rs.begin(), rs.end());
	return _kb_as.add_link(SET_LINK, std::move(results));
}

HandleSet ForwardChainer::get_results_set() const
{
	return _fcstat.get_all_products();
}

SourcePtr ForwardChainer::select_source(const std::string& msgprfx)
{
	// TODO: refine mutex
	std::unique_lock<std::mutex> lock(_part_mutex);

	std::vector<double> weights = _sources.get_weights();

	// Debug log
	if (ure_logger().is_debug_enabled()) {
		OC_ASSERT(weights.size() == _sources.size());
		size_t wi = 0;
		// Sort sources according to their weights
		std::multimap<double, Handle> weighted_sources;
		for (size_t i = 0; i < weights.size(); i++) {
			if (0 < weights[i]) {
				wi++;
				if (ure_logger().is_fine_enabled()) {
					weighted_sources.insert({weights[i], _sources.sources[i]->body});
				}
			}
		}
		LAZY_URE_LOG_DEBUG << msgprfx << "Positively weighted sources ("
		                   << wi << "/" << weights.size() << ")";
		if (ure_logger().is_fine_enabled()) {
			std::stringstream ws_ss;
			for (const auto& wsp : boost::adaptors::reverse(weighted_sources))
				ws_ss << std::endl << wsp.first << " " << wsp.second->id_to_string();
			LAZY_URE_LOG_FINE << msgprfx << ws_ss.str();
		}
	}

	// Calculate the total weight to be sure it's greater than zero
	double total = boost::accumulate(weights, 0.0);

	if (total == 0.0) {
		ure_logger().debug() << msgprfx << "All sources have been exhausted";
		if (_config.get_retry_exhausted_sources()) {
			ure_logger().debug() << msgprfx
			                     << "Reset all exhausted flags to retry them";
			// TODO: This has the effect of deallocating the rules, which
			// might cause a memory corruption if another thread is
			// attempting to apply that rule at the same time.
			_sources.reset_exhausted();
			// Try again
			lock.unlock();
			return select_source(msgprfx);
		} else {
			_sources.set_exhausted();
			return nullptr;
		}
	}

	// Sample sources according to this distribution
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	return *std::next(_sources.sources.begin(), dist(randGen()));
}

SourceRule ForwardChainer::mk_source_rule(const std::string& msgprfx)
{
	SourcePtr source = select_source(msgprfx);
	if (source) {
		LAZY_URE_LOG_DEBUG << msgprfx << "Selected source:" << std::endl
		                   << source->to_string();
	} else {
		LAZY_URE_LOG_DEBUG << msgprfx << "No source selected";
		return SourceRule();
	}

	const RuleSet valid_rules = get_valid_rules(*source);

	// Log valid rules
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		if (valid_rules.empty())
			ss << msgprfx << "No valid rule for that source. Let's try again";
		else
			ss << msgprfx << "The following rules are valid:" << std::endl
			   << valid_rules.to_short_string();
		LAZY_URE_LOG_DEBUG << ss.str();
	}

	if (valid_rules.empty()) {
		source->set_exhausted();
		// Try again, in case another source is available
		return mk_source_rule(msgprfx);
	}

	// Thompson sample according to rule tvs
	TruthValueSeq tvs = valid_rules.get_tvs();
	RulePtr slc_rule = valid_rules[ThompsonSampling(tvs)()];
	bool success = source->insert_rule(slc_rule);
	if (not success)
		return SourceRule();
	source->set_rule_exhausted(slc_rule);
	SourceRule sr(source, slc_rule);

	LAZY_URE_LOG_FINE << msgprfx
	                  << "Produced source rule pair:" << std::endl
	                  << sr.to_string();

	return sr;
}

void ForwardChainer::populate_source_rule_set(const std::string& msgprfx)
{
	LAZY_URE_LOG_DEBUG << msgprfx << "Populate the source rule set (size="
	                   << _source_rule_set.size() << ")";
	int eps = _config.get_expansion_pool_size();
	while (eps <= 0 or (int)_source_rule_set.size() < eps) {
		// Build (source, rule) pair for application trial
		SourceRule sr = mk_source_rule(msgprfx);
		if (not sr.is_valid()) {
			LAZY_URE_LOG_DEBUG << msgprfx
			                   << "Failed to build a source rule pair, "
			                   << "abort populating source rule set";
			break;
		}

		// Insert it to the source rule set
		TruthValuePtr tv = calculate_source_rule_tv(sr);
		bool success = _source_rule_set.insert(sr, tv);
		if (not success) {
			LAZY_URE_LOG_DEBUG << msgprfx
			                   << "Source rule pair:" << std::endl
			                   << oc_to_string(sr) << std::endl
			                   << "already in the source rule set";
		}
	}

	LAZY_URE_LOG_DEBUG << msgprfx << "Source rule set after population (size="
	                   << _source_rule_set.size() << ")";
	LAZY_URE_LOG_FINE << msgprfx << std::endl << _source_rule_set.to_string();
}

std::pair<SourceRule, TruthValuePtr>
ForwardChainer::select_source_rule(const std::string& msgprfx)
{
	return _source_rule_set.thompson_select();
}

TruthValuePtr ForwardChainer::calculate_source_rule_tv(const SourceRule& sr)
{
	double weight = std::min(1.0, sr.source->weight);
	TruthValuePtr tv = sr.rule->get_tv();
	BetaDistribution beta_dstr(tv);
	// Mean and standard deviation directly scale with the weight
	double scaled_mean = weight * beta_dstr.mean();
	double scaled_variance = sq(weight) * beta_dstr.variance();
	return mk_stv(scaled_mean, scaled_variance);
}

RuleSet ForwardChainer::get_valid_rules(const Source& source)
{
	std::lock_guard<std::mutex> lock(_rules_mutex); // TODO: refine

	// Generate all valid rules
	RuleSet valid_rules;
	for (const RulePtr& rule : _rules) {
		// For now ignore meta rules as they are instantiated in
		// do_step()
		if (rule->is_meta())
			continue;

		const AtomSpace& ref_as(_search_focus_set ? *_focus_set_as.get() : _kb_as);
		RuleTypedSubstitutionMap urm =
			rule->unify_source(source.body, source.vardecl, &ref_as);
		RuleSet unified_rules = Rule::strip_typed_substitution(urm);

		// Only insert unexhausted rules for this source
		RuleSet une_rules;
		if (_config.get_full_rule_application()) {
			// Insert the unaltered rule, which will have the effect of
			// applying to all sources, not just this one. Convenient for
			// quickly achieving inference closure, albeit expensive.
			if (not unified_rules.empty() and not source.is_rule_exhausted(rule)) {
				une_rules.insert(rule);
			}
		} else {
			// Insert all specializations obtained from unification
			for (RulePtr ur : unified_rules) {
				if (not source.is_rule_exhausted(ur)) {
					une_rules.insert(ur);
				}
			}
		}

		valid_rules.insert(une_rules.begin(), une_rules.end());
	}
	return valid_rules;
}

RuleProbabilityPair ForwardChainer::select_rule(const Handle& h,
                                                const std::string& msgprfx)
{
	Source src(h);
	return select_rule(src, msgprfx);
}

RuleProbabilityPair ForwardChainer::select_rule(Source& source,
                                                const std::string& msgprfx)
{
	const RuleSet valid_rules = get_valid_rules(source);

	// Log valid rules
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		if (valid_rules.empty())
			ss << msgprfx << "No valid rule";
		else
			ss << msgprfx << "The following rules are valid:" << std::endl
			   << valid_rules.to_short_string();
		LAZY_URE_LOG_DEBUG << ss.str();
	}

	if (valid_rules.empty()) {
		source.set_exhausted();
		return RuleProbabilityPair{nullptr, 0.0};
	}

	return select_rule(valid_rules, msgprfx);
};

RuleProbabilityPair ForwardChainer::select_rule(const RuleSet& valid_rules,
                                                const std::string& msgprfx)
{
	// Build vector of all valid truth values
	TruthValueSeq tvs = valid_rules.get_tvs();

	// Build action selection distribution
	std::vector<double> weights = ThompsonSampling(tvs).distribution();

	// Log the distribution
	if (ure_logger().is_debug_enabled()) {
		std::stringstream ss;
		ss << msgprfx << "Rule weights:";
		size_t i = 0;
		for (const RulePtr& rule : valid_rules) {
			ss << std::endl << weights[i] << " " << rule->get_name();
			i++;
		}
		ure_logger().debug() << ss.str();
	}

	// Sample rules according to the weights
	std::discrete_distribution<size_t> dist(weights.begin(), weights.end());
	RulePtr selected_rule = rand_element(valid_rules, dist);

	// Calculate the probability estimate of having this rule fulfill
	// the objective (required to calculate its complexity)
	double prob = BetaDistribution(selected_rule->get_tv()).mean();

	return RuleProbabilityPair{selected_rule, prob};
}

HandleSet ForwardChainer::apply_rule(const Rule& rule)
{
	HandleSet results;

	// Take the results from applying the rule, add them in the given
	// AtomSpace and insert them in results
	auto add_results = [&](AtomSpace& as, const HandleSeq& hs) {
		for (const Handle& h : hs)
		{
			Type t = h->get_type();
			// If it's a List or Set then add all the results. That
			// kinda means that to infer List or Set themselves you
			// need to Quote them.
			if (t == LIST_LINK or t == SET_LINK) {
				for (const Handle& hc : h->getOutgoingSet()) {
					results.insert(as.add_atom(hc));
				}
			} else {
				results.insert(as.add_atom(h));
			}
		}
	};

	// Wrap in try/catch in case the pattern matcher can't handle it
	try
	{
		AtomSpace& ref_as(_search_focus_set ? *_focus_set_as.get() : _kb_as);
		AtomSpacePtr derived_rule_as(createAtomSpace(&ref_as));
		derived_rule_as->clear_copy_on_write(); // _as should be write-through.
		Handle rhcpy = derived_rule_as->add_atom(rule.get_rule());

		// Make Sure that all constant clauses appear in the AtomSpace
		// as unification might have created constant clauses which aren't
		HandleSeq clauses = rule.get_clauses();
		const HandleSet& varset = rule.get_variables().varset;
		for (Handle clause : clauses)
			if (is_constant(varset, clause))
				if (ref_as.get_atom(clause) == Handle::UNDEFINED)
					return results;

		Handle h = HandleCast(rhcpy->execute(&ref_as));
		add_results(ref_as, h->getOutgoingSet());
	}
	catch (...) {}

	return results;
}

HandleSet ForwardChainer::apply_rule(const SourceRule& sr)
{
	return apply_rule(*sr.rule);
}

void ForwardChainer::validate(const Handle& source)
{
	if (source == Handle::UNDEFINED)
		throw RuntimeException(TRACE_INFO, "ForwardChainer - Invalid source.");
}

void ForwardChainer::expand_meta_rules(const std::string& msgprfx)
{
	std::lock_guard<std::mutex> lock(_rules_mutex);
	// This is kinda of hack before meta rules are fully supported by
	// the Rule class.
	size_t rules_size = _rules.size();
	_rules.expand_meta_rules(_kb_as);

	if (rules_size != _rules.size()) {
		ure_logger().debug() << msgprfx << "The rule set has gone from "
		                     << rules_size << " to " << _rules.size() << " rules";
	}
}
