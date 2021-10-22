/*
 * ForwardChainer.h
 *
 * Copyright (C) 2014,2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
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

#ifndef _OPENCOG_FORWARDCHAINER_H_
#define _OPENCOG_FORWARDCHAINER_H_

#include <mutex>
// #include <shared_mutex>

#include "../UREConfig.h"
#include "SourceSet.h"
#include "SourceRuleSet.h"
#include "FCStat.h"

class ForwardChainerUTest;

namespace opencog
{

enum class source_selection_mode
{
	TV_FITNESS, STI, UNIFORM
};

class Rule;

// Pair of Rule and its probability estimate that it fullfils the
// objective
typedef std::pair<RulePtr, double> RuleProbabilityPair;

class ForwardChainer
{
public:
	/**
	 * Ctor.
	 *
	 * @param kb_as     Knowledge-base atomspace
	 * @param rb_as     Rule-base atomspace
	 * @param rbs       Handle pointing to rule-based system.
	 * @param source    Source to start with, if it is a pattern, or a Set,
	 *                  multiple sources are considered
	 * @param vardecl   Variable declaration of Source if pattern
	 * @param focus_set Set of atoms under focus
	 */
	ForwardChainer(AtomSpace& kb_as,
	               AtomSpace& rb_as,
	               const Handle& rbs,
	               const Handle& source,
	               const Handle& vardecl=Handle::UNDEFINED,
	               AtomSpace* trace_as=nullptr,
	               const HandleSeq& focus_set=HandleSeq());

	/**
	 * Like above, but use as rule-base atomspace, the atomspace of rbs
	 * if any, otherwise use kb_as if rbs has no atomspace.
	 */
	ForwardChainer(AtomSpace& kb_as,
	               const Handle& rbs,
	               const Handle& source,
	               const Handle& vardecl=Handle::UNDEFINED,
	               AtomSpace* trace_as=nullptr,
	               const HandleSeq& focus_set=HandleSeq());
	~ForwardChainer();

	/**
	 * URE configuration accessors
	 */
	UREConfig& get_config();
	const UREConfig& get_config() const;

	/**
	 * Perform forward chaining inference till the termination
	 * criteria have been met.
	 */
	void do_chain();

	/**
	 * run steps (single or multi threaded) until termination criteria
	 * are met.
	 */
	void do_steps_singlethread();
	void do_steps_multithread();

	/**
	 * Source rule producer implementation of do_steps.
	 */
	void do_steps_srpi();

	/**
	 * Perform a single forward chaining inference step on the given
	 * iteration.
	 */
	void do_step(int iteration);

	/**
	 * Source rule producer implementation of do_step.
	 */
	void do_step_srpi(int iteration);

	/**
	 * @return true if the termination criteria have been met.
	 */
	bool termination();

	/**
	 * Log the cause of termination
	 */
	void termination_log();

	/**
	 * @return all results in their order of inference.
	 */
	Handle get_results() const;
	HandleSet get_results_set() const;

private:
	friend class ::ForwardChainerUTest;

	void init(const Handle& source,
	          const Handle& vardecl,
	          const HandleSeq& focus_set);

	void apply_all_rules();

	void validate(const Handle& source);

	/**
	 * Expand all meta rules into mesa rules.
	 *
	 * @param msgprfx is a prefix to prepend before each log message.
	 */
	void expand_meta_rules(const std::string& msgprfx);

	/**
	 * choose next source to expand
	 *
	 * @return  A Source to expand
	 *
	 * Warning: it is not const because the source is gonna be modified
	 * by keeping track of the rules applied to it.
	 */
	SourcePtr select_source(const std::string& msgprfx);

	/**
	 * Build a source rule pair for application trial. If and only if
	 * no such pair is available, then return an invalid pair.
	 */
	SourceRule mk_source_rule(const std::string& msgprfx);

	/**
	 * Populate the source rule set with pairs
	 */
	void populate_source_rule_set(const std::string& msgprfx);

	/**
	 * Select source rule pair
	 */
	std::pair<SourceRule, TruthValuePtr>
	select_source_rule(const std::string& msgprfx);

	/**
	 * Given a source rule pair, calculate its truth value of success.
	 */
	TruthValuePtr calculate_source_rule_tv(const SourceRule& sr);

	/**
	 * Get rules that unify with the source and that are not exhausted,
	 * which include rules currently being run.
	 */
	RuleSet get_valid_rules(const Source& source);

	/**
	 * Choose an applicable rules from the rule base by selecting
	 * rules whose premise structurally matches with the source.
	 *
	 * If no rule can be chosen return invalid rule.
	 *
	 * @return  A rule that in which @param source could ground.
	 *
	 * TODO: move to ControlPolicy
	 */
	RuleProbabilityPair select_rule(const Handle& source,
	                                const std::string& msgprfx="");
	RuleProbabilityPair select_rule(Source& source,
	                                const std::string& msgprfx="");
	RuleProbabilityPair select_rule(const RuleSet&,
	                                const std::string& msgprfx="");

	/**
	 * Apply rule.
	 */
	HandleSet apply_rule(const Rule& rule);
	HandleSet apply_rule(const SourceRule& sr);

	RuleSet _rules; /* loaded rules */

	// Knowledge base atomspace
	AtomSpace& _kb_as;

	// Rule base atomspace (can be the same as _kb_as)
	AtomSpace& _rb_as;

	// The focus set is copied into this atomspace; during chaining,
	// the pattern matcher is applied only to this atomspace.  This
	// is the primary mechanism by which chaining is restricted to
	// the focus set.  This is effective, but not very efficient;
	// perhaps there is some better mechanism?
	AtomSpacePtr _focus_set_as;

	UREConfig _config;

	// Current iteration
	std::atomic<int> _iteration;

	bool _search_focus_set;

	// TODO: subdivide in smaller and shared mutexes
	mutable std::mutex _whole_mutex;
	mutable std::mutex _part_mutex;

	// TODO: use shared mutexes
	mutable std::mutex _rules_mutex;

	// Keep track of the number of threads to make sure
	std::atomic<int> _thread_count;

	// Population of sources to expand forward
	SourceSet _sources;

	FCStat _fcstat;

	// Enable alternative implementation using (source, rule) producer,
	// srpi stands for Source Rule Producer Implementation. This flag
	// is here, likely temporarily, to compare old and new way.
	const bool _srpi;

	// Set of weighted pairs (source, rule).
	SourceRuleSet _source_rule_set;
};

} // ~namespace opencog

#endif /* _OPENCOG_FORWARDCHAINER_H_ */
