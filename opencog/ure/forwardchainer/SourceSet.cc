/*
 * SourceSet.cc
 *
 * Copyright (C) 2018 SingularityNET Foundation
 * Author: Nil Geisweiller
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

#include "SourceSet.h"

#include <boost/range/algorithm/binary_search.hpp>
#include <boost/range/algorithm/lower_bound.hpp>

#include <opencog/util/numeric.h>
#include <opencog/atoms/core/VariableSet.h>

namespace opencog {

bool source_ptr_less::operator()(const SourcePtr& l, const SourcePtr& r) const
{
	return *l < *r;
}

double calculate_weight(const Handle& bdy, double cpx_fctr)
{
	// Calculate weight, for now only one fitness function is hard
	// coded
	//
	// complexity_factor * strength * confidence
	//
	// The minimum value is 1e-16 to not ignore completely the source
	// when the it is a default TV.
	//
	// TODO:
	// 1. Support more fitness functions
	// 2. Explicitely turn the fitness into a probability of success
	TruthValuePtr tv = bdy->getTruthValue();
	double fitness = tv->get_mean() * tv->get_confidence();
	return std::max(1e-16, cpx_fctr * fitness);
}

Source::Source(const Handle& bdy, const Handle& vdcl, double cpx, double cpx_fctr)
	: body(bdy),
	  vardecl(vdcl),
	  complexity(cpx),
	  complexity_factor(cpx_fctr),
	  weight(calculate_weight(bdy, cpx_fctr)),
	  exhausted(false)
{
}

bool Source::operator==(const Source& other) const
{
	return content_eq(body, other.body) and content_eq(vardecl, other.vardecl);
}

bool Source::operator<(const Source& other) const
{
	// Sort by content of body, or if equal of vardecl.
	return (body < other.body)
		or (content_eq(body, other.body) and vardecl < other.vardecl);
}

bool Source::insert_rule(RulePtr rule)
{
	std::lock_guard<std::mutex> lock(_mutex);
	return rules.insert(rule).second;
}

void Source::set_exhausted()
{
	std::lock_guard<std::mutex> lock(_mutex);
	exhausted = true;
}

void Source::reset_exhausted()
{
	std::lock_guard<std::mutex> lock(_mutex);
	exhausted = false;
	rules.clear();
}

bool Source::is_exhausted() const
{
	std::lock_guard<std::mutex> lock(_mutex);
	return exhausted;
}

void Source::set_rule_exhausted(const RulePtr& rule)
{
	std::lock_guard<std::mutex> lock(_mutex);
	auto it = rules.find(rule);
	if (it != rules.end())
		(*it)->set_exhausted();
}

bool Source::is_rule_exhausted(const RulePtr& rule) const
{
	std::lock_guard<std::mutex> lock(_mutex);
	auto it = rules.find(rule);
	// Note that the presence of an alpha-equivalent rule in the source
	// is not enough to being considered exhausted, the exhausted flag
	// of the rule must also be explicited set to true. That is in
	// order to possibly make the distinction between a rule that is
	// being tried and a rule that has already been tried. It's not
	// clear though whether we need this distinction, and if we do we
	// probably should make it explicit in the Source or Rule API.
	return it != rules.end() and (*it)->is_exhausted();
}

double Source::expand_complexity(double prob) const
{
	return complexity - std::log2(prob);
}

double Source::get_weight() const
{
	if (is_exhausted())
		return 0.0;
	return weight;
}

std::string Source::to_string(const std::string& indent) const
{
	std::lock_guard<std::mutex> lock(_mutex);
	std::stringstream ss;
	ss << indent << "body:" << std::endl
	   << oc_to_string(body, indent + oc_to_string_indent) << std::endl
	   << indent << "vardecl:" << std::endl
	   << oc_to_string(vardecl, indent + oc_to_string_indent) << std::endl
	   << indent << "complexity: " << complexity << std::endl
	   << indent << "exhausted: " <<  exhausted << std::endl
	   << indent << "rules:" << std::endl
	   << rules.to_short_string(indent + oc_to_string_indent);
	return ss.str();
}

SourceSet::SourceSet(const UREConfig& config,
                     const Handle& init_source,
                     const Handle& init_vardecl)
	: exhausted(false), _config(config)
{
	if (init_source) {
		// Accept set of initial sources wrapped in a SetLink
		HandleSeq init_sources = init_source->get_type() == SET_LINK ?
			init_source->getOutgoingSet() : HandleSeq{init_source};
		if (init_sources.empty()) {
			exhausted = true;
		} else {
			for (const Handle& src : init_sources) {
				SourcePtr new_src = createSource(src, init_vardecl);
				auto it = boost::lower_bound(sources, new_src, source_ptr_less());
				sources.insert(it, new_src);
			}
		}
	} else {
		exhausted = true;
	}
}

std::vector<double> SourceSet::get_weights() const
{
	std::lock_guard<std::mutex> lock(_mutex);
	std::vector<double> results;
	for (const SourcePtr& src : sources)
		results.push_back(src->get_weight());
	return results;
}

void SourceSet::set_exhausted()
{
	std::lock_guard<std::mutex> lock(_mutex);
	exhausted = true;
}

void SourceSet::reset_exhausted()
{
	std::lock_guard<std::mutex> lock(_mutex);
	if (sources.empty()) {
		exhausted = true;
		return;
	}

	for (SourcePtr& src : sources)
		src->reset_exhausted();
	exhausted = false;
}

bool SourceSet::is_exhausted() const
{
	std::lock_guard<std::mutex> lock(_mutex);
	return exhausted;
}

void SourceSet::insert(const HandleSet& products, const Source& src,
                       double prob, const std::string& msgprfx)
{
	std::lock_guard<std::mutex> lock(_mutex);
	const static Handle empty_variable_set = Handle(createVariableSet(HandleSeq()));

	// Calculate the complexity of the new sources
	double new_cpx = src.expand_complexity(prob);
	double new_cpx_fctr = std::exp2(-_config.get_complexity_penalty() * new_cpx);

	// Keep all new sources
	Sources new_srcs;
	for (const Handle& product : products) {
		SourcePtr new_src = createSource(product, empty_variable_set,
		                                 new_cpx, new_cpx_fctr);

		// Make sure it isn't already in the sources
		if (boost::binary_search(sources, new_src, source_ptr_less())) {
			LAZY_URE_LOG_FINE << msgprfx
			                  << "The following source is already in the population: "
			                  << new_src->body->id_to_string();
		} else {
			new_srcs.push_back(new_src);
		}
	}

	// Insert all new sources
	for (SourcePtr new_src : new_srcs) {
		// Insert it while preserving the order
		auto it = boost::lower_bound(sources, new_src, source_ptr_less());
		sources.insert(it, new_src);
	}

	// Log the new sources
	if (ure_logger().is_debug_enabled()) {
		LAZY_URE_LOG_DEBUG << msgprfx
		                   << products.size() << " results, including "
		                   << new_srcs.size() << " new sources";
		HandleSeq new_src_bodies;
		for (const SourcePtr& new_src : new_srcs)
			new_src_bodies.push_back(new_src->body);
		LAZY_URE_LOG_DEBUG << msgprfx << "New sources:"
		                    << std::endl << new_src_bodies;
	}
}

size_t SourceSet::size() const
{
	std::lock_guard<std::mutex> lock(_mutex);
	return sources.size();
}

bool SourceSet::empty() const
{
	std::lock_guard<std::mutex> lock(_mutex);
	return sources.empty();
}

std::string SourceSet::to_string(const std::string& indent) const
{
	std::lock_guard<std::mutex> lock(_mutex);
	return oc_to_string(sources, indent);
}

std::string oc_to_string(const Source& src, const std::string& indent)
{
	return src.to_string(indent);
}

std::string oc_to_string(const SourceSet::Sources& sources, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << sources.size() << std::endl;
	size_t i = 0;
	for (const SourcePtr& src : sources) {
		ss << indent << "source[" << i << "]:" << std::endl
		   << src->to_string(indent + oc_to_string_indent);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const SourceSet& sources, const std::string& indent)
{
	return sources.to_string(indent);
}

} // ~namespace opencog
