/*
 * SourceRuleSet.cc
 *
 * Copyright (C) 2020 SingularityNET Foundation
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

#include "SourceRuleSet.h"

#include <boost/range/algorithm/lower_bound.hpp>
#include <boost/algorithm/cxx11/all_of.hpp>

#include <opencog/util/oc_assert.h>

namespace opencog {

SourceRule::SourceRule(Source* src, Rule* rl)
	: source(src), rule(rl)
{
}

SourceRule::~SourceRule()
{
}

bool SourceRule::operator==(const SourceRule& other) const
{
	return &source == &other.source and &rule == &other.rule;
}

bool SourceRule::operator<(const SourceRule& other) const
{
	return (&source < &other.source)
		or (&source == &other.source and &rule < &other.rule);
}

bool SourceRule::is_valid() const
{
	return source != nullptr and rule != nullptr;
}

SourceRuleSet::SourceRuleSet()
	: _thompson_smp(tvs)
{
}

bool SourceRuleSet::insert(const SourceRule& sr, TruthValuePtr tv)
{
	auto it = boost::lower_bound(source_rule_seq, sr);
	if (it == source_rule_seq.end() or *it != sr) {
		it = source_rule_seq.insert(it, sr);
		size_t idx = std::distance(source_rule_seq.begin(), it);
		tvs.insert(std::next(tvs.begin(), idx), tv);
		return true;
	}
	// The pair is already in the source rule set
	return false;
}

SourceRule SourceRuleSet::thompson_select()
{
	if (tvs.empty())
		return SourceRule();

	// Select the next source rule pair to apply
	size_t rnd_idx = _thompson_smp();
	SourceRule slc_sr = source_rule_seq[rnd_idx];

	// Remove it from the container to not be selected again
	source_rule_seq.erase(std::next(source_rule_seq.begin(), rnd_idx));
	tvs.erase(std::next(tvs.begin(), rnd_idx));

	// Return the selected source rule pair
	return slc_sr;
}

std::string oc_to_string(const SourceRule& sr, const std::string& indent)
{
	static const std::string nullstr("nullptr");
	std::stringstream ss;
	// Only print hash because not the master container
	ss << indent << "source: "
	   << (sr.source ? sr.source->body->id_to_string() : nullstr)
	   << std::endl << indent << "rule: "
	   << (sr.rule ? sr.rule->to_short_string() : nullstr);
	return ss.str();
}

std::string oc_to_string(const SourceRuleSet& srs, const std::string& indent)
{
	std::stringstream ss;
	std::string indent2 = indent + oc_to_string_indent;
	ss << indent << "size = " << srs.source_rule_seq.size();
	size_t i = 0;
	for (const SourceRule& sr : srs.source_rule_seq) {
		ss << std::endl << indent << "source,rule[" << i << "]:"
		   << std::endl << oc_to_string(sr, indent2);
		i++;
	}
	return ss.str();
}

} // ~namespace opencog
