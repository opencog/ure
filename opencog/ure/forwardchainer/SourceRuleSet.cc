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

SourceRule::SourceRule(SourcePtr src, RulePtr rl)
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

std::string SourceRule::to_string(const std::string& indent) const
{
	static const std::string nullstr("nullptr");
	std::stringstream ss;
	// Only print hash because not the master container
	ss << indent << "source[" << source << "]: "
	   << (source ? source->body->id_to_string() : nullstr)
	   << std::endl << indent << "rule[" << rule << "]: "
	   << (rule ? rule->to_short_string() : nullstr);
	return ss.str();
}

SourceRuleSet::SourceRuleSet()
	: _thompson_smp(tv_seq)
{
}

bool SourceRuleSet::insert(const SourceRule& sr, TruthValuePtr tv)
{
	auto it = boost::lower_bound(source_rule_seq, sr);
	if (it == source_rule_seq.end() or *it != sr) {
		it = source_rule_seq.insert(it, sr);
		size_t idx = std::distance(source_rule_seq.begin(), it);
		tv_seq.insert(std::next(tv_seq.begin(), idx), tv);
		return true;
	}
	// The pair is already in the source rule set
	return false;
}

std::pair<SourceRule, TruthValuePtr> SourceRuleSet::thompson_select()
{
	if (tv_seq.empty())
		return {SourceRule(), nullptr};

	// Select the next source rule pair to apply
	size_t rnd_idx = _thompson_smp();
	SourceRule slc_sr = source_rule_seq[rnd_idx];
	TruthValuePtr slc_tv = tv_seq[rnd_idx];

	// Remove it from the container to not be selected again
	source_rule_seq.erase(std::next(source_rule_seq.begin(), rnd_idx));
	tv_seq.erase(std::next(tv_seq.begin(), rnd_idx));

	// Return the selected source rule pair
	return {slc_sr, slc_tv};
}

bool SourceRuleSet::empty() const
{
	return source_rule_seq.empty();
}

size_t SourceRuleSet::size() const
{
	return source_rule_seq.size();
}

std::string SourceRuleSet::to_string(const std::string& indent) const
{
	std::stringstream ss;
	std::string indent2 = indent + oc_to_string_indent;
	ss << indent << "size = " << source_rule_seq.size();
	size_t i = 0;
	for (const SourceRule& sr : source_rule_seq) {
		ss << std::endl << indent << "(source,rule)[" << i << "]:"
		   << std::endl << sr.to_string(indent2);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const SourceRule& sr, const std::string& indent)
{
	return sr.to_string(indent);
}

std::string oc_to_string(const SourceRuleSet& srs, const std::string& indent)
{
	return srs.to_string(indent);
}

} // ~namespace opencog
