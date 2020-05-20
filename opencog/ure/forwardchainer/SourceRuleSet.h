/*
 * SourceSet.h
 *
 * Copyright (C) 2020 SingularityNET Foundation
 *
 * Author: Nil Geisweiller <ngeiswei@gmail.com>
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

#ifndef _OPENCOG_SOURCERULESET_H_
#define _OPENCOG_SOURCERULESET_H_

#include <opencog/util/empty_string.h>

#include "../ThompsonSampling.h"

#include "SourceSet.h"

namespace opencog
{

/**
 * Pair of source and rule, actually pointers as not primary owner.
 */
class SourceRule : public boost::totally_ordered<SourceRule>
{
public:
	SourceRule(SourcePtr src=nullptr, RulePtr rule=nullptr);
	~SourceRule();

	/**
	 * Comparison operators. Based on src and rule, not tv, as indeed
	 * such tv depends on the inference path leading to that (source,
	 * rule) pair, thus would fail to capture confluence.
	 */
	bool operator==(const SourceRule& other) const;
	bool operator<(const SourceRule& other) const;

	/**
	 * Return true iff the pair is valid, that is both source and rule
	 * pointers are non null.
	 */
	bool is_valid() const;

	/**
	 * Turn the pair into a string representation. Useful for
	 * debugging.
	 */
	std::string to_string(const std::string& indent=empty_string) const;

	// Pointers of source and rule from other master
	// containers. Although these pointers are never null, we use
	// pointers instead of references to avoid re-implementing
	// SourceRule::operator=().
	SourcePtr source;
	RulePtr rule;
};

/**
 * Class holding (source, rule) reference pairs to be selected and
 * applied. Each pair is weighted by a second order probability of
 * success, thus the selector must turn these second order
 * probabilities of success into first order probabilities of action
 * via Thompson sampling ideally, or less ideally but possibly more
 * efficiently tournament selection.
 *
 * This container is also called the Expansion Pool.
 */
class SourceRuleSet
{
public:
	SourceRuleSet();

	/**
	 * Insert (source, rule) pair in the container, alongside it's
	 * second order probability of success. Return false if insertion
	 * fails, that is such pair already exists in the container.
	 */
	bool insert(const SourceRule& sr, TruthValuePtr tv);

	/**
	 * Select a pair according to Thompson sampling and remove it from
	 * the set. Return the empty source rule pair, and the nullptr
	 * truth value if the set is empty.
	 */
	std::pair<SourceRule, TruthValuePtr> thompson_select();

	// TODO: implement tournament selection as well, as a cheaper
	// alternative to Thompson sampling.

	/**
	 * Return true iff the pool is empty
	 */
	bool empty() const;

	/**
	 * Return the number of source rule pairs in the container
	 */
	size_t size() const;

	/**
	 * Turn the source rule pool into a string representation. Useful
	 * for debugging.
	 */
	std::string to_string(const std::string& indent=empty_string) const;

	// Ordered sequence of source rule pairs.
	std::vector<SourceRule> source_rule_seq;

	// Ordered sequence of Truth Values, representing the second order
	// weights of each source rule pair. In the same order as
	// source_rule_seq.
	//
	// It's easier here to have a sequence of TVs as opposed to having
	// having weighted pairs because Thompson sampling takes in a
	// sequence of TVs.
	TruthValueSeq tv_seq;

private:
	ThompsonSampling _thompson_smp;
};

std::string oc_to_string(const SourceRule& sr,
                         const std::string& indent=empty_string);
std::string oc_to_string(const SourceRuleSet& srs,
                         const std::string& indent=empty_string);

} // ~namespace opencog

#endif /* _OPENCOG_SOURCERULESET_H_ */
