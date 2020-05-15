/*
 * SourceSet.h
 *
 * Copyright (C) 2018 SingularityNET Foundation
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

#ifndef _OPENCOG_SOURCESET_H_
#define _OPENCOG_SOURCESET_H_

#include <vector>
#include <mutex>

#include <boost/operators.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>

#include "../Rule.h"
#include "../UREConfig.h"

namespace opencog
{

/**
 * Each source is associated to
 *
 * 1. The source body and its variable declaration
 *
 * 2. a complexity (reflecting the probability that expanding it will
 *    fulfill the objective),
 *
 * 3. the set of rules that have expanded it so far,
 *
 * 4. a flag call indicating if the source expansions have been exhausted.
 */
// TODO: this class has thing in common with AndBIT, maybe their
// common things could be placed in a parent class.
class Source : public boost::totally_ordered<Source>
{
public:
	Source(const Handle& body,
	       const Handle& vardecl=Handle::UNDEFINED,
	       double complexity=0.0,
	       double complexity_factor=1.0);

	/**
	 * Comparison operators. Only body and vardecl are used for
	 * comparison, not the weight or complexity, because such
	 * quantities depend on the inference path leading to the source,
	 * thus would fail to capture confluence.
	 */
	bool operator==(const Source& other) const;
	bool operator<(const Source& other) const;

	/**
	 * Insert rule in the rule set to remember it is being
	 * applied. Return true if insertion is successful (that is if no
	 * alpha-equivalent rule was already there).
	 */
	bool insert_rule(RulePtr rule);

	/**
	 * Set exhausted flag to true
	 */
	void set_exhausted();

	/**
	 * Set exhausted flag back to false, and erase tried rules
	 */
	void reset_exhausted();

	/**
	 * Get exhausted flag
	 */
	bool is_exhausted() const;

	/**
	 * Set the exhausted flag of that rule to true
	 */
	void set_rule_exhausted(const RulePtr& rule);

	/**
	 * Check if the given rule has been tried
	 */
	bool is_rule_exhausted(const RulePtr& rule) const;

	/**
	 * Return the complexity of new source expanded from this source by
	 * a rule with probability of success prob.
	 */
	double expand_complexity(double prob) const;

	/**
	 * Return the weight (probability estimate up to a normalizing
	 * factor) of expanding src.
	 */
	double get_weight() const;

	std::string to_string(const std::string& indent=empty_string) const;

	// Body of the source
	const Handle body;

	// Variable declaration, if any, associated to body
	const Handle vardecl;

	// Sum of the complexities of the steps involved in producing it.
	const double complexity;

	// Proxy for the prior probability of the source, taking into
	// account the complexity penalty.
	//
	// Note that in case the complexity penalty is negative (which can
	// be used for depth-first search) then the complexity factor can
	// be greater than 1.0.
	const double complexity_factor;

	// Weight, akin to the unormalized probability of selecting that
	// source.
	//
	// Note that in case the complexity penalty is negative (which can
	// be used for depth-first search) then the weight can be greater
	// than 1.0.
	const double weight;

	// True iff all rules that could expand the source have been tried
	bool exhausted;

	// Rules so far attempted on that source. Primary owner.
	RuleSet rules;

private:
	// TODO: subdivide in smaller and shared mutexes
	mutable std::mutex _mutex;
};

typedef std::shared_ptr<Source> SourcePtr;
#define createSource std::make_shared<Source>
struct source_ptr_less
{
	bool operator()(const SourcePtr& l, const SourcePtr& r) const;
};

/**
 * Population of sources to forwardly expand. Primary owner.
 */
// TODO: this class has things in common with BIT, maybe their common
// things could be placed in a parent class.
class SourceSet
{
public:
	SourceSet(const UREConfig& config,
	          const Handle& init_source,
	          const Handle& init_vardecl);

	/**
	 * Return a sequence of weights (probability estimate up to a
	 * normalizing factor) of picking the corresponding source.
	 */
	std::vector<double> get_weights() const;

	/**
	 * Set exhausted flag to true
	 */
	void set_exhausted();

	/**
	 * When new inference rules come in or we get to retry exhausted
	 * sources, then reset exhausted flags.
	 */
	void reset_exhausted();

	/**
	 * Get exhausted flag
	 */
	bool is_exhausted() const;

	/**
	 * Insert produced sources from src into the population, by
	 * applying rule with a given probability of success prob (useful
	 * for calculating complexity).
	 */
	void insert(const HandleSet& products, const Source& src,
	            double prob, const std::string& msgprfx="");

	size_t size() const;

	bool empty() const;

	std::string to_string(const std::string& indent=empty_string) const;

	// Collection of sources. We use a sorted vector instead of a set
	// because the source being expanded is modified (it keeps track of
	// its expansion rules). Alternatively we could use a set and
	// define Sources::rules as mutable.
	typedef std::vector<SourcePtr> Sources;
	Sources sources;

	// True iff all sources have been tried
	bool exhausted;

private:
	const UREConfig& _config;

	// TODO: subdivide in smaller and shared mutexes
	mutable std::mutex _mutex;
};

std::string oc_to_string(const Source& source,
                         const std::string& indent=empty_string);
std::string oc_to_string(const SourceSet::Sources& sources,
                         const std::string& indent=empty_string);
std::string oc_to_string(const SourceSet& sources,
                         const std::string& indent=empty_string);

} // ~namespace opencog

#endif /* _OPENCOG_SOURCESET_H_ */
