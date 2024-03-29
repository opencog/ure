/*
 * BIT.h
 *
 * Copyright (C) 2016-2017 OpenCog Foundation
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

#ifndef _OPENCOG_BIT_H
#define _OPENCOG_BIT_H

#include <boost/operators.hpp>

#include <opencog/util/empty_string.h>
#include <opencog/ure/Rule.h>
#include <opencog/ure/Utils.h>
#include <opencog/atoms/base/Handle.h>
#include "Fitness.h"

class BITUTest;

namespace opencog
{

/**
 * A BIT (Back Inference Tree) node, and how it relates to its
 * children. A back-inference tree is an and-or-tree, where there are
 * 2 types of children, or-children and and-children. The or-children
 * are represented by BITNode::rules, because multiple rules or rule
 * variations can infer the same target. Then within each rule or rule
 * variation, the rule premises are and-children because in order to
 * apply a certain rule all premises must be fulfilled.
 */
class BITNode
{
public:
	BITNode(const Handle& body=Handle::UNDEFINED,
	        const BITNodeFitness& fitness=BITNodeFitness());

	// BITNode handle (TODO: maybe this is not necessary)
	Handle body;

	// BITNode fitness
	BITNodeFitness fitness;

	// Or-children at the rule level, as multiple rules, or rule
	// variations (partially unified, etc) can yield the same target.
	RuleTypedSubstitutionMap rules;

	// The complexity of the BITNode. For now -log(probability).
	double complexity;

	// True iff all valid rules have already expanded this BIT-node.
	bool exhausted;

	// Estimate the probability of usefulness of expanding this
	// BIT-Node.
	// TODO: Maybe this should be moved to BackwardChainer
	double operator()() const;

	std::string to_string(const std::string& indent="") const;
};

/**
 * And-BIT
 */
class AndBIT : public boost::totally_ordered<AndBIT>
{
	friend class ::BITUTest;

public:
	// FCS associated to the and-BIT
	Handle fcs;

	// Mapping from the FCS leaves to BITNodes
	typedef std::unordered_map<Handle, BITNode> HandleBITNodeMap;
	HandleBITNodeMap leaf2bitnode;

	// The complexity of an and-BIT is the sum of the complexities of
	// the steps involved in producing it. More specifically the steps
	// of choosing the BIT-leaf to expand from and the rule to expand
	// with.
	double complexity;

	// True iff all leaves are exhausted (see BITNode::exhausted)
	bool exhausted;

	// Queried atomspace
	const AtomSpace* queried_as;

	/**
	 * @brief Initialize an and-BIT with a certain target, vardecl and
	 * fitness and add it in bit_as. If an extra atomspace queried_as
	 * is provided, then subsequent and-BITs produced from it will
	 * have their constants removed if present in the queried
	 * atomspace.
	 */
	AndBIT();
	AndBIT(AtomSpace& bit_as, const Handle& target, Handle vardecl,
	       const BITNodeFitness& fitness=BITNodeFitness(),
	       const AtomSpace* queried_as=nullptr);
	/**
	 * @brief construct a and-BIT given its FCS and complexity.
	 */
	AndBIT(const Handle& fcs, double complexity=0.0,
	       const AtomSpace* queried_as=nullptr);
	~AndBIT();

	/**
	 * @brief Expand the and-BIT given a target leaf and rule.
	 *
	 * @param leaf from which to expand the and-BIT.
	 * @param rule with which to expand the and-BIT.
	 * @param prob the probability with which this rule was expanded,
	 *
	 * @return A new and-BIT resulting from the expansion.
	 *
	 * @todo support fitness function.
	 */
	AndBIT expand(const Handle& leaf,
	              const RuleTypedSubstitutionPair& rule,
	              double prob=1.0) const;

	/**
	 * @brief Randomly select a leaf of the FCS. Leaves with lower
	 * BIT-node fitness have more chance of being selected (cause they
	 * need to get fitter).
	 *
	 * @return The selected leaf.
	 */
	BITNode* select_leaf();

	/**
	 * Set the and-BIT exhausted flags to false. Take care of the
	 * BIT-nodes exhausted flags as well.
	 */
	void reset_exhausted();

	/**
	 * Detect whether the expanded fcs rewrite term contains cycles. A
	 * cycle is defined as having two nodes (intermediary or not
	 * conclusions or premises) on the same branch path.
	 *
	 * For instance
	 *
	 * 	             [10587135790209818836][1] [14389148767193402296][1]
	 *               ---conditional-full-instantiation-scope-formula----
	 * [13731348359613425443][1] [10211050625380104512][1]
	 * ======fuzzy-conjunction-introduction-formula=======
	 *             [12968852461573975386][1] [14389148767193402296][1]
	 *             ---conditional-full-instantiation-scope-formula----
	 *                          [10211050625380104512][1]
	 *
	 * has cycles because the conclusion [10911677580466648304][1] is
	 * present in the same branch path, so is [14389148767193402296][1].
	 */
	bool has_cycle() const;
	bool has_cycle(const Handle& h, HandleSet ancestors = {}) const;

	/**
	 * Comparison operators. For operator< compare fcs by complexity, or by
	 * handle value if they are of the same size.
	 */
	bool operator==(const AndBIT& andbit) const;
	bool operator<(const AndBIT& andbit) const;

	std::string to_string(const std::string& indent="") const;

	/**
	 * Print the inference tree of a given FCS in ascii art.
	 *
	 * Basically it ignores the pattern part of the FCS and only
	 * consider how the premises relate to the conclusions in the
	 * rewrite term.
	 *
	 * For instance, if the following FCS has the rewrite term
	 *
	 * ExecutionOutputLink
	 *   <rule-1>
	 *   ListLink
	 *     <conclusion-1>
	 *     <premise-1->
	 *     <premise-2->
	 *     ExecutionOutputLink
	 *       <rule-2>
	 *       ListLink
	 *         <conclusion-2>
	 *         <premise-3>
	 *         ExecutionOutputLink
	 *           <rule-3>
	 *           <conclusion-3>
	 *
	 * Then this function is gonna produce
	 *
	 *                                    --------------<rule-3>
	 *                       <premise-3>  <conclusion-3>
	 *                       ---------------------------<rule-2>
	 *   <premise-1>  <premise-2>  <conclusion-2>
	 *   ----------------------------------------<rule-1>
	 *                <conclusion-1>
	 *
	 * Where <conclusion-1>, <premises-1>, etc, are replaced by Handle
	 * values. Also, note that <conclusion-2> for instance is a
	 * conclusion from <rule-2> viewpoint, but a premise from <rule-1>
	 * viewpoint.
	 *
	 * When the rule takes unordered premises, using a SetLink, then
	 * the straight line is replaced by a doubled line. For instance,
	 * to take a similar example, where <rule-2> happens have
	 * unordered premises, the following FCS rewrite term
	 *
	 * ExecutionOutputLink
	 *   <formula-1>
	 *   ListLink
	 *     <conclusion-1>
	 *     <premise-1->
	 *     <premise-2->
	 *     ExecutionOutputLink
	 *       <formula-2>
	 *       ListLink
	 *         <conclusion-2>
	 *         SetLink
	 *           <premise-3>
	 *           ExecutionOutputLink
	 *             <formula-3>
	 *             <conclusion-3>
	 *
	 * Will be displayed as followed
	 *
	 *                                    --<formula-3>-
	 *                       <premise-3>  <conclusion-3>
	 *                       ========<formula-2>========
	 *   <premise-1>  <premise-2>  <conclusion-2>
	 *   --------------<formula-1>---------------
	 *                <conclusion-1>
	 */
	std::string fcs_to_ascii_art(const Handle& fcs) const;
	std::string fcs_rewrite_to_ascii_art(const Handle& h) const;

private:
	// Weighted distribution over the targets leaves, defined
	// according to their BIT-node fitnesses. The higher the fitness
	// the lower the chance of being selected as it is already fit.
	typedef std::discrete_distribution<size_t> LeafDistribution;

	/**
	 * Calculate the complexity of the and-BIT resulting from expanding
	 * this and-BIT from leaf with a rule with a given probability
	 * estimate of success.
	 */
	double expand_complexity(const Handle& leaf, double prob) const;

	/**
	 * Given an FCS, a leaf of it to expand, and a rule, return a new
	 * FCS where the leaf has been substituted by the rule premises
	 * and rule application.
	 *
	 * A forward chaining strategy is represented according to
	 * https://github.com/opencog/atomspace/issues/903. TODO:
	 * copy/paste the doc here and in the wiki as well.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs(const Handle& leaf,
	                  const RuleTypedSubstitutionPair& rule) const;

	/**
	 * @brief Given that FCS is defined generate the mapping from FCS
	 * leaves to bitnotes.
	 */
	void set_leaf2bitnode();

	/**
	 * @brief Build the BITNode associated to leaf, insert it in
	 * leaf2bitnode and return its iterator. If already in then return
	 * the iterator or the existing BITNode.
	 */
	HandleBITNodeMap::iterator
	insert_bitnode(Handle leaf, const BITNodeFitness& fitness);

	/**
	 * Return all the leaves (or blanket because these new target
	 * leaves cover the previous intermediary targets), of an
	 * FCS.
	 */
	HandleSet get_leaves() const;
	HandleSet get_leaves(const Handle& h) const;

	/**
	 * Given a FCS, a leaf of it and a rule and its associated typed
	 * substitution. Substitution the FCS according to the typed
	 * substitution.
	 */
	Handle substitute_unified_variables(const Handle& leaf,
	                                    const Unify::TypedSubstitution& ts) const;

	/**
	 * Given the pattern term of an FCS where all variables have been
	 * substituted by the corresponding terms in the rule conclusion,
	 * expand the rule conclusion by its premises.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs_pattern(const Handle& fcs_pattern, const Rule& rule) const;

	/**
	 * Given the rewrite term of an FCS where all variables have been
	 * substituted by the corresponding terms in the rule conclusion,
	 * replace the rule conclusion by the rule rewrite term.
	 *
	 * TODO: give examples.
	 */
	Handle expand_fcs_rewrite(const Handle& fcs_rewrite,
	                          const Rule& rule) const;

	/**
	 * Return true if atom is an argument of an evaluation
	 */
	bool is_argument_of(const Handle& eval, const Handle& atom) const;

	/**
	 * Equal even if one of them is locally quoted
	 */
	bool is_locally_quoted_eq(const Handle& lhs, const Handle& rhs) const;

	/**
	 * Create a pattern body given a sequence of present and virtual
	 * clauses, such that
	 *
	 * 1. Wrap in an AndLink if clauses contains multiple virtual and
	 *    non-virtual clauses.
	 *
	 * 2. Wrap in PresentLink if clauses contains multiple non-virtual
	 *    clauses.
	 *
	 * 3. Group under the same PresentLink multiple non-virtual
	 *    clauses.
	 *
	 * 4. Remove redundant clauses.
	 *
	 * Clauses are passed by copy because they are modified by the
	 * method.
	 */
	Handle mk_pattern(HandleSeq prs_clauses, HandleSeq virt_clauses) const;

	/**
	 * Remove redudant atoms in an unordered sequence atoms. As a side
	 * effect it may re-order it.
	 */
	static void remove_redundant(HandleSeq& hs);

	static HandleSeq get_present_clauses(const Handle& pattern);
	static HandleSeq get_present_clauses(const HandleSeq& clauses);
	static HandleSeq get_virtual_clauses(const Handle& pattern);
	static HandleSeq get_virtual_clauses(const HandleSeq& clauses);

	/**
	 * Merge horizontally 2 ascii art strings, avoiding collision. The
	 * collision distance is specified in dst, in number of horizontal
	 * characters.
	 */
	static std::vector<std::string>
	ascii_art_hmerge(const std::vector<std::string>& laa /* botton to top */,
	                 const std::vector<std::string>& raa /* botton to top */,
	                 unsigned dst);
	static std::string ascii_art_hmerge(const std::vector<std::string>& aas,
	                                    unsigned dst=1);
	static std::string ascii_art_hmerge(const std::string& laa,
	                                    const std::string& raa,
	                                    unsigned dst);

	/**
	 * Turn a multiline string into a list of string where the last
	 * line is the first of the line, and the first line is the last
	 */
	static std::vector<std::string> reverse_split(const std::string& aa);

	/**
	 * Return the bottom line of an ascii art string.
	 */
	static std::string bottom_line(const std::string& aa);

	/**
	 * Return the number of leading spaces.
	 */
	static unsigned leading_spaces(const std::string& line);

	/**
	 * Abbreviate the formula string to fit inside a line separator (to
	 * reach a given target size, tg_size).
	 *
	 * It assumes the string only uses lower case words dash separated,
	 * such as
	 *
	 * implication-conditional-conjunction-introduction
	 *
	 * It will attempt not to remove entire words.
	 *
	 * This function is inspired by Paul Davis's short_version function
	 * of the Ardour PBD library.
	 */
	static std::string remove_vowels(std::string str, size_t tg_size);
	static std::string remove_consonants(std::string str, size_t tg_size);
	static std::string abbreviate(std::string str, size_t tg_size);

	/**
	 * Given an ascii art, produce a string that seperates the upper
	 * and lower ascii arts.
	 */
	static std::string line_separator(const std::string& up_aa,
	                                  const std::string& low_aa,
	                                  const Handle& gsn,
	                                  bool unordered_premises=false);
};

/**
 * Back Inference Tree. A graph of BIT-Nodes and a collection of
 * and-BITs (as Forward Chaining Strategies, FCS for short), with
 * methods to build and use it.
 */
class BIT
{
public:
	// Child atomspace of the queried atomspace for storing the BIT
	AtomSpace bit_as;

	// Collection of and-BITs. We use a sorted vector instead of a set
	// because the andbit being expanded is modified (its expanded
	// bit-Node keeps track of the expansion). Alternatively we could
	// use a set and define AndBIT::leaf2bitnode as mutable.
	typedef std::vector<AndBIT> AndBITs;
	AndBITs andbits;

	/**
	 * Ctor/Dtor
	 */
	BIT(); // Dummy BIT, for testing
	BIT(AtomSpace& as, const Handle& target, const Handle& vardecl,
	    const BITNodeFitness& fitness=BITNodeFitness());
	~BIT();

	/**
	 * @brief return true iff the BIT is empty (i.e. has no and-BITs).
	 */
	bool empty() const;

	/**
	 * @brief return the number of and-BITs in the BIT.
	 */
	size_t size() const;

	/**
	 * @brief Initialize the BIT and return the initial and-BIT.
	 */
	AndBIT* init();

	/**
	 * Expand the andbit, add it to the BIT and return its pointer. If
	 * the expansion has failed return nullptr.
	 *
	 * andbit and bitleaf are not passed by const because bitleaf
	 * keeps a record of that expansion and is this modified during
	 * that step.
	 */
	AndBIT* expand(AndBIT& andbit, BITNode& bitleaf,
	               const RuleTypedSubstitutionPair& rule,
	               double prob=1.0);

	/**
	 * Insert a new andbit in the BIT and return its pointer, nullptr
	 * if not inserted (which may happen if an equivalent one is
	 * already in it). Warning: the reference on andbit will likely be
	 * invalid after this call because the container is modified.
	 */
	AndBIT* insert(AndBIT& andbit);

	/**
	 * Erase the given and-BIT from the BIT and remove its FCS from
	 * bit_as.
	 */
	template<typename It> AndBITs::iterator erase(It pos);

	/**
	 * Reset to false all and-BITs exhausted flags.
	 */
	void reset_exhausted_flags();

	/**
	 * Return true if all andbits are exhausted.
	 */
	bool andbits_exhausted() const;

	/**
	 * Return true if the rule is already an or-children of bitnode up
	 * to an alpha conversion.
	 */
	bool contains(const BITNode& bitnode,
	              const RuleTypedSubstitutionPair& rule) const;

private:
	// Queried atomspace
	AtomSpace* _as;

	Handle _init_target;
	Handle _init_vardecl;
	BITNodeFitness _init_fitness;
};

template<typename It>
BIT::AndBITs::iterator BIT::erase(It pos)
{
	remove_hypergraph(bit_as, pos->fcs);
	return andbits.erase(pos);
}

// Gdb debugging, see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
std::string oc_to_string(const BITNode& bitnode,
                         const std::string& indent=empty_string);
std::string oc_to_string(const AndBIT& andbit,
                         const std::string& indent=empty_string);

} // ~namespace opencog

#endif // _OPENCOG_BIT_H
