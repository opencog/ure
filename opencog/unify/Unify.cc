/**
 * Unify.cc
 *
 * Utilities for unifying atoms.
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
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

#include "Unify.h"

#include <boost/algorithm/cxx11/any_of.hpp>

#include <opencog/util/algorithm.h>
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/Context.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/core/RewriteLink.h>
#include <opencog/atoms/pattern/PatternUtils.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {

const Unify::Partitions Unify::empty_partitions({});

const Unify::Partitions Unify::empty_partition_singleton({{}});

Unify::CHandle::CHandle(const Handle& h, const Context& c)
	: handle(h), context(c) {}

bool Unify::CHandle::is_variable() const
{
	return handle->get_type() == VARIABLE_NODE;
}

bool Unify::CHandle::is_free_variable() const
{
	return context.is_free_variable(handle);
}

HandleSet Unify::CHandle::get_free_variables() const
{
	HandleSet free_vars =
		opencog::get_free_variables(handle, context.quotation);
	return set_difference(free_vars, context.shadow);
}

Context::VariablesStack::const_iterator
Unify::CHandle::find_variables(const Handle& h) const
{
	return std::find_if(context.scope_variables.cbegin(),
	                    context.scope_variables.cend(),
	                    [&](const Variables& variables) {
		                    return variables.varset_contains(h);
	                    });
}

bool Unify::CHandle::is_consumable() const
{
	return context.quotation.consumable(handle->get_type());
}

bool Unify::CHandle::is_quoted() const
{
	return context.quotation.is_quoted();
}

bool Unify::CHandle::is_unquoted() const
{
	return context.quotation.is_unquoted();
}

void Unify::CHandle::update()
{
	bool isc = is_consumable();
	context.update(handle);
	if (isc)
		handle = handle->getOutgoingAtom(0);
}

bool Unify::CHandle::is_node_satisfiable(const CHandle& other) const
{
	// If both are variable check whether they could be alpha
	// equivalent, otherwise merely check for equality
	if (is_variable() and other.is_variable())	{
		// Make sure scope variable declarations are stored
		OC_ASSERT(context.store_scope_variables,
		          "You must store the scope variable declarations "
		          "in order to use this method");

		// Search variable declarations associated to the variables
		Context::VariablesStack::const_iterator it = find_variables(handle),
			other_it = other.find_variables(other.handle);
		OC_ASSERT(it != context.scope_variables.cend(),
		          "Contradicts the assumption that this->handle is not free");
		OC_ASSERT(other_it != other.context.scope_variables.cend(),
		          "Contradicts the assumption that other.handle is not free");

		// Check that both variable declarations occured at the same level
		if (std::distance(context.scope_variables.cbegin(), it)
		    != std::distance(other.context.scope_variables.cbegin(), other_it))
			return false;

		// Check that the other variable is alpha convertible
		return it->is_alpha_convertible(handle, other.handle, *other_it, true);
	} else {
		return content_eq(handle, other.handle);
	}
}

bool Unify::CHandle::operator==(const CHandle& ch) const
{
	return content_eq(handle, ch.handle) and (context == ch.context);
}

bool Unify::CHandle::operator<(const CHandle& ch) const
{
	return (handle < ch.handle) or
		(handle == ch.handle and context < ch.context);
}

Unify::CHandle::operator bool() const
{
	return (bool)handle;
}

Unify::SolutionSet::SolutionSet(bool s)
	: Partitions(s ? empty_partition_singleton : empty_partitions) {}

Unify::SolutionSet::SolutionSet(const Unify::Partitions& p)
	: Partitions(p) {}

bool Unify::SolutionSet::is_satisfiable() const
{
	return not empty();
}

void Unify::SolutionSet::insert(const SolutionSet& sol)
{
	Partitions::insert(sol.begin(), sol.end());
}

void Unify::SolutionSet::remove_cycles()
{
	// TODO: replace by std::set::erase_if once C++20 is enabled
	for (auto it = begin(); it != end();) {
		if (has_cycle(*it)) {
			it = erase(it);
		} else {
			++it;
		}
	}
}

Unify::Unify(const Handle& lhs, const Handle& rhs,
             const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	// Set terms to unify
	_lhs = lhs;
	_rhs = rhs;

	// Set _variables
	set_variables(lhs, rhs, lhs_vardecl, rhs_vardecl);
}

Unify::Unify(const Handle& lhs, const Handle& rhs,
             const Variables& lhs_vars, const Variables& rhs_vars)
{
	// Set terms to unify
	_lhs = lhs;
	_rhs = rhs;

	// Set _variables
	_variables = merge_variables(lhs_vars, rhs_vars);
}

Unify::TypedSubstitutions Unify::typed_substitutions(const SolutionSet& sol,
                                                     const Handle& pre) const
{
	OC_ASSERT(sol.is_satisfiable());

	TypedSubstitutions result;
	for (const Partition& partition : sol)
		result.insert(typed_substitution(partition, pre));
	return result;
}

Unify::TypedSubstitution Unify::typed_substitution(const Partition& partition,
                                                   const Handle& pre) const
{
	// Associate the least abstract element to each variable of each
	// block.
	HandleCHandleMap var2cval;
	for (const TypedBlock& block : partition) {
		CHandle least_abstract = find_least_abstract(block, pre);

		// Build variable mapping
		for (const CHandle& ch : block.first)
			if (ch.is_free_variable())
				var2cval.insert({ch.handle, least_abstract});
	}

	// Calculate its closure
	var2cval = substitution_closure(var2cval);

	// Remove ill quotations
	Variables tmpv(_variables.get_vardecl());
	for (auto& vcv : var2cval) {
		bool needless_quotation = true;
		Handle consumed =
			RewriteLink::consume_quotations(tmpv, vcv.second.handle,
			                                vcv.second.context.quotation,
			                                needless_quotation, false);
		vcv.second = CHandle(consumed, vcv.second.context);
	}

	// Calculate its variable declaration
	Handle vardecl = substitution_vardecl(var2cval);

	// Return the typed substitution
	return {var2cval, vardecl};
}

static Variables gen_univars(const Handle& h, const Handle& vardecl)
{
	if (vardecl)
		return Variables(vardecl);
	HandleSet vars = get_free_variables(h);
	HandleSeq varli(vars.begin(), vars.end());
	return Variables(varli);
}

void Unify::set_variables(const Handle& lhs, const Handle& rhs,
                          const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	// Merge the 2 type declarations
	Variables lv = gen_univars(lhs, lhs_vardecl);
	Variables rv = gen_univars(rhs, rhs_vardecl);
	_variables = merge_variables(lv, rv);
}

Unify::CHandle Unify::find_least_abstract(const TypedBlock& block,
                                          const Handle& pre) const
{
	// Get the least abstract element of the block
	auto blk_it = block.first.begin();
	CHandle least_abstract = *blk_it;
	++blk_it;
	for (; blk_it != block.first.end(); ++blk_it)
		if (inherit(*blk_it, least_abstract))
			least_abstract = *blk_it;

	// In case of ties pick up the one in pre (pre stands for
	// precedence)
	for (const CHandle& ch : block.first)
		if (inherit(ch, least_abstract)
		    and
		    (not ch.is_free_variable()
		     or is_unquoted_unscoped_in_tree(pre, ch.handle)))
			least_abstract = ch;

	return least_abstract;
}

Unify::HandleCHandleMap Unify::substitution_closure(const HandleCHandleMap& var2cval) const
{
	// Strip var2cval from its contexts
	HandleMap var2val = strip_context(var2cval);

	// Substitute every value that have variables by other values
	// associated to these variables.
	HandleCHandleMap result(var2cval);
	for (auto& el : result) {
		HandleSet free_vars = el.second.get_free_variables();
		HandleSeq free_list(free_vars.begin(), free_vars.end());
		Variables variables(free_list);
		HandleSeq values = variables.make_sequence(var2val);
		el.second.handle = variables.substitute_nocheck(el.second.handle, values);
	}

	// If we have reached a fixed point then return substitution,
	// otherwise re-iterate
	return hchm_content_eq(result, var2cval) ?
		result : substitution_closure(result);
}

Handle Unify::substitution_vardecl(const HandleCHandleMap& var2val) const
{
	// Build the type declaration for this substitution. For now, the
	// type is merely lhs_vardecl and rhs_vardecl merged together,
	// then all variables assigned for substitution other than
	// themselves are removed. To do well it should be taking into
	// account the possibly more restrictive types found during
	// unification (i.e. the block types).

	Variables ts_variables = _variables;

	for (const auto& el : var2val)
		// Make sure it is not a self substitution
		if (el.first != el.second.handle)
			ts_variables.erase(el.first);

	return ts_variables.get_vardecl();
}

bool Unify::is_pm_connector(const Handle& h)
{
	return is_pm_connector(h->get_type());
}

bool Unify::is_pm_connector(Type t)
{
	return t == AND_LINK or t == OR_LINK or t == NOT_LINK;
}

HandleMultimap Unify::vargraph(const Partition& partition)
{
	HandleMultimap vg;
	for (const auto& blk : partition) {
		HandleMultimap bvg = vargraph(blk.first);
		for (const auto& vvs : bvg) {
			vg[vvs.first].insert(vvs.second.begin(), vvs.second.end());
		}
	}
	return vg;
}

HandleMultimap Unify::vargraph(const Block& blk)
{
	// Standalone variables
	HandleSet stdvars;
	// Variables buried inside terms
	HandleSet trmvars;

	// Fill stdvars and trmvars
	for (const CHandle& ch : blk) {
		if (ch.is_free_variable()) {
			stdvars.insert(ch.handle);
			continue;
		}
		HandleSet fvs = ch.get_free_variables();
		trmvars.insert(fvs.begin(), fvs.end());
	}

	// For each standaline variable associate all terms variables
	HandleMultimap vg;
	for (const Handle& stv : stdvars)
		vg[stv] = trmvars;
	return vg;
}

bool Unify::has_cycle(const Partition& partition)
{
	return has_cycle(vargraph(partition));
}

bool Unify::has_cycle(const Block& blk)
{
	return has_cycle(vargraph(blk));
}

bool Unify::has_cycle(const HandleMultimap& vg)
{
	using boost::algorithm::any_of;
	HandleMultimap cvg = closure(vg);
	return any_of(cvg, [](const HandleMultimap::value_type& vvs) {
			return contains(vvs.second, vvs.first); });
}

HandleMultimap Unify::closure(const HandleMultimap& vg)
{
	return fixpoint(&Unify::closure_step, vg);
}

HandleMultimap Unify::closure_step(const HandleMultimap& vg)
{
	HandleMultimap nvg(vg);
	for (const auto& vvs : vg) {
		for (const Handle& v : vvs.second) {
			const HandleSet& third = nvg[v];
			nvg[vvs.first].insert(third.begin(), third.end());
		}
	}
	return nvg;
}

Handle Unify::substitute(BindLinkPtr bl, const TypedSubstitution& ts,
                         const AtomSpace* queried_as)
{
	// TODO: make sure that ts.second contains the declaration of all
	// variables
	return substitute(bl, strip_context(ts.first), ts.second, queried_as);
}

static Handle make_vardecl(const Handle& h)
{
	HandleSet vars = get_free_variables(h);
	return Handle(createVariableSet(HandleSeq(vars.begin(), vars.end())));
}

Handle Unify::substitute(BindLinkPtr bl, const HandleMap& var2val,
                         Handle vardecl, const AtomSpace* queried_as)
{
	// Perform substitution over the existing variable declaration, if
	// no new alternative is provided.
	if (not vardecl) {
		// If the bind link has no variable declaration either then
		// infer one
		Handle old_vardecl = bl->get_vardecl() ? bl->get_vardecl()
			: make_vardecl(bl->get_body());
		// Substitute the variables in the old vardecl to obtain the
		// new one.
		vardecl = substitute_vardecl(old_vardecl, var2val);
	}

	const Variables& variables = bl->get_variables();

	// Turn the map into a vector of new variable names/values
	HandleSeq values = variables.make_sequence(var2val);

	// Substituted BindLink outgoings
	HandleSeq hs;

	// Perform substitution over the pattern term, then remove
	// constant clauses
	Handle clauses = variables.substitute_nocheck(bl->get_body(), values);
	Variables tmpv(vardecl);
	bool needless_quotation = true;
	clauses = RewriteLink::consume_quotations(tmpv, clauses,
                             Quotation(), needless_quotation, true);
	if (queried_as)
		clauses = remove_constant_clauses(vardecl, clauses, queried_as);
	hs.push_back(clauses);

	// Perform substitution over the rewrite terms
	for (const Handle& himp: bl->get_implicand())
	{
		Handle rewrite = variables.substitute_nocheck(himp, values);
		Variables tmpv(vardecl);
		bool needless_quotation = true;
		rewrite = RewriteLink::consume_quotations(tmpv, rewrite,
		                      Quotation(), needless_quotation, false);
		hs.push_back(rewrite);
	}

	// Filter vardecl
	vardecl = filter_vardecl(vardecl, hs);

	// Insert vardecl in hs if defined
	if (vardecl)
		hs.insert(hs.begin(), vardecl);

	// Create the substituted BindLink
	return createLink(std::move(hs), bl->get_type());
}

Handle Unify::substitute_vardecl(const Handle& vardecl,
                                 const HandleMap& var2val)
{
	if (not vardecl)
		return Handle::UNDEFINED;

	Type t = vardecl->get_type();

	// Base cases

	if (t == VARIABLE_NODE) {
		auto it = var2val.find(vardecl);
		// Only substitute if the variable is substituted by another variable
		if (it != var2val.end() and it->second->get_type() == VARIABLE_NODE)
			return it->second;
		return Handle::UNDEFINED;
	}

	// Recursive cases

	HandleSeq oset;

	if (t == VARIABLE_LIST or t == VARIABLE_SET) {
		for (const Handle& h : vardecl->getOutgoingSet()) {
			Handle nh = substitute_vardecl(h, var2val);
			if (nh)
				oset.push_back(nh);
		}
		if (oset.empty())
			return Handle::UNDEFINED;
	}
	else if (t == TYPED_VARIABLE_LINK) {
		Handle new_var = substitute_vardecl(vardecl->getOutgoingAtom(0),
		                                    var2val);
		if (new_var) {
			oset.push_back(new_var);
			oset.push_back(vardecl->getOutgoingAtom(1));
		} else return Handle::UNDEFINED;
	}
	else {
		OC_ASSERT(false, "Not implemented");
	}
	return createLink(std::move(oset), t);
}

// Return true iff the given handle is not in the given atomspace (if
// any).
static bool not_in_atomspace(const Handle& handle, const AtomSpace* atomspace)
{
	return nullptr != atomspace
	   and nullptr == atomspace->get_atom(handle);
}

// Return true iff the given clause is constant and is not in the
// given atomspace. A set of variables is provided to make the
// distinction between variables and variable interpreted as constants
// (they are constants is not in vars). Checking the present of the
// clause in the atomspace matters because we don't want to trigger a
// rule that will produce a term conditioned on the presence of a
// clause that was actually not in the atomspace (that would have been
// turned into a constant due to partial instantiation of a rule).
static bool not_constant(const HandleSet& vars,
                         const Handle& clause,
                         const AtomSpace* as)
{
	return not_in_atomspace(clause, as) or not is_constant(vars, clause);
}

// TODO: for now it is assumed clauses are connected by an AndLink
// only. To fix that one needs to generalize
// PatternLink::unbundle_clauses to make it usable in that code too.
//
// TODO: maybe replace Handle vardecl by Variables variables.
Handle Unify::remove_constant_clauses(const Handle& vardecl,
                                      const Handle& clauses,
                                      const AtomSpace* as)
{
	VariableListPtr vl = createVariableList(vardecl);
	HandleSet vars = vl->get_variables().varset;

	// Remove constant clauses
	Type t = clauses->get_type();
	HandleSeq hs;
	if (t == AND_LINK) {
		for (const Handle& clause : clauses->getOutgoingSet()) {
			if (not_constant(vars, clause, as)) {
				hs.push_back(clause);
			}
		}
	} else if (not_constant(vars, clauses, as)) {
		return clauses;
	}
	return createLink(std::move(hs), AND_LINK);
}

Unify::SolutionSet Unify::operator()()
{
	// If the declaration is ill typed, there is no solution
	if (not _variables.is_well_typed())
		return SolutionSet();

	// It is well typed, perform the unification
	SolutionSet sol = unify(_lhs, _rhs);

	// Remove partitions with cycles
	sol.remove_cycles();

	return sol;
}

Unify::SolutionSet Unify::unify(const CHandle& lhs, const CHandle& rhs) const
{
	return unify(lhs.handle, rhs.handle, lhs.context, rhs.context);
}

Unify::SolutionSet Unify::unify(const Handle& lh, const Handle& rh,
                                Context lc, Context rc) const
{
	Type lt(lh->get_type());
	Type rt(rh->get_type());

	///////////////////
	// Base cases    //
	///////////////////

	// Make sure both handles are defined
	if (not lh or not rh)
		return SolutionSet();

	CHandle lch(lh, lc);
	CHandle rch(rh, rc);

	bool lq = lc.quotation.consumable(lt);
	bool rq = rc.quotation.consumable(rt);

	// If one is a node
	if (lh->is_node() or rh->is_node()) {
		// If one is a free variable that is declared in _variables, and
		// they are different, then unifies.
		if (is_free_declared_variable(lch) or is_free_declared_variable(rch)) {
			if (lch == rch) {
				// Do not construct a solution like {X}->X to not
				// overload the solution set.
				//
				// Since the context is taken into account they have
				// the same context, thus if one of them is free, the
				// other is free as well, therefore they are
				// satisfiable.
				return SolutionSet(true);
			} else {
				return mkvarsol(lch, rch);
			}
		} else if (!lq and !rq)
			return SolutionSet(is_node_satisfiable(lch, rch));
	}

	////////////////////////
	// Recursive cases    //
	////////////////////////

    // Consume quotations
	if (lq and rq) {
		lc.quotation.update(lt);
		rc.quotation.update(rt);
		return unify(lh->getOutgoingAtom(0), rh->getOutgoingAtom(0), lc, rc);
	}
	if (lq) {
		lc.quotation.update(lt);
		return unify(lh->getOutgoingAtom(0), rh, lc, rc);
	}
	if (rq) {
		rc.quotation.update(rt);
		return unify(lh, rh->getOutgoingAtom(0), lc, rc);
	}

	// Update contexts
	lc.update(lh);
	rc.update(rh);

	// At least one of them is a link, check if they have the same
	// type (e.i. do they match so far)
	if (lt != rt)
		return SolutionSet();

	// At this point they are both links of the same type.
	if (rh->is_unordered_link())
		return unordered_unify(lh->getOutgoingSet(), rh->getOutgoingSet(), lc, rc);
	else
		return ordered_unify(lh->getOutgoingSet(), rh->getOutgoingSet(), lc, rc);
}

Unify::SolutionSet Unify::unordered_unify(const HandleSeq& lhs,
                                          const HandleSeq& rhs,
                                          Context lc, Context rc) const
{
	SolutionSet sol(false);

	HandleSeq perm(rhs);
	do {
		sol.insert(ordered_unify(lhs, perm, lc, rc));
	} while (std::next_permutation(perm.begin(), perm.end()));

	return sol;
}

Unify::SolutionSet Unify::ordered_unify(const HandleSeq& lhs,
                                        const HandleSeq& rhs,
                                        Context lc, Context rc) const
{
	SolutionSet sol(false);

	if (lhs.empty() and rhs.empty()) return SolutionSet(true);

#define is_lh_glob lhs[0]->get_type() == GLOB_NODE and is_declared_variable(lhs[0])
#define is_rh_glob rhs[0]->get_type() == GLOB_NODE and is_declared_variable(rhs[0])

	if (!lhs.empty() and !rhs.empty() and !(is_lh_glob) and !(is_rh_glob)){
		const auto head_sol = unify(lhs[0], rhs[0], lc, rc);
		const auto tail_sol = ordered_unify(tail(lhs), tail(rhs), lc, rc);
		return join(head_sol, tail_sol);
	}

	// If lhs[0] we need to try to unify for every possible number
	// of arguments the glob can contain.
	if (!lhs.empty() and is_lh_glob)
		ordered_unify_glob(lhs, rhs, sol, lc, rc);

	// The flip flag is to prevent redundant partitions.
	// i:e for globs X and U with the same type restriction
	//     {{{X, U}, U}} and {{{X, U}, X}} are equivalent.
	if (!rhs.empty() and is_rh_glob)
		ordered_unify_glob(rhs, lhs, sol, rc, lc, true);

#undef is_lh_glob
#undef is_rh_glob

	return sol;
}

void Unify::ordered_unify_glob(const HandleSeq &lhs,
                               const HandleSeq &rhs,
                               Unify::SolutionSet &sol,
                               Context lc, Context rc, bool flip) const
{
	const auto inter = _variables.get_interval(lhs[0]);
	for (size_t i = inter.first;
	     (i <= inter.second and i <= rhs.size()); i++) {
		// The condition is to avoid extra complexity when calculating
		// type-intersection for glob. Should be fixed from the atomspace
		// Variables::is_type.
		Handle r_h;
		if (i == 1) {
			Type rtype = (*rhs.begin())->get_type();
			if (GLOB_NODE == rtype)
				r_h = *rhs.begin();
			else if (QUOTE_LINK == rtype or UNQUOTE_LINK == rtype)
				r_h = createLink((*rhs.begin())->getOutgoingSet(), LIST_LINK);
			else r_h = createLink(HandleSeq(rhs.begin(), rhs.begin() + i), LIST_LINK);
		}
		else r_h = createLink(HandleSeq(rhs.begin(), rhs.begin() + i), LIST_LINK);

		auto head_sol = flip ?
		                unify(r_h, lhs[0], rc, lc) :
		                unify(lhs[0], r_h, lc, rc);
		auto tail_sol = flip ?
		                ordered_unify(tail(rhs, i), tail(lhs), rc, lc) :
		                ordered_unify(tail(lhs), tail(rhs, i), lc, rc);
		sol.insert(join(tail_sol, head_sol));
	}
}

HandleSeq Unify::tail(const HandleSeq &seq) const
{
	return seq.empty() ? seq : HandleSeq(std::next(seq.begin()), seq.end());
}

HandleSeq Unify::tail(const HandleSeq &seq, const size_t offset) const
{
	return seq.size() < offset ? seq : HandleSeq(seq.begin() + offset, seq.end());
}

Unify::SolutionSet Unify::pairwise_unify(const std::set<CHandlePair>& pchs) const
{
	SolutionSet sol(true);
	for (const CHandlePair& pch : pchs) {
		auto rs = unify(pch.first, pch.second);
		sol = join(sol, rs);
		if (not sol.is_satisfiable())     // Stop if unification has failed
			return sol;
	}
	return sol;
}

Unify::SolutionSet Unify::comb_unify(const std::set<CHandle>& lhs,
                                     const std::set<CHandle>& rhs) const
{
	SolutionSet sol(true);
	for (const CHandle& lch : lhs) {
		for (const CHandle& rch : rhs) {
			auto rs = unify(lch, rch);
			sol = join(sol, rs);
			if (not sol.is_satisfiable())     // Stop if unification has failed
				return sol;
		}
	}
	return sol;
}

Unify::SolutionSet Unify::comb_unify(const std::set<CHandle>& chs) const
{
	SolutionSet sol(true);
	for (auto lit = chs.begin(); lit != chs.end(); ++lit) {
		for (auto rit = std::next(lit); rit != chs.end(); ++rit) {
			auto rs = unify(*lit, *rit);
			sol = join(sol, rs);
			if (not sol.is_satisfiable())     // Stop if unification has failed
				return sol;
		}
	}
	return sol;
}

HandleSeq Unify::cp_erase(const HandleSeq& hs, Arity i) const
{
	HandleSeq hs_cp(hs);
	hs_cp.erase(hs_cp.begin() + i);
	return hs_cp;
}

Unify::SolutionSet Unify::mkvarsol(CHandle lch, CHandle rch) const
{
	// Attempt to consume quotation to avoid putting quoted elements
	// in the block.
	if (lch.is_free_variable() and rch.is_consumable() and rch.is_quoted())
		rch.update();
	if (rch.is_free_variable() and lch.is_consumable() and lch.is_quoted())
		lch.update();

	CHandle inter = type_intersection(lch, rch);
	if (not inter)
		return SolutionSet();
	else {

		// Special case: if the variable is a glob, and it matched
		// just one item, then unwrap that item. This seems ...
		// kind of perverse to me, but the unit tests expect this
		// behavior. Maybe change things in the future?
		if (GLOB_NODE == lch.handle->get_type() and
		    LIST_LINK == rch.handle->get_type() and
		    1 == rch.handle->get_arity())
		{
			rch.handle = rch.handle->getOutgoingAtom(0);
			if (rch.handle == lch.handle) return SolutionSet(true);
			inter.handle = rch.handle;
		}
		if (GLOB_NODE == rch.handle->get_type() and
		    LIST_LINK == lch.handle->get_type() and
		    1 == lch.handle->get_arity())
		{
			lch.handle = lch.handle->getOutgoingAtom(0);
			if (rch.handle == lch.handle) return SolutionSet(true);
			inter.handle = lch.handle;
		}
		Block pblock{lch, rch};
		Partitions par{{{pblock, inter}}};
		return SolutionSet(par);
	}
}

Unify::SolutionSet Unify::join(const SolutionSet& lhs,
                               const SolutionSet& rhs) const
{
	// No need to join if one of them is non satisfiable
	if (not lhs.is_satisfiable() or not rhs.is_satisfiable())
		return SolutionSet();

	// By now both are satisfiable, thus non empty, join them
	SolutionSet result;
	for (const Partition& rp : rhs)
		result.insert(join(lhs, rp));
	return result;
}

Unify::SolutionSet Unify::join(const SolutionSet& lhs, const Partition& rhs) const
{
	// Base cases
	if (rhs.empty())
		return lhs;

	// Recursive case (a loop actually)
	SolutionSet result;
	for (const auto& par : lhs)
		result.insert(join(par, rhs));
	return result;
}

Unify::SolutionSet Unify::join(const Partition& lhs, const Partition& rhs) const
{
	// Don't bother joining if lhs is empty (saves a bit of computation)
	if (lhs.empty())
		return SolutionSet({rhs});

	// Join
	SolutionSet result({lhs});
	for (const TypedBlock& rhs_block : rhs) {
		// For now we assume result has only 0 or 1 partition
		result = join(result, rhs_block);
		if (not result.is_satisfiable())
			return SolutionSet();
	}

	return result;
}

Unify::SolutionSet Unify::join(const SolutionSet& sol,
                               const TypedBlock& block) const
{
	SolutionSet result;
	for (const Partition& partition : sol)
		result.insert(join(partition, block));
	return result;
}

Unify::SolutionSet Unify::join(const Partition& partition,
                               const TypedBlock& block) const
{
	// Find all partition blocks that have elements in common with block
	TypedBlockSeq common_blocks;
	for (const TypedBlock& p_block : partition)
		if (not has_empty_intersection(block.first, p_block.first))
			common_blocks.push_back(p_block);

	Partition jp(partition);
	if (common_blocks.empty()) {
		// If none then merely insert the independent block
		jp.insert(block);
		return SolutionSet({jp});
	} else {
		// Otherwise join block with all common blocks and replace
		// them by the result (if satisfiable, otherwise return the
		// empty solution set)
		TypedBlock j_block = join(common_blocks, block);
		if (is_satisfiable(j_block)) {
			for (const TypedBlock& rm : common_blocks)
				jp.erase(rm.first);
			jp.insert(j_block);

			// Perform the sub-unification of all common blocks with
			// block and join the solution set to jp
			SolutionSet sol = subunify(common_blocks, block);
			if (sol.is_satisfiable())
				return join(sol, jp);
		}
		return SolutionSet();
	}
}

Unify::TypedBlock Unify::join(const TypedBlockSeq& common_blocks,
                              const TypedBlock& block) const
{
	std::pair<Block, CHandle> result{block};
	for (const auto& c_block : common_blocks) {
		result =  join(result, c_block);
		// Abort if unsatisfiable
		if (not is_satisfiable(result))
			return result;
	}
	return result;
}

Unify::TypedBlock Unify::join(const TypedBlock& lhs, const TypedBlock& rhs) const
{
	OC_ASSERT(lhs.second and rhs.second, "Can only join 2 satisfiable blocks");
	return {set_union(lhs.first, rhs.first),
			type_intersection(lhs.second, rhs.second)};
}

Unify::SolutionSet Unify::subunify(const TypedBlockSeq& common_blocks,
                                   const TypedBlock& block) const
{
	// Form a set with all terms
	std::set<CHandle> all_chs(block.first);
	for (const TypedBlock& cb : common_blocks)
		all_chs.insert(cb.first.begin(), cb.first.end());

	// Build a set of all pairs of terms that may have not been
	// unified so far.
	std::set<CHandlePair> not_unified;
	// This function returns true iff both terms are in the given
	// block. If so it means they have already been unified.
	auto both_in_block = [](const CHandle& lch, const CHandle& rch,
	                        const TypedBlock& block) {
		return contains(block.first, lch) and contains(block.first, rch);
	};
	for (auto lit = all_chs.begin(); lit != all_chs.end(); ++lit) {
		for (auto rit = std::next(lit); rit != all_chs.end(); ++rit) {
			// Check if they are in block
			bool already_unified = both_in_block(*lit, *rit, block);
			// If not, then check if they are in one of the common
			// blocks
			if (not already_unified) {
				for (const TypedBlock& cb : common_blocks) {
					already_unified = both_in_block(*lit, *rit, cb);
					if (already_unified)
						break;
				}
			}
			if (not already_unified)
				not_unified.insert({*lit, *rit});
		}
	}

	// Unify all not unified yet terms
	return pairwise_unify(not_unified);
}

Unify::SolutionSet Unify::subunify(const TypedBlock& lhs,
                                   const TypedBlock& rhs) const
{
	return comb_unify(set_symmetric_difference(lhs.first, rhs.first));
}

bool Unify::is_satisfiable(const TypedBlock& block) const
{
	return (bool)block.second;
}

bool unifiable(const Handle& lhs, const Handle& rhs,
               const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	Unify unify(lhs, rhs, lhs_vardecl, rhs_vardecl);
	return unify().is_satisfiable();
}

bool hm_content_eq(const HandleMap& lhs, const HandleMap& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not content_eq(lit->first, rit->first)
		   or not content_eq(lit->second, rit->second))
			return false;
		++lit; ++rit;
	}
	return true;
}

bool hchm_content_eq(const Unify::HandleCHandleMap& lhs,
                     const Unify::HandleCHandleMap& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not content_eq(lit->first, rit->first)
		    or lit->second != rit->second)
			return false;
		++lit; ++rit;
	}
	return true;
}

bool ts_content_eq(const Unify::TypedSubstitution& lhs,
                   const Unify::TypedSubstitution& rhs)
{
	return lhs.first.size() == rhs.first.size()
		and hchm_content_eq(lhs.first, rhs.first)
		and content_eq(lhs.second, rhs.second);
}

bool tss_content_eq(const Unify::TypedSubstitutions& lhs,
                    const Unify::TypedSubstitutions& rhs)
{
	if (lhs.size() != rhs.size())
		return false;

	auto lit = lhs.begin();
	auto rit = rhs.begin();
	while (lit != lhs.end()) {
		if (not ts_content_eq(*lit, *rit))
			return false;
		++lit; ++rit;
	}
	return true;
}

HandleMap strip_context(const Unify::HandleCHandleMap& hchm)
{
	HandleMap result;
	for (auto& el : hchm) {
		const Context& ctx = el.second.context;
		Handle val = el.second.handle;

		// Insert quotation links if necessary
		for (int i = 0; i < ctx.quotation.level(); i++) {
			if (i == 0 and ctx.quotation.is_locally_quoted())
				val = Handle(createLink(LOCAL_QUOTE_LINK, val));
			else
				val = Handle(createLink(QUOTE_LINK, val));
		}

		// Recreate variable to value mapping without context
		result.insert({el.first, val});
	}
	return result;
}

Unify::CHandle Unify::type_intersection(const CHandle& lch, const CHandle& rch) const
{
	if (inherit(lch, rch))
		return lch;
	if (inherit(rch, lch))
		return rch;
	return Handle::UNDEFINED;
}

TypeSet Unify::simplify_type_union(TypeSet& type) const
{
	return {}; // TODO: do we really need that?
}

TypeSet Unify::get_union_type(const Handle& h) const
{
	const VariableTypeMap& vtm = _variables._typemap;
	auto it = vtm.find(h);
	if (it == vtm.end() or it->second->get_simple_typeset().empty())
		return {ATOM};
	else {
		return it->second->get_simple_typeset();
	}
}

bool Unify::inherit(const CHandle& lch, const CHandle& rch) const
{
	return inherit(lch.handle, rch.handle, lch.context, rch.context);
}

bool Unify::inherit(const Handle& lh, const Handle& rh,
                    Context lc, Context rc) const
{
	Type lt = lh->get_type();
	Type rt = rh->get_type();

	// Recursive cases

	// Consume quotations
	if (lc.quotation.consumable(lt)) {
		lc.quotation.update(lt);
		return inherit(lh->getOutgoingAtom(0), rh, lc, rc);
	}
	if (rc.quotation.consumable(rt)) {
		rc.quotation.update(rt);
		return inherit(lh, rh->getOutgoingAtom(0), lc, rc);
	}

	// If both are links then check that the outgoings of lhs inherit
	// the outgoings of rhs.
	if (lh->is_link() and rh->is_link() and (lt == rt)) {
		if (lh->get_arity() == rh->get_arity()) {
			for (size_t i = 0; i < lh->get_arity(); i++) {
				if (not inherit(lh->getOutgoingAtom(i),
				                rh->getOutgoingAtom(i),
				                lc, rc))
					return false;
			}
			return true;
		} else return false;
	}

	// Base cases

	// If they are equal then lh trivial inherits from rh
	if (lh == rh)
		return true;

	// If both are free variables and declared then look at their types
	// (only simple types are considered for now).
	if (is_free_declared_variable(lc, lh) and is_free_declared_variable(rc, rh))
		return inherit(get_union_type(lh), get_union_type(rh)) and
		       inherit(_variables.get_interval(lh), _variables.get_interval(rh));

	// If only rh is a free and declared variable then check whether lh
	// type inherits from it (using Variables::is_type).
	if (is_free_declared_variable(rc, rh))
		return _variables.is_type(rh, lh);

	return false;
}

bool Unify::inherit(Type lhs, Type rhs) const
{
	return nameserver().isA(lhs, rhs);
}

bool Unify::inherit(Type lhs, const TypeSet& rhs) const
{
	for (Type ty : rhs)
		if (inherit(lhs, ty))
			return true;
	return false;
}

bool Unify::inherit(const TypeSet& lhs, const TypeSet& rhs) const
{
	for (Type ty : lhs)
		if (not inherit(ty, rhs))
			return false;
	return true;
}

bool Unify::inherit(const std::pair<double, double> &lgm,
                    const std::pair<double, double> &rgm) const
{
	return rgm.first <= lgm.first and rgm.second >= lgm.second;
}

bool Unify::is_declared_variable(const Handle& h) const
{
	return _variables.varset_contains(h);
}

bool Unify::is_declared_variable(const CHandle& ch) const
{
	return is_declared_variable(ch.handle);
}

bool Unify::is_free_declared_variable(const CHandle& ch) const
{
	return ch.is_free_variable() and is_declared_variable(ch);
}

bool Unify::is_free_declared_variable(const Context& c, const Handle& h) const
{
	return c.is_free_variable(h) and is_declared_variable(h);
}

bool Unify::is_node_satisfiable(const CHandle& lch, const CHandle& rch) const
{
	// First take care of free undeclared variables treated as
	// constants
	if (lch.is_free_variable() and not is_declared_variable(lch) and
	    rch.is_free_variable() and not is_declared_variable(rch))
		return content_eq(lch.handle, rch.handle);
	return lch.is_node_satisfiable(rch);
}

Variables merge_variables(const Variables& lhs, const Variables& rhs)
{
	Variables new_vars(lhs);
	new_vars.extend_intersect(rhs);
	return new_vars;
}

Handle merge_vardecl(const Handle& lhs_vardecl, const Handle& rhs_vardecl)
{
	if (not lhs_vardecl)
		return rhs_vardecl;
	if (not rhs_vardecl)
		return lhs_vardecl;

	Variables
		lhs_vl(lhs_vardecl),
		rhs_vl(rhs_vardecl);

	Variables new_vars = merge_variables(lhs_vl, rhs_vl);
	return new_vars.get_vardecl();
}

std::string oc_to_string(const Unify::CHandle& ch, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "context:" << std::endl
	   << oc_to_string(ch.context, indent + OC_TO_STRING_INDENT) << std::endl
	   << indent << "atom:" << std::endl
	   << oc_to_string(ch.handle, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::Block& pb, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << pb.size();
	int i = 0;
	for (const auto& el : pb)
		ss << std::endl << indent << "catom[" << i++ << "]:" << std::endl
		   << oc_to_string(el, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedBlock& tb, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "block:" << std::endl
	   << oc_to_string(tb.first, indent + OC_TO_STRING_INDENT) << std::endl
	   << indent << "type:" << std::endl
	   << oc_to_string(tb.second, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedBlockSeq& tbs, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << tbs.size();
	for (size_t i = 0; i < tbs.size(); i++)
		ss << std::endl << indent << "typed block[" << i << "]:" << std::endl
		   << oc_to_string(tbs[i], indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::Partition& up, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << up.size();
	int i = 0;
	for (const auto& p : up) {
		ss << std::endl << indent << "block[" << i << "]:" << std::endl
		   << oc_to_string(p.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "type[" << i << "]:" << std::endl
		   << oc_to_string(p.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Unify::Partitions& par, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << par.size();
	int i = 0;
	for (const auto& el : par) {
		ss << std::endl << indent << "typed partition[" << i << "]:"
		   << std::endl << oc_to_string(el, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Unify::HandleCHandleMap& hchm,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << hchm.size();
	int i = 0;
	for (const auto& hch : hchm) {
		ss << std::endl << indent << "atom[" << i << "]:" << std::endl
		   << oc_to_string(hch.first, indent + OC_TO_STRING_INDENT) << std::endl;
		ss << indent << "catom[" << i << "]:" << std::endl
		   << oc_to_string(hch.second, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

std::string oc_to_string(const Unify::HandleCHandleMap::value_type& hch,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "atom:" << std::endl
	   << oc_to_string(hch.first, indent + OC_TO_STRING_INDENT) << std::endl;
	ss << indent << "catom:" << std::endl
	   << oc_to_string(hch.second, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedSubstitution& ts,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "substitution:" << std::endl
	   << oc_to_string(ts.first, indent + OC_TO_STRING_INDENT) << std::endl
	   << indent << "vardecl:" << std::endl
	   << oc_to_string(ts.second, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedSubstitutions::value_type& ts,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "substitution:" << std::endl
	   << oc_to_string(ts.first, indent + OC_TO_STRING_INDENT) << std::endl
	   << indent << "vardecl:" << std::endl
	   << oc_to_string(ts.second, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const Unify::TypedSubstitutions& tss,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << tss.size();
	int i = 0;
	for (const auto& ts : tss) {
		ss << std::endl << indent << "typed substitution[" << i << "]:"
		   << std::endl << oc_to_string(ts, indent + OC_TO_STRING_INDENT);
		i++;
	}
	return ss.str();
}

} // namespace opencog
