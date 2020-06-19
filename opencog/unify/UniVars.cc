/*
 * opencog/unify/UniVars.cc
 *
 * Copyright (C) 2009, 2014, 2015 Linas Vepstas
 *               2019 SingularityNET Foundation
 *
 * Authors: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *          Nil Geisweiller <ngeiswei@gmail.com> Oct 2019
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/util/algorithm.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/NameServer.h>

#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/core/TypedVariableLink.h>
#include <opencog/atoms/core/TypeNode.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/VariableSet.h>

#include "UniVars.h"

using namespace opencog;

/* ================================================================= */

UniVars::UniVars(bool ordered)
	: Variables(ordered)
{
}

UniVars::UniVars(const Handle& vardecl, bool ordered)
	: Variables(vardecl, ordered)
{
	for (const auto& pr: _typemap)
		setup_types(pr.second);
}

UniVars::UniVars(const HandleSeq& vardecls, bool ordered)
	: Variables(vardecls, ordered)
{
	for (const auto& pr: _typemap)
		setup_types(pr.second);
}

/* ================================================================= */
/**
 * Split out the component types from a TypedVariableLink
 * Need to do this in order to perform type intersections.
 * XXX Long run solution would be to create a distinct
 * type-intersection utility... i.e. given two TypedVariableLink's,
 * compute thier intersection...
 *
 * So ... the code here is a temporary work-around ...
 */
void UniVars::setup_types(const TypedVariableLinkPtr& tvlp)
{
	const Handle& varname(tvlp->get_variable());

	const TypeSet& ts = tvlp->get_simple_typeset();
	if (0 < ts.size())
		_simple_typemap.insert({varname, ts});

	const HandleSet& hs = tvlp->get_deep_typeset();
	if (0 < hs.size())
		_deep_typemap.insert({varname, hs});

	const std::pair<size_t, size_t>& gi = tvlp->get_glob_interval();
	if (tvlp->default_interval() != gi)
		_glob_intervalmap.insert({varname, gi});
}

void UniVars::unpack_vartype(const Handle& htypelink)
{
	Variables::unpack_vartype(htypelink);
	TypedVariableLinkPtr tvlp(TypedVariableLinkCast(htypelink));
	setup_types(tvlp);
}

/* ================================================================= */

/// Return true if the other UniVars struct is equal to this one,
/// up to alpha-conversion. That is, same number of variables, same
/// type restrictions, but possibly different variable names.
///
/// This should give exactly the same answer as performing the tests
///    this->is_type(other->varseq) and other->is_type(this->varseq)
/// That is, the variables in this instance should have the same type
/// restrictions as the variables in the other class.
bool UniVars::is_equal(const UniVars& other) const
{
	size_t sz = varseq.size();
	if (other.varseq.size() != sz) return false;

	if (other._ordered != _ordered) return false;

	// Side-by-side comparison
	for (size_t i = 0; i < sz; i++)
	{
		if (not is_equal(other, i))
			return false;
	}
	return true;
}

bool UniVars::is_equal(const UniVars& other, size_t index) const
{
	const Handle& vme(varseq[index]);
	const Handle& voth(other.varseq[index]);

	// If one is a GlobNode, and the other a VariableNode,
	// then its a mismatch.
	if (vme->get_type() != voth->get_type()) return false;

	// If typed, types must match.
	auto sime = _typemap.find(vme);
	auto soth = other._typemap.find(voth);
	if (sime == _typemap.end() and
	    soth != other._typemap.end() and
	    not soth->second->is_untyped()) return false;

	if (sime != _typemap.end())
	{
		if (soth == other._typemap.end() and
		    sime->second->is_untyped()) return true;
		if (soth == other._typemap.end()) return false;

		if (not sime->second->is_equal(*soth->second)) return false;
	}

	// If we got to here, everything must be OK.
	return true;
}

bool UniVars::is_well_typed() const
{
	for (const auto& vt : _simple_typemap)
		if (not opencog::is_well_typed(vt.second))
			return false;
	return true;
}

/* ================================================================= */
/**
 * Simple type checker.
 *
 * Returns true/false if the indicated handle is of the type that
 * we have memoized.  If this typelist contains more than one type in
 * it, then clearly, there is a mismatch.  If there are no type
 * restrictions, then it is trivially a match.  Otherwise, there must
 * be a TypeChoice, and so the handle must be one of the types in the
 * TypeChoice.
 */
bool UniVars::is_type(const Handle& h) const
{
	// The arity must be one for there to be a match.
	if (1 != varset.size()) return false;

	return is_type(varseq[0], h);
}

/**
 * Type checker.
 *
 * Returns true if we are holding the variable `var`, and if
 * the `val` satisfies the type restrictions that apply to `var`.
 */
bool UniVars::is_type(const Handle& var, const Handle& val) const
{
	if (varset.end() == varset.find(var)) return false;

	VariableSimpleTypeMap::const_iterator tit = _simple_typemap.find(var);
	VariableDeepTypeMap::const_iterator dit = _deep_typemap.find(var);

	const Arity num_args = val->get_type() != LIST_LINK ? 1 : val->get_arity();

	// If one is allowed in interval then there are two alternatives.
	// one: val must satisfy type restriction.
	// two: val must be list_link and its unique outgoing satisfies
	//      type restriction.
	if (is_lower_bound(var, 1) and is_upper_bound(var, 1)
	    and is_type(tit, dit, val))
		return true;
	else if (val->get_type() != LIST_LINK or
	         not is_lower_bound(var, num_args) or
	         not is_upper_bound(var, num_args))
		// If the number of arguments is out of the allowed interval
		// of the variable/glob or val is not List_link, return false.
		return false;

	// Every outgoing atom in list must satisfy type restriction of var.
	for (size_t i = 0; i < num_args; i++)
		if (!is_type(tit, dit, val->getOutgoingAtom(i)))
			return false;

	return true;
}

bool UniVars::is_type(VariableSimpleTypeMap::const_iterator tit,
                      VariableDeepTypeMap::const_iterator dit,
                      const Handle& val) const
{
	bool ret = true;

	// Simple type restrictions?
	if (_simple_typemap.end() != tit)
	{
		const TypeSet &tchoice = tit->second;
		Type htype = val->get_type();
		TypeSet::const_iterator allow = tchoice.find(htype);

		// If the argument has the simple type, then we are good to go;
		// we are done.  Else, fall through, and see if one of the
		// others accept the match.
		if (allow != tchoice.end()) return true;
		ret = false;
	}

	// Deep type restrictions?
	if (_deep_typemap.end() != dit)
	{
		const HandleSet &sigset = dit->second;
		for (const Handle& sig : sigset)
		{
			if (value_is_type(sig, val)) return true;
		}
		ret = false;
	}

	// There appear to be no type restrictions...
	return ret;
}

/**
 * Return true if we contain just a single variable, and this one
 * variable is of type gtype (or is untyped). A typical use is that
 * gtype==VARIABLE_LIST.
 */
bool UniVars::is_type(Type gtype) const
{
	if (1 != varseq.size()) return false;

	// Are there any type restrictions?
	const Handle& var = varseq[0];
	VariableTypeMap::const_iterator tit = _typemap.find(var);
	if (_typemap.end() == tit) return true;

	// There are type restrictions; do they match?
	return tit->second->is_type(gtype);
}

/**
 * Simple type checker.
 *
 * Returns true/false if the indicated handles are of the type that
 * we have memoized.
 *
 * XXX TODO this does not currently handle type equations, as outlined
 * on the wiki; We would need the general pattern matcher to do type
 * checking, in that situation.
 */
bool UniVars::is_type(const HandleSeq& hseq) const
{
	// The arities must be equal for there to be a match.
	size_t len = hseq.size();
	if (varset.size() != len) return false;

	// Check the type restrictions.
	for (size_t i=0; i<len; i++)
	{
		if (not is_type(varseq[i], hseq[i])) return false;
	}
	return true;
}

/* ================================================================= */
/**
 * Interval checker.
 *
 * Returns true if the glob satisfies the lower bound
 * interval restriction.
 */
bool UniVars::is_lower_bound(const Handle& glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);
	return (n >= intervals.first);
}

/**
 * Interval checker.
 *
 * Returns true if the glob satisfies the upper bound
 * interval restriction.
 */
bool UniVars::is_upper_bound(const Handle &glob, size_t n) const
{
	const GlobInterval &intervals = get_interval(glob);
	return (n <= intervals.second or intervals.second < 0);
}

/**
 * Interval checker.
 *
 * Returns true if the glob can match a variable number of items.
 * i.e. if it is NOT an ordinary variable.
 */
bool UniVars::is_globby(const Handle &glob) const
{
	const GlobInterval &intervals = get_interval(glob);
	return (1 != intervals.first or 1 != intervals.second);
}

static const GlobInterval& default_interval(Type t)
{
	static const GlobInterval var_def_interval =
			GlobInterval(1, 1);
	static const GlobInterval glob_def_interval =
			GlobInterval(1, SIZE_MAX);
	return t == GLOB_NODE ? glob_def_interval :
			 var_def_interval;
}

const GlobInterval& UniVars::get_interval(const Handle& var) const
{
	const auto& interval = _glob_intervalmap.find(var);

	if (interval == _glob_intervalmap.end())
		return default_interval(var->get_type());

	return interval->second;
}

/* ================================================================= */
/**
 * Extend a set of variables.
 *
 * That is, merge the given variables into this set.
 *
 * If a variable is both in *this and vset then its type intersection
 * is assigned to it.
 */
void UniVars::extend(const UniVars& vset)
{
	for (const Handle& h : vset.varseq)
	{
		auto index_it = index.find(h);
		if (index_it != index.end())
		{
			// Merge the two typemaps, if needed.
			auto stypemap_it = vset._simple_typemap.find(h);
			if (stypemap_it != vset._simple_typemap.end())
			{
				const TypeSet& tms = stypemap_it->second;
				auto tti = _simple_typemap.find(h);
				if(tti != _simple_typemap.end())
					tti->second = set_intersection(tti->second, tms);
				else
					_simple_typemap.insert({h, tms});
			}
		}
		else
		{
			// Found a new variable! Insert it.
			index.insert({h, varseq.size()});

			auto typemap_it = vset._typemap.find(h);
			if (typemap_it != vset._typemap.end())
			{
				unpack_vartype(HandleCast(typemap_it->second));
			}
			else
			{
				varseq.emplace_back(h);
				varset.insert(h);
			}
		}
		// extend _glob_interval_map
		extend_interval(h, vset);
	}

	// If either this or the other are ordered then the result is ordered
	_ordered = _ordered or vset._ordered;
}

inline GlobInterval interval_intersection(const GlobInterval &lhs,
                                          const GlobInterval &rhs)
{
	const auto lb = std::max(lhs.first, rhs.first);
	const auto ub = std::min(lhs.second, rhs.second);
	return lb > ub ? GlobInterval{0, 0} : GlobInterval{lb, ub};
}

void UniVars::extend_interval(const Handle &h, const UniVars &vset)
{
	auto it = _glob_intervalmap.find(h);
	auto is_in_gim = it != _glob_intervalmap.end();
	const auto intersection = not is_in_gim ? vset.get_interval(h) :
			interval_intersection(vset.get_interval(h), get_interval(h));
	if (intersection != default_interval(h->get_type())) {
		if (is_in_gim) it->second = intersection;
		else _glob_intervalmap.insert({h, intersection});
	}
}

void UniVars::erase(const Handle& var)
{
	// Remove from the type maps
	_simple_typemap.erase(var);
	_deep_typemap.erase(var);

	// Remove from the interval map
	_glob_intervalmap.erase(var);

	// Remove Variables
	Variables::erase(var);
}

bool UniVars::operator==(const UniVars& other) const
{
	return is_equal(other);
}

bool UniVars::operator<(const UniVars& other) const
{
	return Variables::operator<(other)
		or (_simple_typemap == other._simple_typemap
		     and _deep_typemap < other._deep_typemap);
}

/// Look up the type declaration for `var`, but create the actual
/// declaration for `alt`.  This is an alpha-renaming.
Handle UniVars::get_type_decl(const Handle& var, const Handle& alt) const
{
	HandleSeq types;

	// Simple type info
	const auto& sit = _simple_typemap.find(var);
	if (sit != _simple_typemap.end())
	{
		for (Type t : sit->second)
			types.push_back(Handle(createTypeNode(t)));
	}

	const auto& dit = _deep_typemap.find(var);
	if (dit != _deep_typemap.end())
	{
		for (const Handle& sig: dit->second)
			types.push_back(sig);
	}

	// Check if ill-typed a.k.a invalid type intersection.
	if (types.empty() and sit != _simple_typemap.end())
	{
		const Handle ill_type = createLink(TYPE_CHOICE);
		return createLink(TYPED_VARIABLE_LINK, alt, ill_type);
	}

	const auto interval = get_interval(var);
	if (interval != default_interval(var->get_type()))
	{
		Handle il = createLink(INTERVAL_LINK,
		                       Handle(createNumberNode(interval.first)),
		                       Handle(createNumberNode(interval.second)));

		if (types.empty())
			return createLink(TYPED_VARIABLE_LINK, alt, il);

		HandleSeq tcs;
		for (Handle tn : types)
			tcs.push_back(createLink(TYPE_SET_LINK, il, tn));
		return tcs.size() == 1 ?
		       createLink(TYPED_VARIABLE_LINK, alt, tcs[0]) :
		       createLink(TYPED_VARIABLE_LINK, alt,
		                  createLink(tcs, TYPE_CHOICE));
	}

	// No/Default interval found
	if (not types.empty())
	{
		Handle types_h = types.size() == 1 ?
		                 types[0] :
		                 createLink(std::move(types), TYPE_CHOICE);
		return createLink(TYPED_VARIABLE_LINK, alt, types_h);
	}

	// No type info
	return alt;
}

Handle UniVars::get_vardecl() const
{
	HandleSeq vardecls;
	for (const Handle& var : varseq)
		vardecls.emplace_back(get_type_decl(var, var));
	if (vardecls.size() == 1)
		return vardecls[0];

	if (_ordered)
		return Handle(createVariableList(std::move(vardecls)));

	return Handle(createVariableSet(std::move(vardecls)));
}

std::string UniVars::to_string(const std::string& indent) const
{
	std::stringstream ss;

	// FreeVariables
	ss << FreeVariables::to_string(indent) << std::endl;

	// Whether it is ordered
	ss << indent << "_ordered = " << _ordered << std::endl;

	// Simple typemap
	std::string indent_p = indent + OC_TO_STRING_INDENT;
	ss << indent << "_simple_typemap:" << std::endl
	   << oc_to_string(_simple_typemap, indent_p) << std::endl;

	// Glob interval map
	ss << indent << "_glob_intervalmap:" << std::endl
	   << oc_to_string(_glob_intervalmap, indent_p) << std::endl;

	// Deep typemap
	ss << indent << "_deep_typemap:" << std::endl
	   << oc_to_string(_deep_typemap, indent_p);

	return ss.str();
}

std::string opencog::oc_to_string(const UniVars& var, const std::string& indent)
{
	return var.to_string(indent);
}

std::string opencog::oc_to_string(const VariableSimpleTypeMap& vtm,
                                   const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << vtm.size();
	unsigned i = 0;
	for (const auto& v : vtm)
	{
		ss << std::endl << indent << "variable[" << i << "]:" << std::endl
		   << oc_to_string(v.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "types[" << i << "]:";
		for (auto& t : v.second)
			ss << " " << nameserver().getTypeName(t);
		i++;
	}
	return ss.str();
}

std::string opencog::oc_to_string(const GlobIntervalMap& gim,
                                   const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << gim.size();
	unsigned i = 0;
	for (const auto& v : gim)
	{
		ss << std::endl << indent << "glob[" << i << "]:" << std::endl
		   << oc_to_string(v.first, indent + OC_TO_STRING_INDENT) << std::endl
		   << indent << "interval[" << i << "]: ";
		double lo = v.second.first;
		double up = v.second.second;
		ss << ((0 <= lo and std::isfinite(lo)) ? "[" : "(") << lo << ", "
		   << up << ((0 <= up and std::isfinite(up)) ? "]" : ")");
		i++;
	}
	return ss.str();
}
