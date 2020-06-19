/*
 * opencog/unify/UniVars.h
 *
 * Copyright (C) 2015 Linas Vepstas
 * All Rights Reserved
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

#ifndef _OPENCOG_UNIVARS_H
#define _OPENCOG_UNIVARS_H

#include <map>
#include <set>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/TypedVariableLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef std::map<Handle, TypeSet> VariableSimpleTypeMap;
typedef std::map<Handle, HandleSet> VariableDeepTypeMap;
typedef std::map<Handle, GlobInterval> GlobIntervalMap;

/// The UniVars struct defines a list of typed variables "unbundled"
/// from the hypergraph in which they normally occur. The goal of this
/// structure is to unify Variable type declarations. In particular,
/// it can compute the intersection of two different type declarations.
//
// XXX TODO: The intersection of types should be moved to a utility,
// that takes two TypedVariableLinks, and computes thier intersection
// (or thier union).  Doing the intersection here results in muddy code
// that is hard to maintain withou adding new bugs...
//
struct UniVars : public Variables
{
	// CTors. The ordered flag indicates whether we care about the
	// order of the variables. It is false by default and only enabled
	// if VariableList is used.
	UniVars(bool ordered=false);
	UniVars(const Handle& vardecl, bool ordered=false);
	UniVars(const HandleSeq& vardecls, bool ordered=false);

	/// Unbundled variables and type restrictions for them.

	/// _simple_typemap is the (possibly empty) list of restrictions
	/// on the variable types. It holds a disjunction of class Type.
	/// _deep_typemap holds complex or "deep" type definitions, such
	/// as those defined by SignatureLink.
	VariableSimpleTypeMap _simple_typemap;
	VariableDeepTypeMap _deep_typemap;

	/// To restrict how many atoms should be matched for each of the
	/// GlobNodes in the pattern.
	GlobIntervalMap _glob_intervalmap;

	/// Populate the above...
	void unpack_vartype(const Handle&);
	void setup_types(const TypedVariableLinkPtr&);

	/// Return true iff all variables are well typed. For now only
	/// simple types are supported, specifically if some variable is
	/// simple typed NOTYPE, then it returns false.
	bool is_well_typed() const;

	// Return true if the other UniVars struct is equal to this one,
	// up to alpha-conversion. That is, same number of variables, same
	// type restrictions, but different actual variable names.
	// Same as satisfying this->is_type(other->varseq) and also
	// other->is_type(this->varseq) -- the equality is symmetric.
	bool is_equal(const UniVars& other) const;
	bool is_equal(const UniVars& other, size_t index) const;
	bool operator==(const UniVars& other) const;

	// Comparison operators. Convenient to define containers of UniVars
	bool operator<(const UniVars& other) const;

	// Return true if we are holding a single variable, and the handle
	// given as the argument satisfies the type restrictions (if any).
	// Else return false.
	bool is_type(const Handle&) const;

	// Return true if we are holding a single variable, and it can
	// be the indicated type.
	bool is_type(Type) const;

	// Return true if we are holding the variable `var`, and `val`
	// satisfies the type restrictions that apply to `var`.
	bool is_type(const Handle& var, const Handle& val) const;

	// Return true if the sequence is of the same length as the variable
	// declarations we are holding, and if they satisfy all of the type
	// restrictions (if any).
	bool is_type(const HandleSeq& hseq) const;

	// Return true if the int satisfies the lower bound interval restriction.
	// Return false otherwise.
	bool is_lower_bound(const Handle& glob, size_t n) const;

	// Return true if the int satisfies the upper bound interval restriction.
	// Return false otherwise.
	bool is_upper_bound(const Handle& glob, size_t n) const;

	// Return true if the variable is has a range other than
	// (1,1) i.e. if it can match more than one thing.
	bool is_globby(const Handle& glob) const;

	// Extend this by adding in the given variables. If either this or
	// the other are ordered, then the result is ordered
	void extend(const UniVars&);

	// Erase the given variable, if exist
	void erase(const Handle&);

	/// Return the TypedVariableLink for the indicated variable.
	/// Return just the Variable itself, if its not typed.
	Handle get_type_decl(const Handle&, const Handle&) const;

	/// Inverse of UniVars(vardecl).get_variable()
	///
	/// That is, convert UniVars object into a variable declaration,
	/// that is a VariableList, VariableSet, TypedVariableLink,
	/// VariableNode or GlobNode, suitable for direct use in a
	/// ScopeLink.
	///
	/// If empty then return the empty VariableList or VariableSet.
	Handle get_vardecl() const;

	const GlobInterval& get_interval(const Handle&) const;

	// Useful for debugging
	std::string to_string(const std::string& indent=empty_string) const;

protected:

	bool is_type(VariableSimpleTypeMap::const_iterator,
	             VariableDeepTypeMap::const_iterator,
	             const Handle&) const;

	void extend_interval(const Handle &h, const UniVars &vset);
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const VariableSimpleTypeMap& vtm,
                         const std::string& indent=empty_string);
std::string oc_to_string(const GlobIntervalMap& gim,
                         const std::string& indent=empty_string);
std::string oc_to_string(const UniVars& var,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_UNIVARS_H
