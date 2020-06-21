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

/// The UniVars struct defines a list of typed variables "unbundled"
/// from the hypergraph in which they normally occur. The goal of this
/// structure is to unify Variable type declarations. In particular,
/// it can compute the intersection of two different type declarations.
//
struct UniVars : public Variables
{
	// CTors. The ordered flag indicates whether we care about the
	// order of the variables. It is false by default and only enabled
	// if VariableList is used.
	UniVars(bool ordered=false);
	UniVars(const Handle& vardecl, bool ordered=false);
	UniVars(const HandleSeq& vardecls, bool ordered=false);

	// Extend this by adding in the given variables. If either this or
	// the other are ordered, then the result is ordered
	void extend(const UniVars&);

	// Useful for debugging
	std::string to_string(const std::string& indent=empty_string) const;
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const UniVars& var,
                         const std::string& indent=empty_string);

/** @}*/
}

#endif // _OPENCOG_UNIVARS_H
