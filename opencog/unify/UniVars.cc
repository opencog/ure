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

#include <opencog/atoms/core/TypedVariableLink.h>
#include <opencog/atoms/core/TypeIntersectionLink.h>

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
}

UniVars::UniVars(const HandleSeq& vardecls, bool ordered)
	: Variables(vardecls, ordered)
{
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
			auto vit = vset._typemap.find(h);
			if (vit != vset._typemap.end())
			{
				auto tit = _typemap.find(h);
				if (tit != _typemap.end())
				{
					Handle isect = HandleCast(
						createTypeIntersectionLink(HandleSeq{
							HandleCast(tit->second->get_typedecl()),
							HandleCast(vit->second->get_typedecl())}));
					TypedVariableLinkPtr tvp =
						createTypedVariableLink(h, isect);
					_typemap[h] = tvp;
				}
				else
				{
					_typemap.insert({h, vit->second});
				}
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
	}

	// If either this or the other are ordered then the result is ordered
	_ordered = _ordered or vset._ordered;
}
