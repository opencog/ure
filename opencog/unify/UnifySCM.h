/*
 * opencog/ure/unify/UnifySCM.h
 *
 * Copyright (c) 2018 by SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#ifndef _OPENCOG_UNIFY_SCM_H
#define _OPENCOG_UNIFY_SCM_H

#include <opencog/atoms/truthvalue/TruthValue.h>

#include <opencog/unify/Unify.h>

#include <opencog/guile/SchemeModule.h>

namespace opencog
{

class UnifySCM : public ModuleWrap
{

public:
	UnifySCM();

	//Unify the Given Handles without a Variable Declaration
	TruthValuePtr ss_unify_prim(Handle h1, Handle h2);
	//Unify the Given Handles with a Variable Declaration
	TruthValuePtr ss_unify(Handle h1, Handle h2,
	                       Handle h1_vardecl, Handle h2_vardecl);
	//Given a List of Handles Chain Unify them.
	//Takes 4 Handles corresponding to h1 h2 h1_vardecl h2_vardecl unifies them.
	//Take the next 4 Handles. h2 of the first set and h1 of the second set
	//should be the same. Use the Sol from the previous Unifaction to substitute
	//h1 for the next/current Substituion
	TruthValuePtr ss_meta_unify(Handle);

	//Unify h1 and h2. Use resulting Sol to substitute subrule
	Handle ss_substiute(Handle subrule, Handle h1, Handle h2,
						Handle h1_vardecl, Handle h2_vardecl);

	//Check if we have an Empty VariableList/Set
	//Return Handle::UNDEFINED if this is the case
	Handle check_empty(Handle h);

protected:
	void init(void);

}; // class

} // namespace opencog

extern "C" {
void opencog_unify_init(void);
};

#endif // _OPENCOG_DISTRIBUTIONAL_VALUE_SCM_H
