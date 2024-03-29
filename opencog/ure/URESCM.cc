/*
 * URESCM.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Sept 2014
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

#ifdef HAVE_GUILE

#include <opencog/ure/URELogger.h>
#include <opencog/guile/SchemeModule.h>

namespace opencog {

class URESCM : public ModuleWrap
{
protected:
	virtual void init();

	/**
	 * The scheme (cog-mandatory-args-fc) function calls this, to
	 * perform forward-chaining.
	 *
	 * @param rbs          A node, holding the name of the rulebase.
	 * @param source       The source atom with which to start the chaining.
	 * @param vardecl      The variable declaration, if any, of the source.
	 * @param trace_as     AtomSpace where to record the inference traces
	 * @param focus_set    A SetLink containing the atoms to which forward
	 *                     chaining will be applied.  If the set link is
	 *                     empty, chaining will be invoked on the entire
	 *                     atomspace.
	 *
	 * @return             A SetLink containing the results of FC inference.
	 */
	Handle do_forward_chaining(Handle rbs,
	                           Handle source,
	                           Handle vardecl,
	                           bool trace_enabled,
	                           AtomSpace *trace_as,
	                           Handle focus_set);

	/**
	 * The scheme (cog-mandatory-args-bc) function calls this, to
	 * perform forward-chaining.
	 *
	 * @param rbs          A node, holding the name of the rulebase.
	 * @param target       The target atom with which to start the chaining from.
	 * @param vardecl      The variable declaration, if any, of the target.
	 * @param trace_as     AtomSpace where to record the back-inference traces
	 * @param control_as   AtomSpace where to find the inference control rules
	 * @param focus_set    A SetLink containing the atoms to which forward
	 *                     chaining will be applied.  If the set link is
	 *                     empty, chaining will be invoked on the entire
	 *                     atomspace.
	 *
	 * @return             A SetLink containing the results of FC inference.
	 */
	Handle do_backward_chaining(Handle rbs,
	                            Handle target,
	                            Handle vardecl,
	                            bool trace_enabled,
	                            AtomSpace* trace_as,
	                            bool control_enabled,
	                            AtomSpace* control_as,
	                            Handle focus_set);

	Handle get_rulebase_rules(Handle rbs);

	/**
	 * Return the URE logger
	 */
	Logger* do_ure_logger();

public:
	URESCM();
};

} /*end of namespace opencog*/

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/guile/SchemePrimitive.h>

#include "forwardchainer/ForwardChainer.h"
#include "backwardchainer/BackwardChainer.h"
#include "UREConfig.h"

using namespace opencog;

URESCM::URESCM() : ModuleWrap("opencog ure") {}

/// This is called while (opencog ure) is the current module.
/// Thus, all the definitions below happen in that module.
void URESCM::init(void)
{
	define_scheme_primitive("cog-mandatory-args-fc",
		&URESCM::do_forward_chaining, this, "ure");

	define_scheme_primitive("cog-mandatory-args-bc",
		&URESCM::do_backward_chaining, this, "ure");

	define_scheme_primitive("cog-ure-logger",
		&URESCM::do_ure_logger, this, "ure");
}

Handle URESCM::do_forward_chaining(Handle rbs,
                                   Handle source,
                                   Handle vardecl,
                                   bool trace_enabled,
                                   AtomSpace *trace_as,
                                   Handle focus_set_h)
{
	AtomSpacePtr asp = SchemeSmob::ss_get_env_as("cog-mandatory-args-fc");
	HandleSeq focus_set = {};

	// A ListLink means that the variable declaration is undefined
	if (vardecl->get_type() == LIST_LINK)
		vardecl = Handle::UNDEFINED;

	if (not trace_enabled)
		trace_as = nullptr;

	if (focus_set_h->get_type() == SET_LINK)
		focus_set = focus_set_h->getOutgoingSet();
	else
		throw RuntimeException(
			TRACE_INFO,
			"URESCM::do_forward_chaining - focus set should be SET_LINK type!");

	ForwardChainer fc(*asp.get(), rbs, source, vardecl, trace_as, focus_set);
	fc.do_chain();
	return fc.get_results();
}

Handle URESCM::do_backward_chaining(Handle rbs,
                                    Handle target,
                                    Handle vardecl,
                                    bool trace_enabled,
                                    AtomSpace *trace_as,
                                    bool control_enabled,
                                    AtomSpace *control_as,
                                    Handle focus_link)
{
	// A ListLink means that the variable declaration is undefined
	if (vardecl->get_type() == LIST_LINK)
		vardecl = Handle::UNDEFINED;

	if (not trace_enabled)
		trace_as = nullptr;

	if (not control_enabled)
		control_as = nullptr;

	AtomSpacePtr asp = SchemeSmob::ss_get_env_as("cog-mandatory-args-bc");
	BackwardChainer bc(*asp.get(), rbs, target, vardecl, trace_as, control_as, focus_link);

	bc.do_chain();

	return bc.get_results();
}

Logger* URESCM::do_ure_logger()
{
	return &ure_logger();
}

extern "C" {
void opencog_ure_init(void);
};

void opencog_ure_init(void)
{
	static URESCM ure;
	ure.module_init();
}

#endif // HAVE_GUILE
