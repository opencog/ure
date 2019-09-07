/*
 * DistributionalValueSCMDV.cc
 *
 * Scheme small objects (SMOBS) for attention values.
 *
 * Copyright (c) 2018 SingularityNet
 *
 * Written by Roman Treutlein <roman@singularitynet.io>
 */

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

#include <opencog/atoms/base/Node.h>

#include <opencog/unify/UnifySCM.h>

using namespace opencog;

UnifySCM::UnifySCM()
	: ModuleWrap ("opencog unify") {}

void UnifySCM::init(void)
{
	define_scheme_primitive("cog-unify-prim",
	                        &UnifySCM::ss_unify_prim,
	                        this, "unify");
	define_scheme_primitive("cog-unify",
	                        &UnifySCM::ss_unify,
	                        this, "unify");
	define_scheme_primitive("cog-meta-unify",
	                        &UnifySCM::ss_meta_unify,
	                        this, "unify");
	define_scheme_primitive("cog-substitute",
	                        &UnifySCM::ss_substiute,
	                        this, "unify");
}

TruthValuePtr UnifySCM::ss_unify_prim(Handle h1, Handle h2)
{
	Unify unify(h1, h2);
	Unify::SolutionSet sol = unify();

	if (sol.is_satisfiable())
		return TruthValue::TRUE_TV();
	else
		return TruthValue::FALSE_TV();
}

TruthValuePtr UnifySCM::ss_unify(Handle h1, Handle h2,
                                 Handle h1_vardecl, Handle h2_vardecl)
{
	if (h1_vardecl->get_type() == VARIABLE_LIST &&
	    h1_vardecl->getOutgoingSet().size() == 0)
		h1_vardecl = Handle::UNDEFINED;
	if (h2_vardecl->get_type() == VARIABLE_LIST &&
	    h2_vardecl->getOutgoingSet().size() == 0)
		h2_vardecl = Handle::UNDEFINED;
	Unify unify(h1, h2, h1_vardecl, h2_vardecl);
	Unify::SolutionSet sol = unify();

	if (sol.is_satisfiable())
		return TruthValue::TRUE_TV();
	else
		return TruthValue::FALSE_TV();
}

TruthValuePtr UnifySCM::ss_meta_unify(Handle lst)
{
	HandleSeq hseq = lst->getOutgoingSet();

	if (hseq[2]->get_type() == VARIABLE_LIST &&
	    hseq[2]->getOutgoingSet().size() == 0)
		hseq[2] = Handle::UNDEFINED;
	if (hseq[3]->get_type() == VARIABLE_LIST &&
	    hseq[3]->getOutgoingSet().size() == 0)
		hseq[3] = Handle::UNDEFINED;
	Unify unify = Unify(hseq[0], hseq[1], hseq[2], hseq[3]);
	Unify::SolutionSet sol = unify();

	if (not sol.is_satisfiable())
		return TruthValue::FALSE_TV();

	for (int i = 4; i < hseq.size(); i += 4)
	{
		if (hseq[i+2]->get_type() == VARIABLE_LIST &&
			hseq[i+2]->getOutgoingSet().size() == 0)
			hseq[i+2] = Handle::UNDEFINED;
		if (hseq[i+3]->get_type() == VARIABLE_LIST &&
			hseq[i+3]->getOutgoingSet().size() == 0)
			hseq[i+3] = Handle::UNDEFINED;

		FreeVariables vars;
		vars.find_variables(hseq[i]);
		Unify::TypedSubstitutions tss = unify.typed_substitutions(sol, hseq[i-4]);
		if (vars.varseq.size() != 0)
			for (const auto& ts : tss)
			{
				HandleMap m = strip_context(ts.first);
				hseq[i] = vars.substitute_nocheck(hseq[i], m);
				hseq[i+2] = Unify::substitute_vardecl(hseq[i+2], m);
			}

		unify = Unify(hseq[i], hseq[i+1], hseq[i+2], hseq[i+3]);
		sol = unify();

		if (not sol.is_satisfiable())
			return TruthValue::FALSE_TV();
	}

	return TruthValue::TRUE_TV();
}

Handle UnifySCM::ss_substiute(Handle subrule, Handle h1, Handle h2,
                              Handle h1_vardecl, Handle h2_vardecl)
{
	//FIXME This should not be neccesary but for some reason in the
	//test_conditional_partial_instantiation the Quote and DontEx don't get consumed
	if (subrule->get_type() == QUOTE_LINK)
		subrule = filter_quote(subrule->getOutgoingAtom(0))->getOutgoingAtom(0);

	if (h1_vardecl->get_type() == VARIABLE_LIST &&
	    h1_vardecl->getOutgoingSet().size() == 0)
		h1_vardecl = Handle::UNDEFINED;
	if (h2_vardecl->get_type() == VARIABLE_LIST &&
	    h2_vardecl->getOutgoingSet().size() == 0)
		h2_vardecl = Handle::UNDEFINED;
	Unify unify(h1, h2, h1_vardecl, h2_vardecl);
	Unify::SolutionSet sol = unify();

	if (not sol.is_satisfiable())
		return createLink(DONT_EXEC_LINK,subrule);

	AtomSpace *as = SchemeSmob::ss_get_env_as("fetch-incoming-set");

	Unify::TypedSubstitutions tss = unify.typed_substitutions(sol, h2);

	if (not is_meta(subrule))
		for (const auto& ts : tss)
			subrule = Unify::substitute(BindLinkCast(subrule), ts, as);

	//If the Conclusion contains a variable it will be a free variable
	//at this point so it won't have been substituted above:
	FreeVariables vars;

	vars.find_variables(subrule);
	if (vars.varseq.size() != 0)
		for (const auto& ts : tss)
		{
			HandleMap m = strip_context(ts.first);
			subrule = vars.substitute_nocheck(subrule,m);
		}

	return createLink(DONT_EXEC_LINK,subrule);
}

bool opencog::is_meta(const Handle& h)
{
	BindLinkPtr h_bl(BindLinkCast(h));
	if (not h_bl)
		return false;
	Handle implicand = h_bl->get_implicand();

	if (not implicand)
		return false;

	Type itype = implicand->get_type();
	if (Quotation::is_quotation_type(itype))
		implicand = implicand->getOutgoingAtom(0);

	if (implicand->get_type() == BIND_LINK)
		return true;

	auto schema = "scm: cog-substitute";

	if (implicand->get_type() == EXECUTION_OUTPUT_LINK)
		return NodeCast(implicand->getOutgoingAtom(0))->get_name() == schema;

	return false;
}

Handle opencog::filter_quote(Handle h)
{
	Type t = h->get_type();

	if (t == UNQUOTE_LINK)
		return h->getOutgoingAtom(0);

	if (h->is_node())
		return h;

	HandleSeq res;
	for (Handle oh : h->getOutgoingSet())
	{
		res.push_back(filter_quote(oh));
	}
	return createLink(res,t);
}

void opencog_unify_init(void)
{
	static UnifySCM unifySCM;
	unifySCM.module_init();
}
/* ===================== END OF FILE ============================ */
