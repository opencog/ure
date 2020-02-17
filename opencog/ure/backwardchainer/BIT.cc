/*
 * BIT.cc
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

#include <boost/range/algorithm/binary_search.hpp>
#include <boost/range/algorithm/lower_bound.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/algorithm/unique.hpp>
#include <boost/range/algorithm/find.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm_ext/erase.hpp>
#include <boost/algorithm/cxx11/all_of.hpp>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/join.hpp>

#include <opencog/util/random.h>
#include <opencog/util/algorithm.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/execution/LibraryManager.h>
#include <opencog/atoms/pattern/PatternUtils.h>

#include "BIT.h"
#include "../URELogger.h"
#include <opencog/ure-utility/Utility.h>

namespace opencog {

/////////////
// BITNode //
/////////////

BITNode::BITNode(const Handle& bd, const BITNodeFitness& fi)
	: body(bd), fitness(fi), exhausted(false) {
	complexity = -std::log(operator()());
}

double BITNode::operator()() const
{
	// The probably estimate is anti-proportional to the fitness. The
	// assumption used here is that, if the fitness is already high,
	// expanding the BIT-Node is less likely to increase it. This
	// assumption is perfectly right when the fitness equals its upper
	// bound, but isn't generally right otherwise.
	return (exhausted ? 0.0 : 1.0) *
		(fitness.upper - fitness(*this)) / (fitness.upper - fitness.lower);
}

std::string	BITNode::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "body:" << std::endl
	   << oc_to_string(body, indent + OC_TO_STRING_INDENT) << std::endl
	   << indent << "exhausted: " << exhausted << std::endl
	   << indent << "rules:" << std::endl
	   << (indent + oc_to_string_indent) << "size = " << rules.size();
	std::string rule_indent = indent + oc_to_string_indent + oc_to_string_indent;
	size_t i = 0;
	for (const auto& rule : rules)
		ss << std::endl << (indent + oc_to_string_indent)
		   << "rule[" << i++ << "]:" << std::endl
		   << rule.first.to_short_string(rule_indent);
	return ss.str();
}

////////////
// AndBIT //
////////////

AndBIT::AndBIT() : complexity(0), exhausted(false), queried_as(nullptr) {}

AndBIT::AndBIT(AtomSpace& bit_as, const Handle& target, Handle vardecl,
               const BITNodeFitness& fitness, const AtomSpace* qas)
	: exhausted(false), queried_as(qas)
{
	// Create initial FCS
	vardecl = gen_vardecl(target, vardecl); // in case it is undefined
	Handle body =
		Unify::remove_constant_clauses(vardecl, target, queried_as);
	HandleSeq bl{body, target};
	vardecl = filter_vardecl(vardecl, body); // remove useless vardecl
	if (vardecl)
		bl.insert(bl.begin(), vardecl);
	fcs = bit_as.add_link(BIND_LINK, std::move(bl));

	// Insert the initial BITNode and initialize the AndBIT complexity
	auto it = insert_bitnode(target, fitness);
	complexity = it->second.complexity;
}

AndBIT::AndBIT(const Handle& f, double cpx, const AtomSpace* qas)
	: fcs(f), complexity(cpx), exhausted(false), queried_as(qas)
{
	set_leaf2bitnode();         // TODO: might differ till needed to optimize
}

AndBIT::~AndBIT() {}

AndBIT AndBIT::expand(const Handle& leaf,
                      const RuleTypedSubstitutionPair& rule,
                      double prob) const
{
	Handle new_fcs = expand_fcs(leaf, rule);
	double new_cpx = expand_complexity(leaf, prob);

	// Only consider expansions that actually expands
	if (content_eq(fcs, new_fcs)) {
		ure_logger().warn() << "The new FCS is equal to the old one. "
							<< "There is probably a bug. This expansion has "
							<< "been cancelled.";
		return AndBIT();
	}

	// Discard expansion with cycle
	if (has_cycle(new_fcs)) {
		ure_logger().debug() << "The new FCS has some cycle (some conclusion "
							 << "has itself has premise, directly or "
							 << "indirectly). This expansion has been cancelled.";
		return AndBIT();
	}

	return AndBIT(new_fcs, new_cpx, queried_as);
}

BITNode* AndBIT::select_leaf()
{
	// Generate the distribution over target leaves according to the
	// BIT-node fitnesses. The higher the fitness the lower the chance
	// of being selected as it is already fit.
	std::vector<double> weights;
	bool all_weights_null = true;
	for (const auto& lb : leaf2bitnode) {
		double p = lb.second();
		weights.push_back(p);
		if (p > 0) all_weights_null = false;
	}

	// Check that the distribution is well defined
	if (all_weights_null)
		return nullptr;

	// If well defined then sample according to it
	LeafDistribution dist(weights.begin(), weights.end());
	return &rand_element(leaf2bitnode, dist).second;
}

void AndBIT::reset_exhausted()
{
	for (auto& el : leaf2bitnode)
		el.second.exhausted = false;
	exhausted = false;
}

bool AndBIT::has_cycle() const
{
	return has_cycle(fcs);
}

Handle AndBIT::ExtractSubFCS(Handle fcs) const
{
	Handle rewrite = BindLinkCast(fcs)->get_implicand()[0]; //Assume there is only one
	Type rewrite_type = rewrite->get_type();

	if (rewrite_type == EXECUTION_OUTPUT_LINK)
	{
		while (rewrite_type == EXECUTION_OUTPUT_LINK)
		{
			rewrite = rewrite->getOutgoingAtom(1)->getOutgoingAtom(0);
			rewrite_type = rewrite->get_type();
		}
		rewrite = rewrite->getOutgoingAtom(0)->getOutgoingAtom(0);
	}
	else
		rewrite = rewrite->getOutgoingAtom(0);

	return rewrite;
}

bool AndBIT::has_cycle(Handle h, HandleSet ancestors) const
{
	while (is_meta(h))
		h = ExtractSubFCS(h);

	if (h->get_type() == BIND_LINK)
		return has_cycle(BindLinkCast(h)->get_implicand()[0]); //Assume there is only one

	if (h->get_type() == EXECUTION_OUTPUT_LINK) {
		Handle arg = h->getOutgoingAtom(1);
		if (arg->get_type() == LIST_LINK) {
			Handle conclusion = arg->getOutgoingAtom(0);
			if (is_in(conclusion, ancestors))
				return true;

			ancestors.insert(conclusion);
			Arity arity = arg->get_arity();
			if (1 < arity) {
				bool unordered_premises =
					arg->getOutgoingAtom(1)->get_type() == SET_LINK;
				if (unordered_premises) {
					OC_ASSERT(arity == 2,
							  "Mixture of ordered and unordered"
							  " premises not implemented!");
					arg = arg->getOutgoingAtom(1);
					for (const Handle& ph : arg->getOutgoingSet())
						if (has_cycle(ph, ancestors))
							return true;
					return false;
				} else {
					for (Arity i = 1; i < arity; ++i) {
						Handle ph = arg->getOutgoingAtom(i);
						if (has_cycle(ph, ancestors))
							return true;
						return false;
					}
				}
			}
			return false;
		} else {
			return is_in(arg, ancestors);
		}
	} else {
		return is_in(h, ancestors);
	}
}

bool AndBIT::operator==(const AndBIT& andbit) const
{
	return fcs == andbit.fcs;
}

bool AndBIT::operator<(const AndBIT& andbit) const
{
	// Sort by complexity to so that simpler and-BITs come first. Then
	// by content. Makes it easier to prune by complexity. It should
	// also make sampling a bit faster. And finally the user probabably
	// want that.
	return (complexity < andbit.complexity)
		or (complexity == andbit.complexity
			and content_based_handle_less()(fcs, andbit.fcs));
}

std::string AndBIT::to_string(const std::string& indent) const
{
	return oc_to_string(fcs, indent);
}

std::string AndBIT::fcs_to_ascii_art(const Handle& nfcs) const
{
	return fcs_rewrite_to_ascii_art(BindLinkCast(nfcs)->get_implicand()[0]);
}

std::string AndBIT::fcs_rewrite_to_ascii_art(const Handle& h) const
{
	if (h->get_type() == EXECUTION_OUTPUT_LINK) {
		Handle gsn = h->getOutgoingAtom(0);
		Handle arg = h->getOutgoingAtom(1);
		if (arg->get_type() == LIST_LINK) {
			// Render the conclusion
			Handle conclusion = arg->getOutgoingAtom(0);
			std::string conclusion_aa = fcs_rewrite_to_ascii_art(conclusion);

			// Render the premises
			Arity arity = arg->get_arity();
			if (1 < arity) {
				std::vector<std::string> premises_aas;
				bool unordered_premises =
					arg->getOutgoingAtom(1)->get_type() == SET_LINK;
				if (unordered_premises) {
					OC_ASSERT(arity == 2,
							  "Mixture of ordered and unordered"
							  " premises not implemented!");
					arg = arg->getOutgoingAtom(1);
					for (const Handle& ph : arg->getOutgoingSet())
						premises_aas.push_back(fcs_rewrite_to_ascii_art(ph));
				} else {
					for (Arity i = 1; i < arity; ++i) {
						Handle ph = arg->getOutgoingAtom(i);
						premises_aas.push_back(fcs_rewrite_to_ascii_art(ph));
					}
				}
				// Merge horizontally the ascii arts of all premises
				std::string premises_merged_aa = ascii_art_hmerge(premises_aas);

				// Put a line over the head of the conclusion, with
				// the premises over that line.
				std::string ul(line_separator(premises_merged_aa, conclusion_aa,
											  gsn, unordered_premises));
				unsigned ulls = leading_spaces(ul);
				unsigned ullls = ul.size() + ulls;
				unsigned conclusion_offset = ullls < conclusion_aa.size() ? 0 :
					(ullls - conclusion_aa.size()) / 2;
				std::string conclusion_indent(conclusion_offset, ' ');
				return premises_merged_aa + "\n" + ul + "\n"
					+ conclusion_indent + conclusion_aa;
			} else {
				// No premises, just put a line over the head of the
				// conclusion
				std::string line_str(line_separator("", conclusion_aa, gsn));
				return line_str + "\n" + conclusion_aa;
			}
		} else {
			// No premise, put a line over the head of the conclusion
			std::string conclusion_aa = fcs_rewrite_to_ascii_art(arg);
			std::string line_str(line_separator("", conclusion_aa, gsn));
			return line_str + "\n" + conclusion_aa;
		}
	} else return h->id_to_string();
}

double AndBIT::expand_complexity(const Handle& leaf, double prob) const
{
	// Calculate the complexity of the expanded and-BIT. Sum up the
	// complexity of the parent and-BIT with the complexity of the
	// expanded BIT-node and the complexity of the rule (1 - log(prob))
	return complexity
		+ leaf2bitnode.find(leaf)->second.complexity
		+ 1 - log(prob);
}

Handle AndBIT::force_merge_vardecl(const Handle& lhs, const Handle& rhs) const
{
	if (not lhs)
		return rhs;
	if (not rhs)
		return lhs;

	HandleSeq vars;

	Type lhs_t = lhs->get_type();
	if (lhs_t == VARIABLE_LIST && lhs_t == VARIABLE_SET)
		vars.insert(vars.end(),
					lhs->getOutgoingSet().begin(),
					lhs->getOutgoingSet().end());
	else
		vars.push_back(lhs);

	Type rhs_t = rhs->get_type();
	if (rhs_t == VARIABLE_LIST && rhs_t == VARIABLE_SET)
		vars.insert(vars.end(),
					rhs->getOutgoingSet().begin(),
					rhs->getOutgoingSet().end());
	else
		vars.push_back(rhs);

	if (lhs_t == VARIABLE_LIST || rhs_t == VARIABLE_LIST)
		return fcs->getAtomSpace()->add_link(VARIABLE_LIST, vars);
	else
		return fcs->getAtomSpace()->add_link(VARIABLE_SET, vars);
}

Handle AndBIT::modify_meta(const Handle& subrule, AtomSpace * as,
						   std::function<Handle(const Handle&, AtomSpace *)> func) const
{
	Handle params = subrule->getOutgoingAtom(1);
	Handle quote = params->getOutgoingAtom(0);

	Handle h;
	//Need to handle nested ExecutionOutputLinks
	if (quote->get_type() == EXECUTION_OUTPUT_LINK)
	{
		h = modify_meta(quote, as, func);
	}
	else
	{
		Handle dontex = quote->getOutgoingAtom(0);
		Handle bnd = dontex->getOutgoingAtom(0);

		Handle new_bnd = func(bnd, as);

		h = as->add_link(DONT_EXEC_LINK, new_bnd);
		h = as->add_link(QUOTE_LINK, h);
	}

	Handle nparams = as->add_link(LIST_LINK, h,
								  params->getOutgoingAtom(1),
								  params->getOutgoingAtom(2),
								  params->getOutgoingAtom(3),
								  params->getOutgoingAtom(4));

	return as->add_link(EXECUTION_OUTPUT_LINK,
						subrule->getOutgoingAtom(0),
						nparams);
}

Handle AndBIT::expand_fcs(const Handle& leaf,
                          const RuleTypedSubstitutionPair& rule) const
{
	Handle nfcs;
	AtomSpace * as = fcs->getAtomSpace();
	// Unify the rule conclusion with the leaf, and substitute any
	// variables in it by the associated term.
	BindLinkPtr fcs_bl(BindLinkCast(fcs));
	nfcs = Unify::substitute(fcs_bl, rule.second, queried_as);

	//We need to consider 4 different cases
	//Expanding a Meta FCS with a Meta Rule
	//Expanding a Meta FCS with a normal Rule
	//Expanding a normal FCS with a Meta Rule
	//Expanding a normal Fcs with a normal Rule

	if (is_meta(fcs))
	{
		BindLinkPtr nfcs_bl(BindLinkCast(nfcs));
		Handle nfcs_mvardecl = nfcs_bl->get_vardecl();
		Handle nfcs_mpattern = nfcs_bl->get_body();
		Handle nfcs_mrewrite = nfcs_bl->get_implicand()[0]; //Assume there is only one

		if (rule.first.is_meta())
		{
			LAZY_URE_LOG_DEBUG << "Meta-FCS Expansion With Meta-Rule\n";

			Handle nmvardecl = rule.first.get_vardecl();
			Handle mpattern = rule.first.get_implicant();
			Handle rule_rewrite = rule.first.get_implicand();
			Handle conclusion = rule.first.get_conclusion();

			auto hpair = expand_mfcs_pattern(mpattern,nfcs_mpattern);
			Handle nmpattern = hpair.first;
			nfcs_mpattern = hpair.second;

			std::function<Handle(const Handle&, AtomSpace *)> func;
			func = [&nfcs_mvardecl, &nfcs_mpattern, &nfcs_mrewrite,
					&conclusion, this]
				(const Handle& bnd, AtomSpace * as) -> Handle
				{
					BindLinkPtr blp = BindLinkCast(bnd);
					Handle vardecl = blp->get_vardecl();
					Handle pattern = blp->get_body();
					Handle rewrite = blp->get_implicand()[0]; //Assume there is only one

					//Merge Sub-Rule with FCS
					Handle npattern = expand_fcs_pattern(nfcs_mpattern, conclusion,
														 pattern);

					std::function<Handle(const Handle&, AtomSpace *)> func;
					func = [&conclusion, &rewrite, &func, this]
						(const Handle& bnd, AtomSpace * as) -> Handle
						{
							BindLinkPtr blp = BindLinkCast(bnd);
							Handle nfcs_vardecl = blp->get_vardecl();
							Handle nfcs_pattern = blp->get_body();
							Handle nfcs_rewrite = blp->get_implicand()[0]; //Assume there is only one

							Handle nrewrite;
							if (is_meta(bnd))
								nrewrite = modify_meta(nfcs_rewrite, as, func);
							else
								nrewrite = expand_fcs_rewrite(nfcs_rewrite,
															  conclusion,
															  rewrite);

							if (nfcs_vardecl)
								return as->add_link(BIND_LINK, nfcs_vardecl,
													nfcs_pattern, nrewrite);
							else
								return as->add_link(BIND_LINK,nfcs_pattern,
													nrewrite);

						};

					Handle nrewrite = modify_meta(nfcs_mrewrite, as, func);

					//Force Merge to keep quoted vardels
					Handle nvardecl = filter_vardecl(nfcs_mvardecl,
													 {npattern, nrewrite});
					nvardecl = force_merge_vardecl(nvardecl,vardecl);

					return as->add_link(BIND_LINK,nvardecl,npattern,nrewrite);
				};

			Handle nmrewrite = modify_meta(rule_rewrite, as, func);

			//Create new MFCS
			if (nmvardecl)
				nfcs = as->add_link(BIND_LINK, nmvardecl, nmpattern, nmrewrite);
			else
				nfcs = as->add_link(BIND_LINK, nmpattern, nmrewrite);

		}
		else
		{
			LAZY_URE_LOG_DEBUG << "Expanding Meta-FCS with normal Rule.\n";

			Handle rule_vardecl = rule.first.get_vardecl();
			Handle rule_pattern = rule.first.get_implicant();
			Handle rule_rewrite = rule.first.get_implicand();
			Handle rule_conclusion = rule.first.get_conclusion();

			Handle nmpattern = expand_fcs_pattern(nfcs_mpattern, rule_conclusion,
												  rule_pattern);

			//Deconstruct Meta-FCS Rewrite to access Sub-FCS

			std::function<Handle(const Handle&, AtomSpace *)> func;
			func = [&rule_conclusion, &rule_rewrite, this]
				(const Handle& bnd, AtomSpace * as) -> Handle
				{
					Handle vardecl = BindLinkCast(bnd)->get_vardecl();
					Handle pattern = BindLinkCast(bnd)->get_body();
					Handle rewrite = BindLinkCast(bnd)->get_implicand()[0]; //Assume only one exists

					Handle nrewrite = expand_fcs_rewrite(rewrite,
														 rule_conclusion,
														 rule_rewrite);

					//Rebuild Meta-FCS Rewrite with modified Sub-FCS
					if (vardecl)
						return as->add_link(BIND_LINK, vardecl, pattern, nrewrite);
					else
						return as->add_link(BIND_LINK, pattern, nrewrite);
				 };

			Handle nmrewrite = modify_meta(nfcs_mrewrite, as, func);

			//Merge Vardecls
			Handle nmvardecl = merge_vardecl(nfcs_mvardecl,rule_vardecl);
			nmvardecl = filter_vardecl(nmvardecl,{nmpattern,nmrewrite});

			//Create new MFCS
			if (nmvardecl)
				nfcs = as->add_link(BIND_LINK, nmvardecl, nmpattern, nmrewrite);
			else
				nfcs = as->add_link(BIND_LINK, nmpattern, nmrewrite);
		}
	}
	else
	{
		BindLinkPtr nfcs_bl(BindLinkCast(nfcs));
		Handle nfcs_vardecl = nfcs_bl->get_vardecl();
		Handle nfcs_pattern = nfcs_bl->get_body();
		Handle nfcs_rewrite = nfcs_bl->get_implicand()[0]; // assume that there is only one
		Handle rule_vardecl = rule.first.get_vardecl();

		Handle conclusion = rule.first.get_conclusion();

		if (rule.first.is_meta())
		{
			LAZY_URE_LOG_DEBUG << "Expanding FCS to Meta-FCS\n";
			Handle nmvardecl = rule.first.get_vardecl();
			Handle nmpattern = rule.first.get_implicant();
			Handle rule_rewrite = rule.first.get_implicand();

			std::function<Handle(const Handle&, AtomSpace *)> func;
			func = [&nfcs_vardecl, &nfcs_pattern, &nfcs_rewrite,
						 &conclusion, this]
				(const Handle& bnd, AtomSpace * as)
				{
					Handle vardecl = BindLinkCast(bnd)->get_vardecl();
					Handle pattern = BindLinkCast(bnd)->get_body();
					Handle rewrite = BindLinkCast(bnd)->get_implicand()[0]; // Assume there is only one

					//Merge Sub-Rule with FCS
					Handle npattern = expand_fcs_pattern(nfcs_pattern, conclusion,
														 pattern);

					Handle nrewrite = expand_fcs_rewrite(nfcs_rewrite, conclusion,
														 rewrite);

					//Do we need force merge here?
					Handle nvardecl = filter_vardecl(nfcs_vardecl,
													 {npattern, nrewrite});
					nvardecl = force_merge_vardecl(nvardecl,vardecl);

					//Rebuild Meta-Rule Rewrite with modified Sub-Rule
					return as->add_link(BIND_LINK, nvardecl, npattern, nrewrite);
				};

			Handle nmrewrite = modify_meta(rule_rewrite, as, func);

			//Create new MFCS
			if (nmvardecl)
				nfcs = as->add_link(BIND_LINK, nmvardecl, nmpattern, nmrewrite);
			else
				nfcs = as->add_link(BIND_LINK, nmpattern, nmrewrite);
		}
		else
		{
			LAZY_URE_LOG_DEBUG << "Expanding FCS normally\n";
			Handle implicant = rule.first.get_implicant();
			Handle implicand = rule.first.get_implicand();

			// Generate new pattern term
			Handle npattern = expand_fcs_pattern(nfcs_pattern, conclusion,
												 implicant);

			// Generate new rewrite term
			Handle nrewrite = expand_fcs_rewrite(nfcs_rewrite, conclusion,
												 implicand);

			// Generate new vardecl
			// TODO: is this merging necessary?
			Handle merged_vardecl = merge_vardecl(nfcs_vardecl, rule_vardecl);
			Handle nvardecl = filter_vardecl(merged_vardecl, {npattern, nrewrite});


			// Remove constant clauses from npattern
			npattern = Unify::remove_constant_clauses(nvardecl, npattern,
													  queried_as);

			// Generate new atomese forward chaining s trategy
			HandleSeq noutgoings({npattern, nrewrite});
			if (nvardecl)
				noutgoings.insert(noutgoings.begin(), nvardecl);
	nfcs = as->add_link(BIND_LINK, std::move(noutgoings));
		}

	}
	// Log expansion
	LAZY_URE_LOG_DEBUG << "Expanded forward chainer strategy:" << std::endl
	                   << nfcs->to_string();
	LAZY_URE_LOG_DEBUG << "With inference tree:" << std::endl << std::endl
	                   << fcs_to_ascii_art(nfcs) << std::endl;

	return nfcs;
}

void AndBIT::set_leaf2bitnode()
{
	// For each present clause of a fcs, associate a corresponding BITNode
	BindLinkPtr blp = BindLinkCast(fcs);
	Handle body = blp->get_body();
	for (const Handle& leaf : get_present_clauses(body))
		insert_bitnode(leaf, BITNodeFitness());
}

AndBIT::HandleBITNodeMap::iterator
AndBIT::insert_bitnode(Handle leaf, const BITNodeFitness& fitness)
{
	if (not leaf)
		return leaf2bitnode.end();

	HandleBITNodeMap::iterator it = leaf2bitnode.find(leaf);
	if (it == leaf2bitnode.end())
		return leaf2bitnode.emplace(leaf, BITNode(leaf, fitness)).first;
	return it;
}

std::pair<Handle,Handle>
AndBIT::expand_mfcs_pattern(const Handle& rule_pattern,
                            const Handle& mfcs_pattern) const
{

	//We need to combine cog-unify or cog-meta-unify calls
	//into 1 big cog-meta-unify call
	HandleSeq mfcs_prs_clauses = get_present_clauses(mfcs_pattern);
	HandleSeq mfcs_virt_clauses = get_virtual_clauses(mfcs_pattern);
	HandleSeq prs_clauses = get_present_clauses(rule_pattern);
	HandleSeq virt_clauses = get_virtual_clauses(rule_pattern);

	auto findf = [](const Handle& h){
			if (h->get_type() == EVALUATION_LINK)
			{
				std::string name = NodeCast(h->getOutgoingAtom(0))->get_name();
				if (name == "scm: cog-unify" || name == "scm: cog-meta-unify")
					return true;
			}
			return false;};

	//Find the calls to cog-unify or cog-meta-unfiy
	auto it_mfcs = find_if(mfcs_virt_clauses.begin(),
						   mfcs_virt_clauses.end(),findf);
	auto it_rule = find_if(virt_clauses.begin(),virt_clauses.end(),findf);

	HandleSeq rule_args = (*it_rule)->getOutgoingAtom(1)->getOutgoingSet();
	//Might not be true if the Meta-FCS was created by a higer order Meta-FCS
	//As it was already merged into some other cog-meta-unify and removed here
	if (it_mfcs != mfcs_virt_clauses.end())
	{
		HandleSeq mfcs_args = (*it_mfcs)->getOutgoingAtom(1)->getOutgoingSet();
		rule_args.insert(rule_args.end(),mfcs_args.begin(),mfcs_args.end());
		mfcs_virt_clauses.erase(it_mfcs);
	}
	//Can't use ListLink because we want to pass it as a singele Argument
	Handle narglist = createLink(rule_args,ORDERED_LINK);
	Handle pred = createNode(GROUNDED_PREDICATE_NODE,"scm: cog-meta-unify");
	*it_rule = createLink(EVALUATION_LINK,pred,narglist);

	return std::make_pair(mk_pattern(prs_clauses,virt_clauses),
						  mk_pattern(mfcs_prs_clauses,mfcs_virt_clauses));
}

Handle AndBIT::expand_fcs_pattern(const Handle& fcs_pattern,
                                  const Handle& conclusion,
                                  const Handle& implicant) const
{
	HandleSeq prs_clauses = get_present_clauses(implicant);
	HandleSeq virt_clauses = get_virtual_clauses(implicant);
	HandleSeq prs_fcs_clauses = get_present_clauses(fcs_pattern);
	HandleSeq virt_fcs_clauses = get_virtual_clauses(fcs_pattern);

	// Remove any present fcs clauses that is equal to the conclusion
	auto eq_to_conclusion = [&](const Handle& h) {
								return content_eq(conclusion, h); };
	boost::remove_erase_if(prs_fcs_clauses, eq_to_conclusion);

	// Remove any virtual fcs clause that:
	// 1. is equal to the conclusion.
	// 2. is a precondition that uses that conclusion as argument.
	auto to_remove = [&](const Handle& h) {
						 return eq_to_conclusion(h)
							 or is_argument_of(h, conclusion); };
	boost::remove_erase_if(virt_fcs_clauses, to_remove);

	// Add present rule clauses
	prs_fcs_clauses.insert(prs_fcs_clauses.end(),
						   prs_clauses.begin(), prs_clauses.end());

	// Add virtual rule clauses
	virt_fcs_clauses.insert(virt_fcs_clauses.end(),
							virt_clauses.begin(), virt_clauses.end());

	// Assemble the body
	return mk_pattern(prs_fcs_clauses, virt_fcs_clauses);
}

Handle AndBIT::expand_fcs_rewrite(const Handle& fcs_rewrite,
                                  const Handle& conclusion,
                                  const Handle& implicand) const
{
	// Base cases

	// Replace the fcs rewrite atoms by the rule rewrite if equal to
	// the rule conclusion
	if (content_eq(fcs_rewrite, conclusion))
		return implicand;

	AtomSpace& as = *fcs->getAtomSpace();

	// Recursive cases

	Type t = fcs_rewrite->get_type();

	if (t == EXECUTION_OUTPUT_LINK) {
		// If it is an ExecutionOutput then skip the first input
		// argument as it is a conclusion already.
		Handle gsn = fcs_rewrite->getOutgoingAtom(0);
		Handle arg = fcs_rewrite->getOutgoingAtom(1);
		bool unquote = false;
		if (arg->get_type() == UNQUOTE_LINK)
		{
			arg = arg->getOutgoingAtom(0);
			unquote = true;
		}
		if (arg->get_type() == LIST_LINK) {
			HandleSeq args = arg->getOutgoingSet();
			for (size_t i = 1; i < args.size(); i++)
				args[i] = expand_fcs_rewrite(args[i], conclusion, implicand);
			arg = as.add_link(LIST_LINK, std::move(args));
		}
		if (unquote)
			arg = as.add_link(UNQUOTE_LINK,arg);
		return as.add_link(EXECUTION_OUTPUT_LINK, {gsn, arg});
	} else if (t == SET_LINK) {
		// If a SetLink then treat its arguments as (unordered)
		// premises.
		HandleSeq args = fcs_rewrite->getOutgoingSet();
		for (size_t i = 0; i < args.size(); i++)
			args[i] = expand_fcs_rewrite(args[i], conclusion, implicand);
		return as.add_link(SET_LINK, std::move(args));
	} else
		// If none of the conditions apply just leave alone. Indeed,
		// assuming that the pattern matcher is executing the rewrite
		// term eagerly then it is garantied that all premises TVs
		// will be updated before running a rule, so we don't need to
		// substitute parts of a term containing the conclusion by the
		// application rule(premises).
		return fcs_rewrite;
}

bool AndBIT::is_argument_of(const Handle& eval, const Handle& atom) const
{
	Type evalt = eval->get_type();
	if (evalt == EVALUATION_LINK) {
		Handle args = eval->getOutgoingAtom(1);
		if (content_eq(args, atom))
			return true;
		if (args->get_type() == LIST_LINK)
			for (Arity i = 0; i < args->get_arity(); i++)
				if (content_eq(args->getOutgoingAtom(i), atom))
					return true;
	}
	if (evalt == NOT_LINK)
		return is_argument_of(eval->getOutgoingAtom(0),atom);
	if (evalt == EQUAL_LINK || evalt == IDENTICAL_LINK)
		for (auto arg : eval->getOutgoingSet())
			if (content_eq(arg, atom))
				return true;
	if (atom->is_link())
		for (auto elem : atom->getOutgoingSet())
			if (is_argument_of(eval,elem))
				return true;
	return false;
}

bool AndBIT::is_locally_quoted_eq(const Handle& lhs, const Handle& rhs) const
{
	if (content_eq(lhs, rhs))
		return true;
	Type lhs_t = lhs->get_type();
	Type rhs_t = rhs->get_type();
	if (lhs_t == LOCAL_QUOTE_LINK and rhs_t != LOCAL_QUOTE_LINK)
		return content_eq(lhs->getOutgoingAtom(0), rhs);
	if (lhs_t != LOCAL_QUOTE_LINK and rhs_t == LOCAL_QUOTE_LINK)
		return content_eq(lhs, rhs->getOutgoingAtom(0));
	return false;
}

Handle AndBIT::mk_pattern(HandleSeq prs_clauses, HandleSeq virt_clauses) const
{
	// Remove redundant clauses
	remove_redundant(prs_clauses);
	remove_redundant(virt_clauses);

	// Assemble the body
	AtomSpace& as = *fcs->getAtomSpace();
	if (not prs_clauses.empty())
		virt_clauses.push_back(as.add_link(PRESENT_LINK, std::move(prs_clauses)));
	return virt_clauses.empty() ? Handle::UNDEFINED
		: (virt_clauses.size() == 1 ? virt_clauses.front()
		   : as.add_link(AND_LINK, std::move(virt_clauses)));
}

void AndBIT::remove_redundant(HandleSeq& hs)
{
	boost::sort(hs);
	boost::erase(hs, boost::unique<boost::return_found_end>(hs));
}

HandleSeq AndBIT::get_present_clauses(const Handle& pattern)
{
	if (pattern->get_type() == AND_LINK)
		return get_present_clauses(pattern->getOutgoingSet());
	else if (pattern->get_type() == PRESENT_LINK)
		return pattern->getOutgoingSet();
	else
		return {};
}

HandleSeq AndBIT::get_present_clauses(const HandleSeq& clauses)
{
	HandleSeq prs_clauses;
	for (const Handle& clause : clauses) {
		if (clause->get_type() == PRESENT_LINK) {
			const HandleSeq& oset = clause->getOutgoingSet();
			prs_clauses.insert(prs_clauses.end(), oset.begin(), oset.end());
		}
	}
	return prs_clauses;
}

HandleSeq AndBIT::get_virtual_clauses(const Handle& pattern)
{
	if (pattern->get_type() == AND_LINK)
		return get_virtual_clauses(pattern->getOutgoingSet());
	else if (pattern->get_type() == PRESENT_LINK)
		return {};
	else
		return {pattern};
}

HandleSeq AndBIT::get_virtual_clauses(const HandleSeq& clauses)
{
	HandleSeq virt_clauses;
	for (const Handle& clause : clauses)
		if (clause->get_type() != PRESENT_LINK)
			virt_clauses.push_back(clause);
	return virt_clauses;
}

std::vector<std::string>
AndBIT::ascii_art_hmerge(const std::vector<std::string>& laa,
                         const std::vector<std::string>& raa,
                         unsigned dst)
{
	if (laa.empty())
		return raa;
	if (raa.empty())
		return laa;

	// Max line size of laa (in common with raa)
	int left_max = 0;

	// Min leading spaces of raa (in common with laa)
	int right_min = std::numeric_limits<int>::max();

	// Calculate the merging offset of origin of the right image
	// relative to the origin of the left one.
	size_t ls = laa.size();
	size_t rs = raa.size();
	for (size_t i = 0; i < std::min(ls, rs); ++i) {
		left_max = std::max(left_max, (int)laa[i].size());
		right_min = std::min(right_min, (int)leading_spaces(raa[i]));
	}

	// Only add dst if there is an actual border to not collide with.
	if (left_max) left_max += dst;

	// Where to paste the raa
	size_t offset = std::max(0, left_max - right_min);

	// Perform merging
	std::vector<std::string> result;
	for (size_t i = 0; i < std::max(ls, rs); ++i) {
		std::string line;
		if (i < ls)
			line = laa[i];
		if (i < rs) {
			// Fill with spaces
			while (line.size() < offset)
				line += " ";
			// Remove unused leading spaces
			line += i < ls ? raa[i].substr(line.size() - offset) : raa[i];
		}
		result.push_back(line);
	}
	return result;
}

std::string AndBIT::ascii_art_hmerge(const std::string& laa,
                                     const std::string& raa, unsigned dst)
{
	// Split laa into lines, from bottom to top
	std::vector<std::string> laa_lines = reverse_split(laa);
	// Split raa into lines, from bottom to top
	std::vector<std::string> raa_lines = reverse_split(raa);
	// Produce the merge and join it back into a string
	std::vector<std::string> res_lines =
		ascii_art_hmerge(laa_lines, raa_lines, dst);
	boost::reverse(res_lines);
	return boost::join(res_lines, "\n");
}

std::string AndBIT::ascii_art_hmerge(const std::vector<std::string>& aas,
                                     unsigned dst)
{
	std::string result;
	for (const std::string& aa : aas)
		result = ascii_art_hmerge(result, aa, dst);
	return result;
}

std::vector<std::string> AndBIT::reverse_split(const std::string& aa)
{
	std::vector<std::string> result;
	boost::split(result, aa, boost::is_any_of("\n"));
	boost::reverse(result);
	return result;
}

std::string AndBIT::bottom_line(const std::string& aa)
{
	std::vector<std::string> aa_lines = reverse_split(aa);
	return aa_lines.front();
}

unsigned AndBIT::leading_spaces(const std::string& line)
{
	std::string left_trimmed_line = boost::trim_left_copy(line);
	return line.size() - left_trimmed_line.size();
}

std::string AndBIT::line_separator(const std::string& up_aa,
                                   const std::string& low_aa,
                                   const Handle& gsn,
                                   bool unordered_premises)
{
	// Calculate the leading space and line separator sizes. We assume
	// that low_aa has no leading space.
	size_t lead_sp_size = 0;                // Leading space size
	size_t line_sep_size = low_aa.size();	// Line separator size
	if (not up_aa.empty()) {
		std::string up_bl = bottom_line(up_aa);
		size_t up_bls = up_bl.size();
		lead_sp_size = leading_spaces(up_bl);
		line_sep_size = std::max(line_sep_size, up_bls - lead_sp_size);
	}

	// Get formula string
	std::string lang, lib, fun;
	LibraryManager::parse_schema(gsn->get_name(), lang, lib, fun);
	std::string formula_str = fun.substr(0, line_sep_size - 2);
	size_t formula_str_size = formula_str.size();

	// Overlay the formula string on top of the line
	std::string line_str;
	size_t offset = (line_sep_size - formula_str_size) / 2;
	for (size_t i = 0; i < line_sep_size; ++i)
		if (i < offset or offset + formula_str_size <= i)
			line_str.push_back(unordered_premises ? '=' : '-');
		else
			line_str.push_back(formula_str[i - offset]);

	// Append leading space in front of the line
	return std::string(lead_sp_size, ' ') + line_str;
}

/////////
// BIT //
/////////

BIT::BIT() : _as(nullptr) {}

BIT::BIT(AtomSpace& as,
         const Handle& target,
         const Handle& vardecl,
         const BITNodeFitness& fitness)
	: bit_as(&as), // child atomspace of as
	  _as(&as), _init_target(target), _init_vardecl(vardecl),
	  _init_fitness(fitness) {}

BIT::~BIT() {}

bool BIT::empty() const
{
	return andbits.empty();
}

size_t BIT::size() const
{
	return andbits.size();
}

AndBIT* BIT::init()
{
	andbits.emplace_back(bit_as, _init_target,
						 _init_vardecl, _init_fitness, _as);

	LAZY_URE_LOG_DEBUG << "Initialize BIT with:" << std::endl
					   << andbits.begin()->to_string();

	return &*andbits.begin();
}

AndBIT* BIT::expand(AndBIT& andbit, BITNode& bitleaf,
                    const RuleTypedSubstitutionPair& rule, double prob)
{
	// Make sure that the rule is not already an or-child of bitleaf.
	if (is_in(rule, bitleaf)) {
		ure_logger().debug() << "An equivalent rule has already expanded "
							 << "that BIT-node, abort expansion";
		return nullptr;
	}

	// Insert the rule as or-branch of this bitleaf
	bitleaf.rules.insert(rule);

	// Expand the and-BIT and insert it in the BIT, if the expansion
	// was successful
	AndBIT new_andbit = andbit.expand(bitleaf.body, rule, prob);
	return (bool)new_andbit.fcs ? insert(new_andbit) : nullptr;
}

AndBIT* BIT::insert(AndBIT& andbit)
{
	// Check that it isn't already in the BIT
	if (boost::find(andbits, andbit) != andbits.end()) {
		LAZY_URE_LOG_DEBUG << "The following and-BIT is already in the BIT: "
						   << andbit.fcs->id_to_string();
		return nullptr;
	}
	// Insert while keeping the order
	auto it = andbits.insert(boost::lower_bound(andbits, andbit), andbit);

	// Return andbit pointer
	return &*it;
}

void BIT::reset_exhausted_flags()
{
	for (AndBIT& andbit : andbits)
		andbit.reset_exhausted();
}

bool BIT::andbits_exhausted() const
{
	return boost::algorithm::all_of(andbits, [](const AndBIT& andbit) {
			return andbit.exhausted; });
}

bool BIT::is_in(const RuleTypedSubstitutionPair& rule,
                const BITNode& bitnode) const
{
	for (const auto& bnr : bitnode.rules)
		if (rule.first.is_alpha_equivalent(bnr.first))
			return true;
	return false;
}

std::string oc_to_string(const BITNode& bitnode, const std::string& indent)
{
	return bitnode.to_string(indent);
}

std::string oc_to_string(const AndBIT& andbit, const std::string& indent)
{
	return andbit.to_string(indent);
}

// std::string oc_to_string(const BIT& bit)
// {
//	return bit.to_string();
// }

} // ~namespace opencog
