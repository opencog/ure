/*
 * Rule.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Authors: Misgana Bayetta <misgana.bayetta@gmail.com> 2015
 *          Nil Geisweiller 2015-2016
 *          Shujing Ke 2018
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

#include <queue>

#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/algorithm/cxx11/any_of.hpp>

#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/TypeUtils.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/util/oc_assert.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/unify/Unify.h>

#include "URELogger.h"

#include "Rule.h"

namespace opencog {

HandleSet RuleSet::aliases() const
{
	HandleSet aliases;
	for (const auto& rule : *this)
		aliases.insert(rule.get_alias());
	return aliases;
}

std::string RuleSet::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "size = " << size() << std::endl;
	size_t i = 0;
	for (const Rule& rule : *this)
		ss << indent << "rule[" << i++ << "]:" << std::endl
		   << oc_to_string(rule, indent + OC_TO_STRING_INDENT) << std::endl;
	return ss.str();
}

std::string RuleSet::to_short_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "size = " << size();
	size_t i = 0;
	for (const auto& rule : *this)
		ss << std::endl << indent << "rule[" << i++ << "]:" << std::endl
		   << rule.to_short_string(indent + oc_to_string_indent);
	return ss.str();
}

Rule::Rule()
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED) {}

Rule::Rule(const Handle& rule_member)
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED)
{
	init(rule_member);
}

Rule::Rule(const Handle& rule_alias, const Handle& rbs)
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED)
{
	init(rule_alias, rbs);
}

Rule::Rule(const Handle& rule_alias, const Handle& rule, const Handle& rbs)
	: premises_as_clauses(false), _rule_alias(Handle::UNDEFINED)
{
	init(rule_alias, rule, rbs);
}

void Rule::init(const Handle& rule_member)
{
	OC_ASSERT(rule_member != Handle::UNDEFINED);
	if (not nameserver().isA(rule_member->get_type(), MEMBER_LINK))
		throw InvalidParamException(TRACE_INFO,
		                            "Rule '%s' is expected to be a MemberLink",
		                            rule_member->to_string().c_str());

	Handle rule_alias = rule_member->getOutgoingAtom(0);
	Handle rbs = rule_member->getOutgoingAtom(1);
	init(rule_alias, rbs);
}

void Rule::init(const Handle& rule_alias, const Handle& rbs)
{
	Handle rule = DefineLink::get_definition(rule_alias);
	init(rule_alias, rule, rbs);
}

void Rule::init(const Handle& rule_alias, const Handle& rule, const Handle& rbs)
{
	OC_ASSERT(rule->get_type() == BIND_LINK);
	_rule = BindLinkCast(rule);

	_rule_alias = rule_alias;
	_name = _rule_alias->get_name();
	_rbs = rbs;
	AtomSpace& as = *rule_alias->getAtomSpace();
	Handle ml = as.get_link(MEMBER_LINK, rule_alias, rbs);
	_tv = ml->getTruthValue();

    verify_rule();
}

bool Rule::verify_rule()
{
    // currently do not verify meta rules
    if (is_meta())
        return true;

    Handle rewrite = _rule->get_implicand()[0]; // assume only one rewrite
    Type rewrite_type = rewrite->get_type();

    // check 1: If there are multiple conclusions
    if ((rewrite_type == AND_LINK) || (rewrite_type == LIST_LINK))
    {
        logger().warn() << "\nRule::verify_rule: " << _rule_alias->get_name()
                        << " contains multiple conclusions.\n"
                        << "This rule will not work in backwardchainer.\n"
                        << "All the conclusions should be wrapped with an ExecutionOutPutLink.\n"
                        << "Please check /atomspace/examples/ure/DummyExecutionOutput.scm for example."
                        << std::endl;
        return false;
    }

    return true;
}

bool Rule::operator==(const Rule& r) const
{
	return content_eq(Handle(_rule), Handle(r._rule));
}

bool Rule::operator<(const Rule& r) const
{
	return content_based_handle_less()(Handle(_rule), Handle(r._rule));
}

bool Rule::is_alpha_equivalent(const Rule& r) const
{
	return _rule->is_equal(Handle(r._rule));
}

TruthValuePtr Rule::get_tv() const
{
	return _tv;
}

void Rule::set_name(const std::string& name)
{
	_name = name;
}

std::string& Rule::get_name()
{
	return _name;
}

const std::string& Rule::get_name() const
{
	return _name;
}

void Rule::set_rule(const Handle& h)
{
	_rule = BindLinkCast(h);
}

Handle Rule::get_rule() const
{
	return Handle(_rule);
}

Handle Rule::get_alias() const
{
	return _rule_alias;
}

Handle Rule::get_definition() const
{
	return DefineLink::get_definition(_rule_alias);
}

Handle Rule::get_rbs() const
{
	return _rbs;
}

void Rule::add(AtomSpace& as)
{
	if (!_rule)
		return;

	// The BindLink of the rule itself is not added to atomspace in
	// order to avoid alpha-converting it if an equivalent rule already
	// exist. Indeed such a rule has been previously alpha-converted to
	// avoid variable name collisions with the inference tree it is
	// going to expand, and adding it to the atomspace might undo that
	// alpha-conversion.
	//
	// A workaround be to alpha-convert right before inference tree
	// expansion. However since alpha-conversion has already taken
	// place during unification (see Rule::unify_source or
	// Rule::unify_target) we avoid re-doing the alpha-conversion that
	// way.
	_rule = createBindLink(std::move(HandleSeq(_rule->getOutgoingSet())));
}

Handle Rule::get_vardecl() const
{
	// Generate the VarDecl from Variables.
	// This is needed in the case that a BindLink doesn't have a VarDecl
	if (_rule)
		return _rule->get_variables().get_vardecl();
	return Handle::UNDEFINED;
}

const Variables& Rule::get_variables() const
{
	if (_rule)
		return _rule->get_variables();
	static Variables empty_variables;
	return empty_variables;
}

/**
 * Get the implicant (input) of the rule defined in a BindLink.
 *
 * @return the Handle of the implicant
 */
Handle Rule::get_implicant() const
{
	if (_rule)
		return _rule->get_body();
	return Handle::UNDEFINED;
}

Handle Rule::get_implicand() const
{
	if (_rule)
		return _rule->get_implicand()[0];  // assume that there is only one.
	return Handle::UNDEFINED;
}

bool Rule::is_valid() const
{
	return (bool)_rule;
}

bool Rule::is_meta() const
{
	Handle implicand = get_implicand();

	if (not implicand)  // XXX this check is never needed !?
		return false;

	Type itype = implicand->get_type();
	//Subrule might be quoted
	if (Quotation::is_quotation_type(itype))
	{
		implicand = implicand->getOutgoingAtom(0);
		itype = implicand->get_type();
	}
	//Check if implicand is a subrule
	if (itype == BIND_LINK)
		return true;

	//Check if implicand is a subrule that requires substitution
	auto schema = "scm: cog-substitute";
	if (itype == EXECUTION_OUTPUT_LINK)
		return NodeCast(implicand->getOutgoingAtom(0))->get_name() == schema;

	return false;
}

bool Rule::has_cycle() const
{
	// Return true iff at least one premise is equal to the conclusion
	const Handle c = get_conclusion();
	for (const Handle& h : get_premises())
		if (content_eq(c, h))
			return true;
	return false;
}

HandleSeq Rule::get_clauses() const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return HandleSeq();

	Handle implicant = get_implicant();
	Type t = implicant->get_type();
	HandleSeq hs;

	if (t == AND_LINK or t == OR_LINK) {
		const HandleSeq& oset = implicant->getOutgoingSet();
		// if there is PresentLink then only return clauses under the
		// PresentLink(s), as the other clauses can be assumed to be
		// virtual.
		auto is_present =
			[](const Handle& h) { return h->get_type() == PRESENT_LINK; };
		bool has_prsnt_lnk = boost::algorithm::any_of(oset, is_present);
		if (has_prsnt_lnk) {
			for (const Handle& h : implicant->getOutgoingSet()) {
				if (is_present(h)) {
					hs.insert(hs.end(),
					          h->getOutgoingSet().begin(), h->getOutgoingSet().end());
				}
			}
		} else {
			hs = implicant->getOutgoingSet();
		}
	} else if (t == PRESENT_LINK) {
		hs = implicant->getOutgoingSet();
	} else {
		hs.push_back(implicant);
	}

	return hs;
}


Handle Rule::filter_quote(Handle h) const
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

HandleSeq Rule::get_premises() const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return HandleSeq();

	Handle rewrite = _rule->get_implicand()[0];  // assume there is only one.

	if (is_meta())
		rewrite = ExtractSubruleRewrite(rewrite);

	Type rewrite_type = rewrite->get_type();

	// If not an ExecutionOutputLink then return the clauses
	if (premises_as_clauses || rewrite_type != EXECUTION_OUTPUT_LINK)
		return get_clauses();

	// Otherwise search the premises in the rewrite term's ExecutionOutputLink
	HandleSeq premises;
	if (rewrite_type == EXECUTION_OUTPUT_LINK) {
		Handle args = rewrite->getOutgoingAtom(1);
		if (args->get_type() == LIST_LINK) {
			OC_ASSERT(args->get_arity() > 0);
			for (Arity i = 1; i < args->get_arity(); i++) {
				Handle argi = args->getOutgoingAtom(i);
				// Return unordered premises
				if (argi->get_type() == SET_LINK) {
					for (Arity j = 0; j < argi->get_arity(); j++)
						premises.push_back(argi->getOutgoingAtom(j));
				}
				// Return ordered premise
				else {
					premises.push_back(argi);
				}
			}
		}
	}
	return premises;
}

Handle Rule::ExtractSubruleRewrite(Handle rewrite) const
{
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

	rewrite = BindLinkCast(rewrite)->get_implicand();
	rewrite = filter_quote(rewrite);
	return rewrite;
}

Handle Rule::get_conclusion() const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return Handle::UNDEFINED;

	Handle rewrite = _rule->get_implicand()[0];  // assume there is only one.
	//Handle Meta Rule
	if (is_meta())
		rewrite = ExtractSubruleRewrite(rewrite);

	Type rewrite_type = rewrite->get_type();

	// If not an ExecutionOutputLink then return the rewrite term
	if (rewrite_type != EXECUTION_OUTPUT_LINK)
		return rewrite;

	Handle args = rewrite->getOutgoingAtom(1);
	if (args->get_type() == LIST_LINK) {
		OC_ASSERT(args->get_arity() > 0);
		return args->getOutgoingAtom(0);
	} else {
		return args;
	}

}

HandlePairSeq Rule::get_conclusions() const
{
	HandlePairSeq results;

	// If the rule's handle has not been set yet
	if (not is_valid())
		return HandlePairSeq();

	Handle vardecl = get_vardecl();
	for (const Handle& c : get_conclusion_patterns())
		results.push_back({filter_vardecl(vardecl, c), c});

	return results;
}

RuleTypedSubstitutionMap Rule::unify_source(const Handle& source,
                                            const Handle& vardecl,
                                            const AtomSpace* queried_as) const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return {};

	// To guarantee that the rule variable does not have the same name
	// as any variable in the source. XXX This is only a stochastic
	// guarantee, there is a small chance that the new random name
	// will still collide.
	Rule alpha_rule = rand_alpha_converted();

	RuleTypedSubstitutionMap unified_rules;
	Handle rule_vardecl = alpha_rule.get_vardecl();

	for (const Handle& premise : alpha_rule.get_premises())
	{
		Unify unify(source, premise, vardecl, rule_vardecl);
		Unify::SolutionSet sol = unify();
		if (sol.is_satisfiable()) {
			Unify::TypedSubstitutions tss =
				unify.typed_substitutions(sol, source);
			// For each typed substitution produce a new rule by
			// substituting all variables by their associated
			// values.
			for (const auto& ts : tss) {
				Rule substituted = alpha_rule.substituted(ts, vardecl,
														  queried_as);
				unified_rules.insert({substituted, ts});
			}
		}
	}
	return unified_rules;
}

RuleTypedSubstitutionMap Rule::unify_target(const Handle& target,
                                            const Handle& vardecl,
                                            const AtomSpace* queried_as) const
{
	// If the rule's handle has not been set yet
	if (not is_valid())
		return {};

	// To guarantee that the rule variable does not have the same name
	// as any variable in the target. XXX This is only a stochastic
	// guarantee, there is a small chance that the new random name
	// will still collide.
	Rule alpha_rule = rand_alpha_converted();

	RuleTypedSubstitutionMap unified_rules;
	Handle alpha_vardecl = alpha_rule.get_vardecl();
	for (const Handle& alpha_pat : alpha_rule.get_conclusion_patterns())
	{
		Unify unify(target, alpha_pat, vardecl, alpha_vardecl);
		Unify::SolutionSet sol = unify();
		if (sol.is_satisfiable()) {
			Unify::TypedSubstitutions tss =
				unify.typed_substitutions(sol, target);
			// For each typed substitution produce a new rule by
			// substituting all variables by their associated
			// values.
			for (const auto& ts : tss) {
				Rule substituted = alpha_rule.substituted(ts, vardecl,
														  queried_as);
				unified_rules.insert({substituted, ts});
			}
		}
	}

	return unified_rules;
}

RuleSet Rule::strip_typed_substitution(const RuleTypedSubstitutionMap& rules)
{
	RuleSet rs;
	for (const auto& r : rules)
		rs.insert(r.first);

	return rs;
}

Handle Rule::apply(AtomSpace& as) const
{
	return HandleCast(_rule->execute(&as));
}

std::string Rule::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "name: " << _name << std::endl
	   << indent << "tv: " << _tv->to_string() << std::endl
	   << indent << "rule:" << std::endl
	   << _rule->to_string(indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string Rule::to_short_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << get_name() << " " << get_rule()->id_to_string();
	return ss.str();
}

Rule Rule::rand_alpha_converted() const
{
	// Clone the rule
	Rule result = *this;

	// Alpha convert the rule
	result.set_rule(_rule->alpha_convert());

	return result;
}

HandleSeq Rule::get_conclusion_patterns() const
{
	HandleSeq results;
	Handle implicand = get_implicand();
	if (is_meta())
		implicand = ExtractSubruleRewrite(implicand);
	Type t = implicand->get_type();
	if (LIST_LINK == t)
		for (const Handle& h : implicand->getOutgoingSet())
			results.push_back(get_conclusion_pattern(h));
	else
		results.push_back(get_conclusion_pattern(implicand));

	return results;
}

Handle Rule::get_conclusion_pattern(const Handle& h) const
{
	Type t = h->get_type();
	if (EXECUTION_OUTPUT_LINK == t)
		return get_execution_output_first_argument(h);
	else
		return h;
}

Handle Rule::get_execution_output_first_argument(const Handle& h) const
{
	OC_ASSERT(h->get_type() == EXECUTION_OUTPUT_LINK);
	Handle args = h->getOutgoingAtom(1);
	if (args->get_type() == LIST_LINK) {
		OC_ASSERT(args->get_arity() > 0);
		return args->getOutgoingAtom(0);
	} else
		return args;
}

Rule Rule::substituted(const Unify::TypedSubstitution& ts,
                       const Handle& vardecl,
                       const AtomSpace* queried_as) const
{
	Rule new_rule(*this);

	Handle qvdecl;
	Handle qrvdecl;
	if (vardecl)
		qvdecl = createLink(QUOTE_LINK, vardecl);
	else
		qvdecl = createLink(QUOTE_LINK, createLink(VARIABLE_LIST));

	if (is_meta())
	{
		//Take Apart the Rule
		Handle vardecl = get_vardecl();
		Handle body = get_implicant();
		Handle subrule = get_implicand();

		//Take Apart the SubRule
		bool quoted = false;
		if (subrule->get_type() == QUOTE_LINK)
		{
			subrule = subrule->getOutgoingAtom(0);
			quoted = true;
		}
		BindLinkPtr subrulebl = BindLinkCast(subrule);
		Handle subvardecl = subrulebl->get_vardecl();
		Handle subbody = subrulebl->get_body();
		Handle subrewite = subrulebl->get_implicand();

		//Get the SubRule VarDecl required for Unification
		if (subvardecl)
			qrvdecl = createLink(QUOTE_LINK, subvardecl);
		else
			qrvdecl = createLink(QUOTE_LINK, createLink(VARIABLE_LIST));

		//Create the New Rule Body
		HandleSeq out = body->getOutgoingSet();
		HandleMap m = strip_context(ts.first);
		for (auto elem : m)
		{
			Handle n = createNode(GROUNDED_PREDICATE_NODE, "scm: cog-unify");
			Handle q = createLink(QUOTE_LINK, elem.second);
			Handle l = createLink(LIST_LINK, elem.first, q, qrvdecl, qvdecl);
			Handle h = createLink(EVALUATION_LINK, n, l);
			out.push_back(h);
		}
		body = createLink(out,body->get_type());

		//Create the new Rule Rewrite
		//If we have a subrule with substitution
		if (subrewite->get_type() == EXECUTION_OUTPUT_LINK)
		{
			Handle pred = subrewite->getOutgoingAtom(0);
			Handle params = subrewite->getOutgoingAtom(1);
			Type paramstype = params->get_type();
			bool paramsquoted = false;
			if (Quotation::is_quotation_type(paramstype))
			{
				params = params->getOutgoingAtom(0);
				paramsquoted = true;
			}
			if (params->is_link())
			{
				HandleSeq oset = params->getOutgoingSet();
				Variables vars;
				vars.find_variables(oset[0]);
				oset[0] = vars.substitute(oset[0],m);

				if (oset[0]->get_type() == QUOTE_LINK)
					oset[0] = filter_quote(oset[0]->getOutgoingAtom(0));

				params = createLink(oset,params->get_type());
			}
			else
			{
				params = m.begin()->second;
			}
			if (paramsquoted)
				params = createLink(paramstype,params);
			subrewite = createLink(EXECUTION_OUTPUT_LINK,pred,params);
		}
		else
		{
			//Do we need to check this? A Meta Rule with a non substituing
			//subrule seems weird
			throw RuntimeException(TRACE_INFO,"Rule.cc:729 Not implemnted.");
		}

		subrule = createLink(BIND_LINK, subvardecl,subbody,subrewite);

		if (quoted)
		{
			subrule = createLink(DONT_EXEC_LINK,subrule);
			subrule = createLink(QUOTE_LINK,subrule);
		}

		//We need to substitute the Variable in the SubRule Pattern
		//to restricts the source/target properly
		for (auto elem : m)
		{
			if (elem.first == elem.second)
				continue;
			Handle q = createLink(QUOTE_LINK, elem.second);
			Handle formula = createNode(GROUNDED_SCHEMA_NODE,
										"scm: cog-substitute");
			HandleSeq args = HandleSeq({subrule, elem.first, q, qrvdecl, qvdecl});
			Handle params = createLink(args, LIST_LINK);
			subrule = createLink(EXECUTION_OUTPUT_LINK, formula, params);
		}

		new_rule.set_rule(createLink(BIND_LINK, vardecl, body, subrule));
	}
	else
		new_rule.set_rule(Unify::substitute(_rule, ts, queried_as));
	return new_rule;
}

std::string oc_to_string(const Rule& rule, const std::string& indent)
{
	return rule.to_string(indent);
}
std::string oc_to_string(const RuleSet& rules, const std::string& indent)
{
	return rules.to_string(indent);
}
std::string oc_to_string(const RuleTypedSubstitutionPair& rule_ts,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "rule:" << std::endl
	   << oc_to_string(rule_ts.first, indent + OC_TO_STRING_INDENT)
	   << std::endl;
	ss << indent << "typed substitutions:" << std::endl
	   << oc_to_string(rule_ts.second, indent + OC_TO_STRING_INDENT)
	   << std::endl;
	return ss.str();
}
std::string oc_to_string(const RuleTypedSubstitutionMap& rules,
                         const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << rules.size() << std::endl;
	size_t i = 0;
	for (const RuleTypedSubstitutionPair& rule : rules)
		ss << indent << "rule[" << i++ << "]:" << std::endl
		   << oc_to_string(rule, indent + OC_TO_STRING_INDENT)
		   << std::endl;
	return ss.str();
}

} // ~namespace opencog
