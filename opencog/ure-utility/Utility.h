
#ifndef _OPENCOG_URE_UTILITY_H_
#define _OPENCOG_URE_UTILITY_H_

#include <opencog/atoms/base/Node.h>

namespace opencog
{

inline bool is_meta(const BindLinkPtr h_bl)
{
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

inline bool is_meta(const Handle& h)
{
	return is_meta(BindLinkCast(h));
}

//Used to remove 1 layer of Unquote Links after removing a parent QuoteLink
inline Handle filter_quote(Handle h)
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

}

#endif // _OPENCOG_URE_UTILITY_H_
