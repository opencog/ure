/*
 * FCStat.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
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

#ifndef _OPENCOG_FCSTAT_H_
#define _OPENCOG_FCSTAT_H_

#include <map>

#include <opencog/atoms/base/Handle.h>
#include <opencog/ure/Rule.h>

namespace opencog {

struct InferenceRecord
{
	const Handle hsource;
	const Rule& rule;
	HandleSet product;

	InferenceRecord(Handle h, const Rule& r, const HandleSet& p)
		: hsource(h), rule(r), product(p) {}
};

class FCStat
{
public:
	FCStat(AtomSpace* trace_as) : _trace_as(trace_as) {}

	/**
	 * Record the inference step into memory, as well as in the
	 * atomspace according to the following format:
	 *
	 * ExecutionLink
	 *    <rule>
	 *    List
	 *      <step>
	 *      <source>
	 *    <product>
	 *
	 * where
	 *
	 * 1. <rule> is DefinedSchemaNode <rule-name>
	 * 2. <step> is NumberNode <#iteration>
	 * 3. <source> is the source
	 * 4. <product> is a SetLink <p1> ... <pn> where pi are the products
	 */
	void add_inference_record(unsigned iteration, Handle source,
	                          const Rule& rule, const HandleSet& product);
	HandleSet get_all_products() const;
	HandleSet get_all_products();

private:
	std::vector<InferenceRecord> _inf_rec;
	AtomSpace* _trace_as;

	// TODO: subdivide in smaller and shared mutexes
	mutable std::mutex _whole_mutex;
};

}

#endif /* _OPENCOG_FCSTAT_H_ */
