/*
 * FCStat.cc
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

#include "FCStat.h"
#include <opencog/atoms/core/NumberNode.h>

using namespace opencog;

void FCStat::add_inference_record(unsigned iteration, Handle source,
                                  const Rule& rule,
                                  const HandleSet& product)
{
	{
		std::lock_guard<std::mutex> lock(_whole_mutex);
		_inf_rec.emplace_back(source, rule, product);
	}

	if (_trace_as and not product.empty()) {
		Handle schema = rule.get_alias();
		Handle i = _trace_as->add_node(NUMBER_NODE, std::to_string(iteration + 1));
		Handle inputs = _trace_as->add_link(LIST_LINK, source, i);
		for (const Handle& output : product) {
			_trace_as->add_link(EXECUTION_LINK, schema, inputs, output);
		}
	}
}

HandleSet FCStat::get_all_products() const
{
	std::lock_guard<std::mutex> lock(_whole_mutex);
	HandleSet all;
	for(const auto& ir : _inf_rec)
		all.insert(ir.product.begin(),ir.product.end());

	return all;
}
