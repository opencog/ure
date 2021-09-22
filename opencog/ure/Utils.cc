/*
 * Utils.cc
 *
 * Copyright (C) 2014 OpenCog Foundation
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

#include "Utils.h"

namespace opencog {

/// Return true if all of h was removed.
bool remove_hypergraph(AtomSpace& as, const Handle& h)
{
	// Recursive case 
	if (h->is_link()) {
		HandleSeq oset = h->getOutgoingSet();
		bool success = as.extract_atom(h);
		if (success) {
			// Return true only if entire subgraph was removed.
			for (const Handle& oh : oset)
				if (not remove_hypergraph(as, oh))
					success = false;
		}
		return success;
	}
	// Base case
	else { 
		return as.extract_atom(h);
	}
}

} // ~namespace opencog
