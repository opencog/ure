/*
 * opencog/attentionbank/ImportanceIndex.cc
 *
 * Copyright (C) 2008-2011 OpenCog Foundation
 * All Rights Reserved
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

#include <algorithm>
#include <boost/range/adaptor/reversed.hpp>

#include <opencog/util/functional.h>
#include <opencog/util/Config.h>

#include <opencog/attentionbank/AVUtils.h>
#include <opencog/attentionbank/ImportanceIndex.h>

using namespace opencog;

/**
 * This formula is used to calculate the GROUP_NUM given the GROUP_SIZE
 * 32768 = (sum 2^b , b = 0 to (GROUP_NUM-1)) * GROUP_SIZE + GROUP_SIZE
 * for a GROUP_SIZE of 8 we get:
 * 32768 = (sum 2^b , b = 0 to 11) * 8 + 8
 * This means we have 12 groups with 8 bins each
 * The range of each groups bins is double the previous (2^b) and starts at 1
 * We have to add 8 since [2^c - 1 = sum 2^b , b = 0 to (c-1)] and we have 8
 * such groups
 */
#define GROUP_SIZE 8
#define GROUP_NUM 12
#define IMPORTANCE_INDEX_SIZE (GROUP_NUM*GROUP_SIZE)+GROUP_NUM //104

// ==============================================================

ImportanceIndex::ImportanceIndex()
    : _index(IMPORTANCE_INDEX_SIZE+1)
{
    minAFSize = config().get_int("ECAN_MIN_AF_SIZE", 100);
}

unsigned int ImportanceIndex::importanceBin(short importance)
{
    if (importance < 0)
        return 0;
    if (importance < 2*GROUP_SIZE)
        return importance;

    short imp = std::ceil((importance - GROUP_SIZE) / GROUP_SIZE);

    int sum = 0;
    int i;
    for (i = 0; i <= GROUP_NUM; i++)
    {
        if (sum >= imp)
            break;
        sum = sum + std::pow(2,i);
    }

    int ad = GROUP_SIZE - std::ceil(importance / std::pow(2,(i-1)));

    unsigned int bin = ((i * GROUP_SIZE) - ad);
    if (bin > 104)
        std::cout << "ibin: "<< bin << "\n";
    assert(bin <= IMPORTANCE_INDEX_SIZE);
    return bin;
}

void ImportanceIndex::updateImportance(const Handle& h, int oldbin, int newbin)
{
    if (oldbin == newbin) return;

    Atom* atom = h.operator->();
    _index.remove(oldbin, atom);
    _index.insert(newbin, atom);
    updateTopStiValues(atom);
}

// ==============================================================

void ImportanceIndex::removeAtom(const Handle& h)
{
    AttentionValuePtr oldav = get_av(h);
    set_av(h, nullptr);

    int bin = ImportanceIndex::importanceBin(oldav->getSTI());

    _index.remove(bin, h.operator->());

    std::lock_guard<std::mutex> lock(_mtx);
    // Also remove from topKSTIValueHandles vector
    auto it = std::find_if(
            topKSTIValuedHandles.begin(),
            topKSTIValuedHandles.end(),
            [&h](const HandleSTIPair& p) {return p.first == h; });
    if (it != topKSTIValuedHandles.end()) topKSTIValuedHandles.erase(it);
    //TODO Find the next highest STI valued atom to replace the removed one.
}

// ==============================================================

void ImportanceIndex::updateTopStiValues(Atom* atom)
{
    std::lock_guard<std::mutex> lock(_mtx);

    auto insertHandle = [this](Handle h){
        // delete if this handle is already in the vector. TODO find efficient
        // way of doing this.
        auto it = std::find_if(topKSTIValuedHandles.begin(), topKSTIValuedHandles.end()
                ,[&h](HandleSTIPair p){return p.first == h; });
        if(it != topKSTIValuedHandles.end()) topKSTIValuedHandles.erase(it);

        HandleSTIPair p(h, get_sti(h));
        it = std::lower_bound(topKSTIValuedHandles.begin(),
                topKSTIValuedHandles.end(), p,
                [=](const HandleSTIPair& hsti1, const HandleSTIPair& hsti2){
                return hsti1.second < hsti2.second;
                });
        topKSTIValuedHandles.insert(it, HandleSTIPair(h, get_sti(h)));
    };

    Handle h = atom->getHandle();
    AttentionValue::sti_t sti = get_sti(h);
    if(static_cast<int>(topKSTIValuedHandles.size()) < minAFSize){
        insertHandle(atom->getHandle());
    } else if (topKSTIValuedHandles.begin()->second < sti) {
        topKSTIValuedHandles.erase(topKSTIValuedHandles.begin());
        insertHandle(h);
    }
}

// ==============================================================

void ImportanceIndex::update(void)
{
    // Update MinMax STI values
    AttentionValue::sti_t minSTISeen = 0;
    UnorderedHandleSet minbin = getMinBinContents();
    auto minit = std::min_element(minbin.begin(), minbin.end(),
            [&](const Handle& h1, const Handle& h2) {
                return get_sti(h1) < get_sti(h2);
            });
    if (minit != minbin.end()) minSTISeen = get_sti(*minit);

    AttentionValue::sti_t maxSTISeen = 0;
    UnorderedHandleSet maxbin = getMinBinContents();
    auto maxit = std::max_element(maxbin.begin(), maxbin.end(),
            [&](const Handle& h1, const Handle& h2) {
                return get_sti(h1) < get_sti(h2);
            });
    if (maxit != maxbin.end()) maxSTISeen = get_sti(*maxit);

    if (minSTISeen > maxSTISeen)
        minSTISeen = maxSTISeen;

    updateMinSTI(minSTISeen);
    updateMaxSTI(maxSTISeen);
}

// ==============================================================

AttentionValue::sti_t ImportanceIndex::getMaxSTI(bool average) const
{
    std::lock_guard<std::mutex> lock(_mtx);
    if (average) {
        return (AttentionValue::sti_t) _maxSTI.recent;
    } else {
        return _maxSTI.val;
    }
}

AttentionValue::sti_t ImportanceIndex::getMinSTI(bool average) const
{
    std::lock_guard<std::mutex> lock(_mtx);
    if (average) {
        return (AttentionValue::sti_t) _minSTI.recent;
    } else {
        return _minSTI.val;
    }
}

// ==============================================================

UnorderedHandleSet ImportanceIndex::getHandleSet(
        AttentionValue::sti_t lowerBound,
        AttentionValue::sti_t upperBound) const
{
    UnorderedHandleSet ret;

    if (lowerBound < 0 || upperBound < 0)
        return ret;

    // The indexes for the lower bound and upper bound lists is returned.
    unsigned int lowerBin = importanceBin(lowerBound);
    unsigned int upperBin = importanceBin(upperBound);

    // Build a list of atoms whose importance is equal to the lower bound.
    // For the lower bound and upper bound index, the list is filtered,
    // because there may be atoms that have the same importanceIndex
    // and whose importance is lower than lowerBound or bigger than
    // upperBound.
    std::function<bool(Atom *)> pred =
        [&](Atom* atom)->bool {
            AttentionValue::sti_t sti = get_sti(atom->getHandle());
            return (lowerBound <= sti and sti <= upperBound);
        };

    AtomSet set;
    _index.getContentIf(lowerBin, inserter(set), pred);

    // If both lower and upper bounds are in the same bin,
    // Then we are done.
    if (lowerBin == upperBin) {
        std::transform(set.begin(), set.end(), inserter(ret),
                       [](Atom* atom)->Handle { return atom->getHandle(); });
        return ret;
    }

    // For every index within lowerBound and upperBound,
    // add to the list.
    while (++lowerBin < upperBin)
        _index.getContent(lowerBin, inserter(set));

    // The two lists are concatenated.
    _index.getContentIf(upperBin, inserter(set), pred);

    std::transform(set.begin(), set.end(), inserter(ret),
                   [](Atom* atom)->Handle { return atom->getHandle(); });
    return ret;
}

UnorderedHandleSet ImportanceIndex::getMaxBinContents()
{
    AtomSet set;
    UnorderedHandleSet ret;
    for (int i = IMPORTANCE_INDEX_SIZE ; i >= 0 ; i--)
    {
        if (_index.size(i) > 0)
        {
            _index.getContent(i, inserter(set));
            std::transform(set.begin(), set.end(), inserter(ret),
                   [](Atom* atom)->Handle { return atom->getHandle(); });
            return ret;
        }
    }
    return ret;
}

UnorderedHandleSet ImportanceIndex::getMinBinContents()
{
    AtomSet set;
    UnorderedHandleSet ret;
    for (int i = IMPORTANCE_INDEX_SIZE ; i >= 0 ; i--)
    {
        if (_index.size(i) > 0)
        {
            _index.getContent(i, inserter(set));
            std::transform(set.begin(), set.end(), inserter(ret),
                   [](Atom* atom)->Handle { return atom->getHandle(); });
            return ret;
        }
    }
    return ret;
}

HandleSeq ImportanceIndex::getTopSTIValuedHandles()
{
    std::lock_guard<std::mutex> lock(_mtx);
    HandleSeq hseq;
    for (const HandleSTIPair& p : topKSTIValuedHandles)
        hseq.push_back(p.first);
    return  hseq;
}

size_t ImportanceIndex::bin_size() const
{
    return _index.bin_size();
}

size_t ImportanceIndex::size(int i) const
{
    return _index.size(i);
}
