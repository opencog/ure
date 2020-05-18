from opencog.atomspace import types
from cython.operator cimport dereference as deref, preincrement as inc
from opencog.atomspace cimport cHandle, Atom, AtomSpace, TruthValue
from ure cimport cForwardChainer

# Create a Cython extension type which holds a C++ instance
# as an attribute and create a bunch of forwarding methods
# Python extension type.


cdef class ForwardChainer:
    cdef cForwardChainer * chainer
    cdef AtomSpace _as
    cdef AtomSpace _trace_as
    def __cinit__(self, AtomSpace _as,
                  Atom rbs,
                  Atom source,
                  Atom vardecl=None,
                  AtomSpace trace_as=None,
                  focus_set=[]):
        cdef cHandle c_vardecl
        if vardecl is None:
            c_vardecl = c_vardecl.UNDEFINED
        else:
            c_vardecl = deref(vardecl.handle)

        cdef vector[cHandle] handle_vector
        for atom in focus_set:
            if isinstance(atom, Atom):
                handle_vector.push_back(deref((<Atom>(atom)).handle))
        cdef AtomSpace rbs_as = rbs.atomspace
        self.chainer = new cForwardChainer(deref(_as.atomspace),
                                        deref(rbs_as.atomspace),
                                        deref(rbs.handle),
                                        deref(source.handle),
                                        c_vardecl,
                                        <cAtomSpace*> (NULL if trace_as is None else trace_as.atomspace),
                                        handle_vector)
        self._as = _as
        self._trace_as = trace_as

    def do_chain(self):
        return self.chainer.do_chain()

    def get_results(self):
        cdef cHandle res_handle = self.chainer.get_results()
        cdef Atom result = Atom.createAtom(res_handle)
        return result

    def __dealloc__(self):
        del self.chainer
        self._trace_as = None
        self._as = None
