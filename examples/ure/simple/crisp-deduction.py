#!/usr/bin/env python

# Simple crisp deduction example, run the example in interactive mode
#
# python crisp-deduction.py
#
# then scroll to the bottom and run the commented-out either forward
# or backward chainer queries.
#
# Note that for now it uses the Scheme URE API. There is a ure python
# module, see https://wiki.opencog.org/w/Python#Backward_Chainer but
# it is still incomplete so for now this is the demonstrated way.

from opencog.atomspace import AtomSpace, TruthValue
from opencog.atomspace import types
from opencog.type_constructors import *
from opencog.scheme_wrapper import scheme_eval, scheme_eval_h

a = AtomSpace()
set_default_atomspace(a)

# Load scheme crisp deduction example
scheme_eval(a, "(load \"crisp-deduction.scm\")")

# Run forward chainer via scheme API
fc_result = scheme_eval_h(a, "(crisp-deduction-fc AB)")
print("fc_result =", fc_result)

# # Run backward chainer via scheme API (commented out forward chainer query first)
# bc_result = scheme_eval_h(a, "(crisp-deduction-bc (Implication (Predicate \"A\") (Predicate \"C\")))")
# print("bc_result =", bc_result)
