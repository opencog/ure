# Cython/distutils can only handle a single file as the source for
# a python module.  Since it is helpful to be able to split the binding
# code into separate files, we just include them here.
#
# Note that the ordering of include statements may influence whether
# things work or not

include "forwardchainer.pyx"
include "backwardchainer.pyx"