from ure cimport ure_logger as c_ure_logger
from opencog.logger cimport wrap_clogger, cLogger

def ure_logger():
    cdef cLogger cl = c_ure_logger()
    z= wrap_clogger(&cl)
    return z
