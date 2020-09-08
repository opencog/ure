from ure cimport ure_logger as c_ure_logger
from opencog.logger cimport wrap_clogger, cLogger

def ure_logger():
    z = wrap_clogger(&c_ure_logger())
    return z
