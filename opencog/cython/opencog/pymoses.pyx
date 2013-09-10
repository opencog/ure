__author__ = 'cosmo'

'''
Wrapper for MOSES. Uses the C++ moses_exec function to access MOSES functionality.

run:
The run method returns a collection of candidates as MosesCandidate objects.
Each candidate containing a score, program, and program_type.
Enumeration of program_type: python, combo.
For python, there is an eval method that will evaluate the model on new data.

run_manually:
The run_manually method invokes MOSES without any extra integration. Useful for using it non-programatically via stdout.

Options for use:
1. Within the CogServer, from an embedded MindAgent
2. Within the CogServer, from the interactive python shell
3. In your Python IDE or interpreter. You need to ensure that your path includes '{PROJECT_BINARY_DIR}/opencog/cython'

Loading the module:

from opencog.pymoses import moses
moses = moses()

Example usage of run:

Example #1: XOR example with Python output and Python input

input_data = [[0,0,0],[1,1,0],[1,0,1],[0,1,1]]
output = moses.run(input = input_data, python = True)
model = output[0].eval
model([0,1])  # Returns: True
model([1,1])  # Returns: False

Example #2: Run the majority demo problem, return only one candidate, and use Python output
output = moses.run(args = "-H maj -c 1", python = True)
model = output[0]
model.eval([0,1,0,1,0])  # Returns: False
model.eval([1,1,0,1,0])  # Returns: True

Example #3: Load the XOR input data from a file, return only one candidate, and use Combo output

output = moses.run(args = "-i /path/to/input.txt -c 1")
combo_program = output[0].program
print combo_program  # Prints: and(or(!$1 !$2) or($1 $2))

Example usage of run_manually:

moses.run_manually("--version")
moses.run_manually("-i input.txt -o output.txt")

@todo Implement an option to use a Python function as the MOSES scoring function
'''

from libc.stdlib cimport malloc, free
import shlex
import tempfile
import csv

class MosesException(Exception):
    pass

class MosesCandidate(object):
    def __init__(self, score = None, program = None, program_type = None):
        self.score = score
        self.program = program
        self.program_type = program_type

    def eval(self, arglist):
        if self.program_type != "python":
            raise MosesException('Error: eval method is only defined for candidates with program_type of python.')
        if len(arglist) == 0:
            raise MosesException('Error: eval method requires a list of input values.')

        namespace = {}
        exec self.program in namespace
        return namespace.get('moses_eval')(arglist)

cdef class moses:
    def run(self, input = None, args = "", python = False):
        # Create temporary files for sending input/output to the moses_exec function
        if input is not None:
            input_file = tempfile.NamedTemporaryFile()

            input_file_builder = csv.writer(input_file, delimiter = ',')
            input_file_builder.writerows(input)

            input_file.flush()

        output_file = tempfile.NamedTemporaryFile()

        # Process the argument list for moses_exec
        _args_list = []

        if input is not None:
            _args_list.extend(['-i', input_file.name])
        if python:
            _args_list.extend(['--python', '1'])

        _args_list.extend(['-o', output_file.name])
        _args_list.extend(shlex.split(args))

        #@todo
        print "Debug: args_list = "
        print _args_list
        self._run_args_list(_args_list)

        # Process the output file
        output = output_file.file.read()

        #Candidate = namedtuple("Candidate", "score program program_type")
        candidates = []

        if len(output) == 0:
            raise MosesException('Error: No output file was obtained from MOSES. Check to make sure the input file and arguments provided were valid.')

        # Python output
        # @todo fix iostream_combo.h, so that it doesn't add the extra comma at the end of the python program, on line 69, perhaps by adding a check to see if number_of_children > 1
        elif output.splitlines()[0].startswith('#!/usr/bin/python'): # @todo modify the python output parser to not put a blank line at the beginning
            # The Python header is declared in opencog/learning/moses/moses/types.h (ostream_combo_tree_composite_pbscore_python)
            python_header = "#!/usr/bin/python"

            output_list = [python_header + "\n" + element for element in output.split(python_header)[1:]]

            for candidate in output_list: #output.split("#!/usr/bin/python")[1:]:
                program = candidate #.splitlines()
                program = program.rpartition(',')[0] # @todo fix opencog/comboreduct/combo/iostream_combo.h (ostream_combo_it) to remove the unneeded trailing comma that is inserted by the Python formatter
                candidates.append(MosesCandidate(score = None, program = program, program_type = "python")) #candidates.append(Candidate(score = None, program = program, program_type = "python")) # @todo add score for python programs

        # Combo output
        else:
            output_list = [element[:-1] for element in output.splitlines()]

            for candidate in output_list:
                score = candidate.partition(' ')[0]
                program = candidate.partition(' ')[2]
                candidates.append(MosesCandidate(score = score, program = program, program_type = "combo")) # Candidate(score = score, program = program, program_type = "combo"))

        return candidates

    def run_manually(self, args=""):
        self._run_args_list(shlex.split(args))

    def _run_args_list(self, args_list):
        args_list.insert(0, "moses")
        cdef char **c_argv
        args_list = [bytes(x) for x in args_list]
        c_argv = <char**>malloc(sizeof(char*) * len(args_list))
        for idx, s in enumerate(args_list):
            c_argv[idx] = s
        try:
            moses_exec(len(args_list), c_argv)
        except RuntimeError, ex:
            print "Exception occurred when calling MOSES:\n" + ex.message
            # @todo raise error
        finally:
            free(c_argv)