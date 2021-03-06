"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

optionals = [
    "type_inference/positive/fixed_point_iteration01.impala",
    "type_inference/positive/hard2.impala",
    "type_inference/positive/map.impala",
    "type_inference/positive/type_args_prefix03.impala",
    "type_inference/positive/type_args10.impala",
    "type_inference/positive/type_args11.impala",
    "type_inference/positive/type_args12.impala",
    "type_inference/positive/type_args13.impala",
    "type_inference/positive/type_args14.impala",
    "type_inference/positive/type_args4.impala",
    "type_inference/positive/type_args5.impala",
    "type_inference/positive/type_args6.impala",
    "type_inference/positive/type_args9.impala",
]

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_tests("type_inference/positive", True, ["--emit-annotated"])
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests

