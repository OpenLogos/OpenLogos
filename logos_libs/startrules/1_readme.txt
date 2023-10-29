This directory creates a library to execute start rules
for transl during the lookup processes.

Each rule consists of a regular expression to home in on a type of expression.
The consequent of the rule will calculate a suitable type of token and the
start and end position of the matched text.

There is a factory class that reads the rules from a data file.

The following files are no longer used in this version:
        AtSymbolAction.h/cpp,
        EqualsSymbolAction.h/cpp,
        MathSymbolAction.h/cpp,
        UnfoundAgentAction.h/cpp


The test directory contains a test application to excercise
this library. It uses data in the test/data subdirectory.


