The current directory creates a library used to split compound
german words into individual parts. The library uses a cache
containing a summary of the german source dictionary with suffix
and connector information. The splitting uses a backtracking
algorithm using a longest search first strategy.

The CreateTestData subdirectory contains an application that can
parse a german text document into individual words, and check those
words against a cached dictionary - containing a more full list of
german source words than used above. Any unfound words are sent to
an output file and are candidates for compound words.

The Test subdirectory contains a test program for the halfnoun library.

