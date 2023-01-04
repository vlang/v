## Run the tests here with: `v vlib/v/tests/known_errors/known_errors_test.v`

The intended use of this, is for providing samples, that currently do NOT compile,
but that a future compiler improvement WILL be able to compile, and to track,
whether they were not fixed incidentally, due to an unrelated change/improvement.
For example, code that triggers generating invalid C code can go here,
and later when a bug is fixed, can be moved to a proper _test.v or .vv/.out pair, 
outside of the vlib/v/tests/known_errors/testdata/ folder.
