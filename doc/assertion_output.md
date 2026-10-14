# Assertion and test output

Failed assertions and failed test error propagation write their reports to standard error.
Generated reports flush standard output before each diagnostic line and standard error afterwards.
When both streams share a pipe or file, earlier standard output appears before the report,
including when standard error is fully buffered on Windows.
Assertions marked `@[assert_continues]` keep the same output order when execution continues.
