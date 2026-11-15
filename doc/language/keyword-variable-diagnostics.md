# Keywords used as variable names

A variable declaration beginning with `go`, `spawn`, `match`, or `fn` is rejected at that keyword.
For example, `go := 1` reports "cannot use keyword `go` as a variable name"
on the declaration line.
The diagnostic highlights the keyword rather than an unrelated token later in the function.

Use another variable name. The declaration does not change the meaning of the keyword.
