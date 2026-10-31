Use `exec_args(args)` for literal program arguments. A leading `v` selects the
compiler from this checkout; leading `NAME=value` elements set the child environment.
`exec_args_with_progress(args, outputs)` also checkpoints successful outputs.

The command string versions are deprecated. When a task requires shell syntax,
invoke the shell explicitly and pass dynamic values as quoted positional parameters.
