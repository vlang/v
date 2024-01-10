module ast

// This file contains definitions that are specific to the native backend,
// but also have to be known by previous stages too, like the parser/checker etc.
// Please keep it as small/simple as possible, in order to not burden the *other* backends.

pub const native_builtins = ['assert', 'print', 'eprint', 'println', 'eprintln', 'exit', 'C.syscall']
