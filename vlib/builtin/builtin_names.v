module builtin

// builtin_pub_fn_names lists all public builtin function names.
// This is used by tools like VLS to provide accurate completion suggestions.
// Keep this list in sync with the public functions defined across vlib/builtin/*.v.
pub const builtin_pub_fn_names = ['close', 'copy', 'eprintln', 'eprint', 'error',
	'error_with_code', 'exit', 'flush_stderr', 'flush_stdout', 'free', 'isnil', 'panic', 'print',
	'println']
