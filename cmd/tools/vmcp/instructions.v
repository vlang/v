// The model instructions this server hands an agent at initialization.
//
// gopls publishes the same thing behind a `-instructions` flag so it can be
// loaded into a session's context rather than trusted to be read. Publishing the
// text here means the workflow rules live in the repository, next to the tools
// they describe.
module main

// instructions returns the guidance an agent needs to use these tools well.
//
// It is deliberately short. The tools describe themselves; what an agent cannot
// discover on its own is the order to call them in, and which questions only one
// of them can answer.
pub fn instructions(ws &Workspace) string {
	return '# V language server\n' +
		'\n' +
		'This server is the V compiler itself. It reads and edits V code with the\n' +
		"compiler's own parser, so it answers about code that does not compile yet.\n" +
		'\n' +
		'## A working order\n' +
		'\n' +
		'1. `v_project_info` once per session: it names the module, the compiler and\n' +
		'   the workspace root every other path is relative to.\n' +
		'2. `v_symbols` to learn what a file declares. It is cheaper than reading the\n' +
		'   file and it already carries the doc comments.\n' +
		'3. `v_check` after every change. It is the only authoritative answer to\n' +
		'   "does it compile", and it returns records with file, line and column.\n' +
		'4. `v_test_run` for behaviour. `v_check` will not catch a wrong result.\n' +
		'\n' +
		'## Before guessing, ask\n' +
		'\n' +
		'- A stdlib signature: `v_stdlib_doc`. Do not invent a signature for an\n' +
		'  unfamiliar module.\n' +
		'- What a symbol is: `v_symbol_at`, then `v_references` for its callers.\n' +
		'- A language rule: the `v-lang` skill covers the parts agents get wrong,\n' +
		'  most of all compile-time code and the difference between `?T` and `!T`.\n' +
		'- What the code looked like: `v_ast`, in the same JSON shape as `v ast -p`.\n' +
		'\n' +
		'## Changing code\n' +
		'\n' +
		'- `v_rename_symbol` and `v_format` default to a dry run. Read the plan, then\n' +
		'  pass the flag that applies it.\n' +
		'- `v_edit_replace` requires `expected_old`: read the range first and pass it\n' +
		'  back verbatim. It refuses to write when the file no longer matches, so a\n' +
		'  concurrent change is reported instead of overwritten.\n' +
		'\n' +
		'## V rules that bite\n' +
		'\n' +
		'- A module name must match its directory name, or the import fails silently.\n' +
		'- Function arguments are immutable by default; add `mut` to change one.\n' +
		'- `$if`, `$for` and the other `$` forms are compile time. A runtime `if`\n' +
		'  that mentions a platform specific symbol does not compile.\n' +
		'- `?T` is an optional that can be none; `!T` is a result that can error.\n' +
		'  They are unwrapped with different syntax.\n' +
		'- Use `v fmt`, through `v_format`, on every file you touch.\n' +
		'\n' + workspace_note(ws)
}

// workspace_note describes the tree this particular server is pointed at, so a
// session started in the compiler's own checkout knows it is editing V itself.
fn workspace_note(ws &Workspace) string {
	mut note := '## This workspace\n' +
		'\n' +
		'- root: `' + ws.root + '`\n' +
		'- v.mod: ' + if ws.v_modified {
		'`' + ws.relative(ws.v_mod_file) +
			'` (' + ws.v_mod_name + ')'
	} else {
		'none found'
	} + '\n' +
		'- compiler: `' + ws.compiler + '`\n'
	note += '- read-only: ' + if ws.read_only {
		'yes, no tool may write a file'
	} else {
		'no'
	} + '\n'
	if ws.is_v_checkout {
		note += "\nThis is the V compiler's own source tree. Changes here affect the\n" +
			'compiler: rebuild with `./v self` after touching `vlib/v/` or `cmd/v/`.\n'
	}
	return note
}
