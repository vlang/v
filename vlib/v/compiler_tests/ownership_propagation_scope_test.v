import os
import v.cmdexec

fn test_propagation_defers_preserve_lexical_destructor_order() {
	root := os.join_path(os.vtmp_dir(), 'ownership_propagation_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@DIR, 'testdata', 'ownership_propagation_scope.vv'), source)!
	expected := 'body 5\ndrop 5\nbody 1\ndrop 1\ntake 2\ndrop 2\nbody 3\ndrop 3\nresult-defer\nresult-defer\nresult-failed\nbody 4\ndrop 4\noption-defer\noption-defer\noption-failed\npanic-defer'
	for mode in ['-autofree', '-ownership'] {
		result := cmdexec.run_with_timeout(@VEXE, [mode, '-gc', 'none', '-no-parallel', '-o',
			os.join_path(root, mode.trim_left('-')), 'run', source], 180_000)
		assert result.exit_code == 0, '${mode}: ${result.output}'
		assert result.output.trim_space() == expected, '${mode}: ${result.output}'
	}
}

// Inspect generated cleanup independently of the existing aliased-Option C layout limitation.
fn test_aliased_option_propagation_keeps_cleanup_after_local_declaration() {
	root := os.join_path(os.vtmp_dir(), 'ownership_alias_propagation_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, r"module main
type Maybe = ?int
struct Resource implements Drop { id int }
fn (mut r Resource) drop() { println('drop ${r.id}') }
fn (mut r Resource) free() { r.drop() }
fn maybe() ?int { return 1 }
fn alias_option_path(ok bool) Maybe {
 defer { unsafe { println('alias-option-defer') } }
 _ := maybe()?
 if ok { local := Resource{id: 7}; println(local.id) }
 return 1
}
fn main() { _ := alias_option_path(true) or { 0 } }
")!
	output := os.join_path(root, 'main.c')
	for mode in ['-autofree', '-ownership'] {
		result := cmdexec.run(@VEXE, [mode, '-gc', 'none', '-no-parallel', '-o', output, source])
		assert result.exit_code == 0, result.output
		code := os.read_file(output)!
		definitions := code.split_into_lines().filter(it.contains('alias_option_path(') && it.trim_space().ends_with('{'))
		assert definitions.len == 1, '${definitions}'
		start := code.index(definitions[0]) or { panic('missing alias function') }
		end := code[start..].index('\n}\n') or { panic('missing function end') }
		body := code[start..start + end]
		declaration := body.index(' local =') or { panic('missing local declaration') }
		cleanup := body.index('&local') or {
			body.index('&(local)') or { panic('missing local cleanup') }
		}
		assert cleanup > declaration, body
	}
}
