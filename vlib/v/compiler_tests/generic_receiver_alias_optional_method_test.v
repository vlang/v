import os
import v.cmdexec

// A method on a generic receiver declares no generic params of its own: `T` is
// introduced by the receiver type. Seeding the explicit args found nothing to
// bind, so the specialization was lost and the `?T` payload degraded to
// `unknown` -- emitted as `int`, which then failed to compile against a string.
const generic_receiver_alias_program = "type AliasedID = string

struct AliasBox[T] {
	items map[int]T
}

fn (b AliasBox[T]) get(key int) ?T {
	if key in b.items {
		return b.items[key]
	}
	return none
}

fn main() {
	box := AliasBox[AliasedID]{
		items: {
			1: AliasedID('one')
		}
	}
	got := box.get(1) or { panic('expected a value for key 1') }
	if box.get(2) != none {
		panic('expected no value for key 2')
	}
	println('got=\${got}')
}
"

fn test_optional_method_on_generic_receiver_keeps_alias_specialization() {
	os.find_abs_path_of_executable('cc') or {
		eprintln('skipping generic receiver alias test: cc is unavailable')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_generic_receiver_alias_${os.getpid()}')
	os.mkdir_all(root)!
	// The C error otherwise sends the build to the v1 fallback, which compiles
	// the program fine and hides the regression behind a successful build.
	saved_no_fallback := os.getenv_opt('V_MACOS_V3_NO_FALLBACK')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	defer {
		if saved := saved_no_fallback {
			os.setenv('V_MACOS_V3_NO_FALLBACK', saved, true)
		} else {
			os.unsetenv('V_MACOS_V3_NO_FALLBACK')
		}
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'program.v')
	os.write_file(source, generic_receiver_alias_program)!
	output := os.join_path(root, 'program')
	vexe := os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })
	build := cmdexec.run_with_timeout(vexe, ['-no-retry-compilation', '-cc', 'cc', '-o', output,
		source], 180_000)
	assert build.exit_code == 0, build.output
	run := cmdexec.run_with_timeout(output, [], 30_000)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'got=one', run.output
}
