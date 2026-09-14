// An anonymous struct carries no `pub` of its own and cannot be named from
// anywhere: it is reachable only through the field that declares it, so that
// field's declaration already decided who may see it. The checker used to look it
// up among the private declarations of the module that happens to hold it, and
// reject initializing it from another module:
//
//	error: struct `cli.AnonStruct__..._command_x2e_v_2` was declared as private to
//	module `cli`, so it can not be used inside module `main`
//
// `cli.Command.defaults` is such a field on a public struct, which is what broke
// `v quest`.
import os

const vexe = @VEXE

// compiles reports whether `main_source` builds against a local module holding
// `mod_source`, returning the compiler output so a failure names the error rather
// than just the exit code.
fn compiles(name string, mod_source string, main_source string) (bool, string) {
	dir := os.join_path(os.vtmp_dir(), 'v3_anon_struct_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'holder')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'holder', 'holder.v'), mod_source) or { panic(err) }
	src := os.join_path(dir, 'm.v')
	os.write_file(src, main_source) or { panic(err) }
	out := os.join_path(dir, 'm.exe')
	res := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	return res.exit_code == 0, res.output
}

const holder_module = "module holder

// Hidden has no `pub`, so only `holder` may name it.
struct Hidden {
pub mut:
	x int
}

pub struct Visible {
pub mut:
	x   int
	cfg struct {
	pub mut:
		on bool
	}
}
"

fn test_anonymous_struct_field_of_a_public_struct_is_initializable() {
	ok, output := compiles('public', holder_module, "module main

import holder

fn main() {
	v := holder.Visible{
		x:   1
		cfg: struct {
			on: true
		}
	}
	println(v.cfg.on)
}
")
	assert ok, output
	assert !output.contains('declared as private'), output
}

// The exemption is for anonymous structs only: a named struct without `pub` stays
// private to the module that declares it.
fn test_a_named_private_struct_is_still_rejected() {
	ok, output := compiles('private', holder_module, "module main

import holder

fn main() {
	h := holder.Hidden{
		x: 1
	}
	println(h.x)
}
")
	assert !ok, 'a private struct of another module was accepted'
	assert output.contains('declared as private to module `holder`'), output
}
