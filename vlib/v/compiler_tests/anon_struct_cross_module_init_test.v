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
	// `-new-compiler` keeps V from retrying a rejected program on the V 0.5.2
	// fallback, which would otherwise answer for it: every compile error stages a
	// retry, and the fallback's own diagnostics are what the assertions would end
	// up reading.
	res := os.execute('${os.quoted_path(vexe)} -new-compiler -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	return res.exit_code == 0, res.output
}

// naming_generated_types_is_rejected builds `mod_source` once to read back the names
// the compiler made up for its anonymous fields, then tries to name each of them from
// another module. Both steps share one directory because the generated names encode
// the module's source path, so a name read from one temporary tree does not exist in
// another. Reading them is also the point: knowing the name is exactly what an attempt
// to name such a type needs.
fn naming_generated_types_is_rejected(mod_source string, reader_source string) (int, []string) {
	dir := os.join_path(os.vtmp_dir(), 'v3_anon_generated_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'holder')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'holder', 'holder.v'), mod_source) or { panic(err) }
	src := os.join_path(dir, 'm.v')
	os.write_file(src, reader_source) or { panic(err) }
	c_out := os.join_path(dir, 'm.c')
	gen := os.execute('${os.quoted_path(vexe)} -new-compiler -o ${os.quoted_path(c_out)} ${os.quoted_path(src)}')
	if gen.exit_code != 0 {
		return 0, ['generating C for the reader failed: ${gen.output}']
	}
	text := os.read_file(c_out) or { return 0, ['the generated C could not be read'] }
	mut names := []string{}
	mut rest := text
	for {
		at := rest.index('holder__AnonStruct_') or { break }
		rest = rest[at + 'holder__'.len..].clone()
		mut end := 0
		for end < rest.len && (rest[end].is_alnum() || rest[end] == `_`) {
			end++
		}
		candidate := 'holder.' + rest[..end].clone()
		if candidate !in names {
			names << candidate
		}
		rest = rest[end..].clone()
	}
	mut accepted := []string{}
	exe := os.join_path(dir, 'm.exe')
	for generated in names {
		attempt := 'module main\n\nimport holder\n\nfn main() {\n\ts := ' + generated +
			'{}\n\tprintln(s)\n}\n'
		os.write_file(src, attempt) or { panic(err) }
		res := os.execute('${os.quoted_path(vexe)} -new-compiler -o ${os.quoted_path(exe)} ${os.quoted_path(src)}')
		if res.exit_code == 0 || !res.output.contains('declared as private to module `holder`') {
			accepted << '${generated}: ${res.output}'
		}
	}
	return names.len, accepted
}

const holder_module = "module holder

// Hidden has no `pub`, so only `holder` may name it.
struct Hidden {
pub mut:
	x int
}

// AnonStruct_Secret is named the way the compiler names the aggregates it
// synthesizes, but it is an ordinary private declaration and stays one.
struct AnonStruct_Secret {
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

// The exemption belongs to declarations the compiler synthesized, not to a name
// that looks like one. Nothing stops a module from declaring `AnonStruct_Secret`
// itself, and such a declaration is as private as it was written.
fn test_a_private_struct_named_like_a_synthesized_one_is_still_rejected() {
	ok, output := compiles('named_like_anon', holder_module, "module main

import holder

fn main() {
	s := holder.AnonStruct_Secret{
		x: 1
	}
	println(s.x)
}
")
	assert !ok, 'a private struct of another module was accepted'
	assert output.contains('declared as private to module `holder`'), output
}

// The exemption belongs to the literal, not to the declaration. Publishing the
// synthesized declarations instead would let another module name one of them
// outright - the generated names are deterministic, valid identifiers - and so reach
// an anonymous type that only a private field exposes.
fn test_naming_a_generated_anonymous_type_outright_is_rejected() {
	private_anon_holder := "module holder

// The anonymous type of `secret` is reachable only through a private field.
struct Hidden {
pub mut:
	secret struct {
	pub mut:
		token string
	}
}

pub struct Visible {
pub mut:
	cfg struct {
	pub mut:
		on bool
	}
}

pub fn make() Visible {
	return Visible{}
}
"
	reader := "module main

import holder

fn main() {
	v := holder.make()
	println(v.cfg.on)
}
"
	found, accepted := naming_generated_types_is_rejected(private_anon_holder, reader)
	// Otherwise the case this test is about would not arise.
	assert found > 0, 'no generated anonymous type names were found in the output'
	assert accepted.len == 0, 'generated anonymous types were nameable from another module: ${accepted}'
}

// Adopting the expected type for a bare `struct { ... }` literal is the other way into
// a private declaration. The adoption used to accept any expected type whose name
// started with `AnonStruct_`, so a public function taking a private one could be
// called with a literal from another module - reaching a type the caller could not
// have named - and the privacy check was then skipped because the literal's own
// generated name is a contextual one.
fn test_a_literal_cannot_stand_in_for_a_private_type_named_like_a_synthesized_one() {
	ok, output := compiles('adopt_private', "module holder

// Named the way the compiler names what it synthesizes, but an ordinary private type.
struct AnonStruct_Secret {
pub mut:
	x int
}

pub fn consume(s AnonStruct_Secret) int {
	return s.x
}
", "module main

import holder

fn main() {
	println(holder.consume(struct {
		x: 42
	}))
}
")
	assert !ok, 'a literal stood in for another module\'s private type'
	assert output.contains('holder.AnonStruct_Secret'), output
}

// The genuine case still has to work: a parameter whose type really is an anonymous
// struct the parser synthesized takes a literal from another module.
fn test_a_literal_still_fills_a_genuinely_anonymous_parameter() {
	ok, output := compiles('adopt_anon', "module holder

pub fn consume(s struct {
	x int
}) int {
	return s.x
}
", "module main

import holder

fn main() {
	println(holder.consume(struct {
		x: 42
	}))
}
")
	assert ok, output
}
