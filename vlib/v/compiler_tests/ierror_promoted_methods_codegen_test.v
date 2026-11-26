import os
import v.tempname

fn test_interface_dispatch_uses_promoted_receivers() {
	root := os.join_path(os.vtmp_dir(), 'v3_promoted_receiver_dispatch_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct BaseErr {
	text string
}

fn helper_msg(s string) string {
	return s
}

fn (err BaseErr) msg() string {
	return helper_msg(err.text)
}

fn (err BaseErr) code() int {
	return 42
}

struct WrapErr {
	BaseErr
}

fn fail_value() !int {
	return WrapErr{
		BaseErr: BaseErr{
			text: 'promoted'
		}
	}
}

struct PtrErr {
	text string
}

fn (err &PtrErr) msg() string {
	return err.text
}

fn (err &PtrErr) code() int {
	return 7
}

struct PointerWrapErr {
	&PtrErr
}

fn fail_pointer() !int {
	return PointerWrapErr{
		PtrErr: &PtrErr{
			text: 'pointer'
		}
	}
}

struct InnerErr {
	text string
}

fn (err InnerErr) msg() string {
	return err.text
}

fn (err InnerErr) code() int {
	return 8
}

struct PointerInner {
	&InnerErr
}

struct NestedWrapErr {
	PointerInner
}

fn fail_nested() !int {
	return NestedWrapErr{
		PointerInner: PointerInner{
			InnerErr: &InnerErr{
				text: 'nested'
			}
		}
	}
}

struct FallbackBaseErr {
	text string
}

fn (err FallbackBaseErr) msg() string {
	return err.text
}

fn (err FallbackBaseErr) code() int {
	return 7
}

struct FallbackWrapErr {
	FallbackBaseErr
}

fn (err FallbackWrapErr) msg(extra int) string {
	return err.FallbackBaseErr.text + ':' + extra.str()
}

fn (err FallbackWrapErr) code(extra int) int {
	return extra
}

fn fail_fallback() !int {
	return FallbackWrapErr{
		FallbackBaseErr: FallbackBaseErr{
			text: 'embedded'
		}
	}
}

interface HasNumber {
	m() int
}

struct Inner {
	n int
}

fn (inner Inner) m() int {
	return inner.n
}

// Embedded structs must come first in a struct body.
struct Mid {
	Inner
	pad int
}

struct Outer {
	Mid
	tag int
}

fn use(item HasNumber) int {
	return item.m()
}

fn main() {
	fail_value() or {
		println(err.msg() + ':' + err.code().str())
	}
	fail_pointer() or {
		println(err.msg() + ':' + err.code().str())
	}
	fail_nested() or {
		println(err.msg() + ':' + err.code().str())
	}
	fail_fallback() or {
		println(err.msg() + ':' + err.code().str())
	}

	item := Outer{
		tag: 100
		Mid: Mid{
			pad: 200
			Inner: Inner{
				n: 42
			}
		}
	}
	println(int_str(use(item)))
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('BaseErr__msg(((main__WrapErr*)i->_object)->BaseErr)'), c_code
	assert c_code.contains('helper_msg'), c_code
	assert c_code.contains('PtrErr__msg(&(((main__PointerWrapErr*)i->_object)->PtrErr))'), c_code
	assert c_code.contains('InnerErr__msg((((main__NestedWrapErr*)i->_object)->PointerInner).InnerErr)'), c_code
	assert c_code.contains('FallbackBaseErr__msg(((main__FallbackWrapErr*)i->_object)->FallbackBaseErr)'), c_code
	assert c_code.contains('FallbackBaseErr__code(((main__FallbackWrapErr*)i->_object)->FallbackBaseErr)'), c_code
	assert !c_code.contains('return FallbackWrapErr__msg(*(main__FallbackWrapErr*)i->_object)'), c_code
	assert !c_code.contains('return FallbackWrapErr__code(*(main__FallbackWrapErr*)i->_object)'), c_code
	assert c_code.contains('Inner__m((((main__Outer*)i->_object)->Mid).Inner)'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'promoted:42\npointer:7\nnested:8\nembedded:7\n42', run.output
}

fn test_ierror_embed_compatible_method_dependencies_are_marked_used() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_embed_method_deps_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct GhostError {
	Error
	text string
}

fn ghost_msg_part(text string) string {
	return text
}

fn (err GhostError) msg() string {
	return ghost_msg_part(err.text)
}

fn fail() !int {
	return GhostError{
		text: 'ghost'
	}
}

fn main() {
	fail() or {
		println('ok')
		return
	}
	println('bad')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	// The assertions below inspect implementation bodies in monolithic C output.
	compile := os.exec([@VEXE, '-nocache', '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('GhostError__msg'), c_code
	assert c_code.contains('string ghost_msg_part('), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_unused_ierror_compatible_method_dependencies_are_marked_used_without_unpruning_dead_fns() {
	root := os.join_path(os.vtmp_dir(), 'v3_ierror_unused_method_deps_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	dead_dir := os.join_path(root, 'dead')
	os.mkdir(dead_dir)!
	os.write_file(os.join_path(dead_dir, 'dead.v'), "module dead

pub struct Helper {}

pub fn (h Helper) text() string {
	return 'dead'
}

pub struct DeadErr {}

pub fn (err DeadErr) msg() string {
	return Helper{}.text()
}

pub fn (err DeadErr) code() int {
	return 17
}

pub fn unrelated_dead_fn() string {
	return 'pruned'
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import dead

fn main() {
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	// The assertions below inspect implementation bodies in monolithic C output.
	compile := os.exec([@VEXE, '-nocache', root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('string dead__DeadErr__msg(dead__DeadErr err)'), c_code
	assert c_code.contains('string dead__Helper__text(dead__Helper h)'), c_code
	// An empty struct literal is `{E_STRUCT}` since empty structs are zero-sized.
	assert c_code.contains('return dead__Helper__text((dead__Helper){E_STRUCT});'), c_code
	assert !c_code.contains('dead__unrelated_dead_fn'), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
