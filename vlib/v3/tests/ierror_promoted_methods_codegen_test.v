import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_ierror_promoted_methods_test_${os.getpid()}')
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -gc none -no-parallel -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_ierror_dispatch_uses_promoted_embedded_methods() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_promoted_methods_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
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

fn fail() !int {
	return WrapErr{
		BaseErr: BaseErr{
			text: 'promoted'
		}
	}
}

fn main() {
	fail() or {
		println(err.msg() + ':' + err.code().str())
		return
	}
	println('bad')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_promoted_methods_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('BaseErr__msg(((WrapErr*)i->_object)->BaseErr)'), c_code
	assert c_code.contains('helper_msg'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'promoted:42'
}

fn test_ierror_dispatch_uses_promoted_embedded_pointer_methods() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_promoted_pointer_methods_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct PtrErr {
	text string
}

fn (err &PtrErr) msg() string {
	return err.text
}

fn (err &PtrErr) code() int {
	return 7
}

struct PointerWrapErr {
	PtrErr &PtrErr
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
	InnerErr &InnerErr
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

fn main() {
	fail_pointer() or {
		assert err.msg() == 'pointer'
		assert err.code() == 7
	}
	fail_nested() or {
		assert err.msg() == 'nested'
		assert err.code() == 8
	}
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_promoted_pointer_methods_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('PtrErr__msg(((PointerWrapErr*)i->_object)->PtrErr)'), c_code
	assert c_code.contains('InnerErr__msg(*((((NestedWrapErr*)i->_object)->PointerInner).InnerErr))'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_interface_dispatch_uses_nested_promoted_receiver_path() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_interface_nested_promoted_receiver_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	src := os.join_path(root, 'main.v')
	os.write_file(src, 'interface HasNumber {
	m() int
}

struct Inner {
	n int
}

fn (inner Inner) m() int {
	return inner.n
}

struct Mid {
	pad int
	Inner
}

struct Outer {
	tag int
	Mid
}

fn use(item HasNumber) int {
	return item.m()
}

fn main() {
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
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_interface_nested_promoted_receiver_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('Inner__m((((Outer*)i->_object)->Mid).Inner)'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
}

fn test_ierror_embed_compatible_method_dependencies_are_marked_used() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_embed_method_deps_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
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

	bin := os.join_path(os.temp_dir(), 'v3_ierror_embed_method_deps_out_${os.getpid()}')
	// The assertions below inspect implementation bodies in monolithic C output.
	compile := os.execute('${v3_bin} -nocache ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('GhostError__msg'), c_code
	assert c_code.contains('string ghost_msg_part('), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_unused_ierror_compatible_method_dependencies_are_marked_used_without_unpruning_dead_fns() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_unused_method_deps_${os.getpid()}')
	dead_dir := os.join_path(root, 'dead')
	os.mkdir_all(dead_dir) or { panic(err) }
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

	bin := os.join_path(os.temp_dir(), 'v3_ierror_unused_method_deps_out_${os.getpid()}')
	// The assertions below inspect implementation bodies in monolithic C output.
	compile := os.execute('${v3_bin} -nocache ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('string dead__DeadErr__msg(dead__DeadErr err)'), c_code
	assert c_code.contains('string dead__Helper__text(dead__Helper h)'), c_code
	assert c_code.contains('return dead__Helper__text((dead__Helper){});'), c_code
	assert !c_code.contains('dead__unrelated_dead_fn'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_ierror_direct_bad_signature_falls_back_to_promoted_method() {
	v3_bin := build_v3()

	root := os.join_path(os.temp_dir(), 'v3_ierror_bad_direct_promoted_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "struct BaseErr {
	text string
}

fn (err BaseErr) msg() string {
	return err.text
}

fn (err BaseErr) code() int {
	return 7
}

struct WrapErr {
	BaseErr
}

fn (err WrapErr) msg(extra int) string {
	return err.BaseErr.text + ':' + extra.str()
}

fn (err WrapErr) code(extra int) int {
	return extra
}

fn fail() !int {
	return WrapErr{
		BaseErr: BaseErr{
			text: 'embedded'
		}
	}
}

fn main() {
	fail() or {
		println(err.msg() + ':' + err.code().str())
		return
	}
	println('bad')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_ierror_bad_direct_promoted_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file('${bin}.c') or { '' }
	assert c_code.contains('BaseErr__msg(((WrapErr*)i->_object)->BaseErr)'), c_code
	assert c_code.contains('BaseErr__code(((WrapErr*)i->_object)->BaseErr)'), c_code
	assert !c_code.contains('return WrapErr__msg(*(WrapErr*)i->_object)'), c_code
	assert !c_code.contains('return WrapErr__code(*(WrapErr*)i->_object)'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'embedded:7'
}
