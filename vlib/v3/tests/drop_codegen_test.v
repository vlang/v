import os

const drop_codegen_vexe = @VEXE
const drop_codegen_tests_dir = os.dir(@FILE)
const drop_codegen_v3_dir = os.dir(drop_codegen_tests_dir)
const drop_codegen_vlib_dir = os.dir(drop_codegen_v3_dir)
const drop_codegen_v3_src = os.join_path(drop_codegen_v3_dir, 'v3.v')

fn test_drop_codegen_covers_generic_natural_return_and_propagation_exits() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_drop_codegen_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_drop_codegen_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_drop_codegen_program_${pid}')
	os.rm(v3_bin) or {}
	os.rm(src) or {}
	os.rm(out) or {}
	os.rm(out + '.c') or {}
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${drop_codegen_vexe} -gc none -d ownership -path "${drop_codegen_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${drop_codegen_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(src, "module main

struct Resource implements Drop {
	id int
}

fn (mut r Resource) drop() {
	println('drop \${r.id}')
}

struct Box[T] implements Drop {
	id int
	value T
}

fn (mut b Box[T]) drop() {
	println('box \${b.id}')
}

fn early() int {
	r := Resource{1}
	return r.id
}

fn moved() Resource {
	r := Resource{2}
	return r
}

fn nested() {
	{
		a := Resource{3}
		b := Box[int]{id: 4, value: a.id}
		println(b.value)
	}
	println('nested end')
}

fn fail() ! {
	return error('failed')
}

fn propagated() ! {
	r := Resource{5}
	fail()!
	println(r.id)
}

fn explicit_error() ! {
	r := Resource{11}
	return error('explicit')
}

fn explicit_none() ?int {
	r := Resource{12}
	return none
}

fn maybe_number() ?int {
	return none
}

fn direct_optional_forward() ?int {
	r := Resource{13}
	return maybe_number()
}

fn if_else_branch_drop(cond bool) {
	if cond {
		println('then branch')
	} else {
		r := Resource{14}
		println('else branch')
	}
}

fn value_if_branch_drop(cond bool) int {
	n := if cond {
		r := Resource{15}
		r.id
	} else {
		0
	}
	return n
}

fn typed_return_if_drop(cond bool, id int) int {
	r := Resource{id}
	return if cond { 31 } else { 32 }
}

fn value_or_branch_drop() int {
	n := maybe_number() or {
		r := Resource{16}
		r.id
	}
	return n
}

fn maybe_small_number() ?int {
	return 20
}

fn maybe_small_number_none() ?int {
	return none
}

fn converted_optional_success() ?i64 {
	r := Resource{19}
	return maybe_small_number()
}

fn converted_optional_failure() ?i64 {
	r := Resource{32}
	return maybe_small_number_none()
}

fn small_pair() (i8, i16) {
	return i8(33), i16(34)
}

fn forwarded_multi_return_drop() (int, int) {
	r := Resource{31}
	return small_pair()
}

fn implicit_fn_exit_param_and_local(p Resource) {
	r := Resource{17}
	println('implicit ${p.id}:${r.id}')
}

fn labelled_continue_drop_once() {
	mut i := 0
	outer: for i < 1 {
		outer_r := Resource{20}
		i++
		for {
			inner_r := Resource{21}
			continue outer
			println(inner_r.id)
		}
		println(outer_r.id)
	}
}

fn maybe_resource() ?Resource {
	return Resource{22}
}

fn maybe_resource_id(id int) ?Resource {
	return Resource{id}
}

fn if_guard_binding_drop() {
	if r := maybe_resource() {
		println('guard ${r.id}')
	}
}

fn match_branch_drop(n int) {
	match n {
		0 {
			r := Resource{23}
			println('match \${r.id}')
		}
		else {}
	}
}

fn select_branch_drop() {
	ch := chan int{cap: 1}
	ch <- 24
	select {
		value := <-ch {
			r := Resource{value}
			println('select \${r.id}')
		}
	}
}

fn optional_wrapper_local_drop() {
	x := maybe_resource_id(25)
	if x != none {
		println('optional wrapper')
	}
}

fn c_for_init_normal_drop() {
	mut keep_going := true
	for r := Resource{26}; keep_going; keep_going = false {
		println('for init ${r.id}')
	}
}

fn c_for_init_break_drop() {
	for r := Resource{27}; true; {
		println('for break ${r.id}')
		break
	}
}

fn c_for_init_labelled_break_drop() {
	outer: for r := Resource{28}; true; {
		for {
			println('for labelled break ${r.id}')
			break outer
		}
	}
}

fn loop_exits() {
	for {
		r := Resource{6}
		break
		println(r.id)
	}
	mut i := 0
	for i < 1 {
		r := Resource{7}
		i++
		continue
		println(r.id)
	}
	outer: for {
		r := Resource{8}
		for {
			inner := Resource{9}
			break outer
			println(inner.id)
		}
		println(r.id)
	}
	for _ in 0 .. 1 {
		r := Resource{10}
		println(r.id)
	}
}

fn main() {
	println(early())
	nested()
	r := moved()
	println(r.id)
	propagated() or { println(err.msg()) }
	explicit_error() or { println(err.msg()) }
	explicit_none() or { println('none') }
	direct_optional_forward() or { println('forward none') }
	if_else_branch_drop(false)
	println(value_if_branch_drop(true))
	println(typed_return_if_drop(true, 29))
	println(typed_return_if_drop(false, 30))
	println(value_or_branch_drop())
	implicit_fn_exit_param_and_local(Resource{18})
	println(converted_optional_success() or { i64(-1) })
	converted_optional_failure() or { println('converted none') }
	a, b := forwarded_multi_return_drop()
	println('${a}:${b}')
	labelled_continue_drop_once()
	if_guard_binding_drop()
	match_branch_drop(0)
	select_branch_drop()
	optional_wrapper_local_drop()
	c_for_init_normal_drop()
	c_for_init_break_drop()
	c_for_init_labelled_break_drop()
	loop_exits()
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output == 'drop 1\n1\n3\nbox 4\ndrop 3\nnested end\n2\ndrop 5\nfailed\ndrop 11\nexplicit\ndrop 12\nnone\ndrop 13\nforward none\nelse branch\ndrop 14\ndrop 15\n15\ndrop 29\n31\ndrop 30\n32\ndrop 16\n16\nimplicit 18:17\ndrop 17\ndrop 18\ndrop 19\n20\ndrop 32\nconverted none\ndrop 31\n33:34\ndrop 21\ndrop 20\nguard 22\ndrop 22\nmatch 23\ndrop 23\nselect 24\ndrop 24\noptional wrapper\ndrop 25\nfor init 26\ndrop 26\nfor break 27\ndrop 27\nfor labelled break 28\ndrop 28\ndrop 6\ndrop 7\ndrop 9\ndrop 8\n10\ndrop 10\ndrop 2\n', run.output
}
