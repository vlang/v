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
	loop_exits()
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output == 'drop 1\n1\n3\nbox 4\ndrop 3\nnested end\n2\ndrop 5\nfailed\ndrop 11\nexplicit\ndrop 12\nnone\ndrop 6\ndrop 7\ndrop 9\ndrop 8\n10\ndrop 10\ndrop 2\n', run.output
}
