import os

const lvalue_stabilization_vexe = @VEXE
const lvalue_stabilization_tests_dir = os.dir(@FILE)
const lvalue_stabilization_v3_dir = os.dir(lvalue_stabilization_tests_dir)
const lvalue_stabilization_vlib_dir = os.dir(lvalue_stabilization_v3_dir)
const lvalue_stabilization_v3_src = os.join_path(lvalue_stabilization_v3_dir, 'v3.v')

fn test_lvalue_receiver_stabilization_finishes_after_one_redispatch() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_lvalue_stabilization_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${os.quoted_path(lvalue_stabilization_vexe)} -old-compiler -gc none -no-parallel -path "${lvalue_stabilization_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(lvalue_stabilization_v3_src)}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_lvalue_stabilization_${pid}.v')
	os.write_file(src, 'module main

struct First {}

struct Second {}

type Node = First | Second

struct Item {
mut:
	value int
}

fn (mut item Item) add(value int) {
	item.value += value
}

struct Holder {
mut:
	item Item
}

fn update(node Node) int {
	mut holder := Holder{
		item: Item{
			value: 40
		}
	}
	holder.item.add(10 + (match node {
		First { 5 }
		Second { 6 }
	}))
	return holder.item.value
}

fn update_index(node Node) int {
	mut items := [Item{
		value: 40
	}]
	index := 0
	items[index].add(10 + (match node {
		First { 5 }
		Second { 6 }
	}))
	return items[0].value
}

fn main() {
	println(update(First{}))
	println(update_index(First{}))
}
') or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_lvalue_stabilization_${pid}')
	compile := os.execute('${os.quoted_path(v3_bin)} -no-parallel ${os.quoted_path(src)} -b c -o ${os.quoted_path(bin)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(bin))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '55\n55', run.output
}
