import os
import v.cmdexec
import v.tempname

fn test_lvalue_receiver_stabilization_finishes_after_one_redispatch() {
	root := os.join_path(os.vtmp_dir(), 'v3_lvalue_stabilization_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_lvalue_stabilization.v')
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
	bin := os.join_path(root, 'v3_lvalue_stabilization')
	compile := cmdexec.run_in_merged_with_env(@VEXE, ['-no-parallel', src, '-b', 'c', '-o', bin], '', {
		'VFLAGS': ''
	})
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '55\n55', run.output
}
