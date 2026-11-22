import os

// A by-value parameter stays on the stack when a method is only called through
// a pointer stored in one of its fields, or through a pointer local read from
// it: the call receives that pointer's value, never the parameter's address.
const pointer_receiver_escape_source = 'struct PointerReceiverNode {
mut:
	count int
}

fn (node &PointerReceiverNode) element() PointerReceiverElement {
	mut element := PointerReceiverElement{}
	element.frame[0] = node.count
	return element
}

fn (mut node PointerReceiverNode) bump(by int) ! {
	node.count += by
}

struct PointerReceiverElement {
	node &PointerReceiverNode = unsafe { nil }
mut:
	frame [64]int
}

@[unsafe]
fn pointer_receiver_collect(element PointerReceiverElement) []PointerReceiverElement {
	mut out := []PointerReceiverElement{}
	if element.node != unsafe { nil } {
		out << element.node.element()
	}
	return out
}

@[unsafe]
fn pointer_receiver_record(element PointerReceiverElement) ! {
	if element.node != unsafe { nil } {
		mut node := element.node
		node.bump(element.frame[0])!
	}
}

fn main() {
	mut node := &PointerReceiverNode{
		count: 2
	}
	mut element := PointerReceiverElement{
		node: node
	}
	element.frame[0] = 2
	pointer_receiver_record(element) or { panic(err) }
	println(pointer_receiver_collect(element)[0].frame[0])
}
'

fn test_pointer_receiver_calls_keep_by_value_parameters_on_the_stack() {
	dir := os.join_path(os.vtmp_dir(), 'pointer_receiver_param_escape_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, pointer_receiver_escape_source)!
	c_path := os.join_path(dir, 'main.c')
	build := os.exec([@VEXE, '-b', 'c', '-o', c_path, source])
	assert build.exit_code == 0, build.output
	c_source := os.read_file(c_path)!
	assert c_source.contains('pointer_receiver_collect(main__PointerReceiverElement element)'), c_source.all_after('pointer_receiver_collect(').all_before(')')
	assert c_source.contains('pointer_receiver_record(main__PointerReceiverElement element)'), c_source.all_after('pointer_receiver_record(').all_before(')')
	assert !c_source.contains('__heap_value_param')
	binary := os.join_path(dir, 'main')
	run := os.exec([@VEXE, '-b', 'c', '-o', binary, 'run', source])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '4'
}
