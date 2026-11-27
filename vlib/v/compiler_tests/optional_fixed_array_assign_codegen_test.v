import os
import v.tempname

fn test_optional_array_and_map_codegen() {
	dir := os.join_path(os.vtmp_dir(), 'v3_optional_array_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'v3_optional_array_and_map_input')
	src := bin + '.v'
	os.write_file(src, 'struct ArrayHolder {
mut:
	data ?[2][3]u8
}

fn make_array() [2][3]u8 {
	return [[u8(1), 2, 3]!, [u8(4), 5, 6]!]!
}

fn assign_array_field() {
	mut foo := ArrayHolder{}
	foo.data = make_array()
	data := foo.data or { panic("missing fixed array") }
	assert data == [[u8(1), 2, 3]!, [u8(4), 5, 6]!]!
}

struct Empty {}

type Arr = [2]Empty

struct IndexCounter {
mut:
	calls int
}

fn next_index(mut counter IndexCounter) int {
	counter.calls++
	return 0
}

fn assign_array_index() {
	mut values := []?Arr{len: 1}
	mut counter := IndexCounter{}
	values[next_index(mut counter)] = Arr{}
	assert counter.calls == 1
	assert values[0] != none
	assert values#[-1] != none
	values[next_index(mut counter)] = ?Arr(none)
	assert counter.calls == 2
	assert values[0] == none
	assert values#[-1] == none
}

struct Trace {
mut:
	value int
}

fn make_map(mut trace Trace) map[string]?int {
	trace.value = trace.value * 10 + 1
	return map[string]?int{}
}

fn make_key(mut trace Trace) string {
	trace.value = trace.value * 10 + 2
	return "key"
}

fn compare_map_index() {
	mut trace := Trace{}
	assert make_map(mut trace)[make_key(mut trace)] == none
	assert trace.value == 12
}

type OptionalInts = []?int

type Values = OptionalInts | string

fn smartcast_array_index() {
	x := Values(OptionalInts([?int(none)]))
	if x is OptionalInts {
		assert x[0] == none
		return
	}
	panic("expected OptionalInts")
}

struct Foo {}

struct Bar {}

type Value = Bar | Foo

fn nested_smartcast_array_index() {
	mut values := []?Value{len: 1}
	values[0] = Foo{}
	if values[0] != none {
		if values[0] is Foo {
			assert values[0] != none
			return
		}
	}
	panic("expected Foo")
}

fn main() {
	assign_array_field()
	assign_array_index()
	compare_map_index()
	smartcast_array_index()
	nested_smartcast_array_index()
	println("ok")
}
')!

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c')!
	field_signature := 'void assign_array_field(void) {'
	assert c_code.contains(field_signature), c_code
	field_body := c_code.all_after(field_signature).all_before('\n}')
	assert field_body.contains('foo.data = ({ __v_option_'), field_body
	assert field_body.contains('memcpy(') && field_body.contains('.value, (make_array()).ret_arr'), field_body
	map_signature := 'void compare_map_index(void) {'
	assert c_code.contains(map_signature), c_code
	map_body := c_code.all_after(map_signature).all_before('\n}')
	map_pos := map_body.index('map __in_lhs_') or { panic('missing map temporary') }
	key_pos := map_body.index('string __map_key_') or { panic('missing map key temporary') }
	assert map_pos < key_pos, map_body

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
