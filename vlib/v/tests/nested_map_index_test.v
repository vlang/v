import encoding.binary as bin

[heap]
struct NMachine {
mut:
	threads shared map[u16]NThread
pub mut:
	modules   map[u64]NModule
	constants shared map[u64]NStackValue
}

pub struct NModule {
pub mut:
	functions map[u64]NFunction
	constants map[u64]NStackValue
}

[heap]
pub struct NFunction {
pub mut:
	labels map[u64]int
mut:
	code []u8
}

[heap]
pub struct NThread {
pub mut:
	last_index     int
	current_module u64
mut:
	stack NStack
}

pub type NStackValue = []u8 | u8

pub struct NStack {
mut:
	values []NStackValue
}

pub fn (mut stack NStack) push(val NStackValue) {
	stack.values << val
}

pub fn (mut stack NStack) pop() NStackValue {
	return stack.values.pop()
}

pub struct OperationArgs {
pub:
	opcode u8
pub mut:
	nm     &NMachine
	func   &NFunction
	thread &NThread
	code   &BytecodeIterator
}

pub struct BytecodeIterator {
	arr []u8 [required]
mut:
	idx int
}

pub fn (mut iter BytecodeIterator) next() ?u8 {
	if iter.idx >= iter.arr.len {
		return none
	}
	defer {
		iter.idx++
	}
	return iter.arr[iter.idx]
}

pub fn (mut iter BytecodeIterator) next_u64() ?u64 {
	return read_u64(iter.next_n(8)?)
}

fn load_const(mut args OperationArgs) ? {
	name := args.code.next_u64()?
	val := args.nm.modules[args.thread.current_module]?.constants[name]?
	args.thread.push_stack(val)
}

pub fn read_u64(bytes []u8) u64 {
	mut nb := bytes.clone()
	len := nb.len
	if len < 8 {
		for _ in 0 .. (9 - len) {
			nb << u8(0)
		}
	}
	i := bin.little_endian_u64(nb)
	return i
}

pub fn (mut thread NThread) push_stack(val NStackValue) {
	thread.stack.push(val)
}

pub fn (mut iter BytecodeIterator) next_n(n int) ?[]u8 {
	mut results := []u8{}
	for _ in 0 .. n {
		results << iter.next()?
	}
	return results
}

fn test_nested_map_index() {
	println('hi')
	assert true
}
