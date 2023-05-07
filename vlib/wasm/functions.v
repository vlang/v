module wasm

struct ImportCallPatch {
	mod  string
	name string
mut:
	pos int
}

struct FunctionCallPatch {
	name string
mut:
	pos int
}

type CallPatch = FunctionCallPatch | ImportCallPatch

struct FunctionGlobalPatch {
	idx GlobalIndex
mut:
	pos int
}

type FunctionPatch = CallPatch | FunctionGlobalPatch

pub struct Function {
	tidx int
	idx  int
mut:
	patches []FunctionPatch // sorted
	label   int
	export  bool
	mod     &Module = unsafe { nil }
	code    []u8
	locals  []ValType
pub:
	name string
}
