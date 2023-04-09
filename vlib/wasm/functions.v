module wasm

struct ImportCallPatch {
	mod  string
	name string
	pos  int
}

struct FunctionCallPatch {
	name string
	pos  int
}

type CallPatch = FunctionCallPatch | ImportCallPatch

struct Function {
	tidx int
	idx  int
mut:
	call_patches []CallPatch
	label        int
	export       bool
	mod          &Module
	code         []u8
	locals       []ValType
pub:
	name string
}
