// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// wasm module functions define the structure and behavior of WebAssembly functions.
// This file provides types and methods for function definitions and their metadata.
module wasm

// ImportCallPatch represents a patch for an imported function call or reference.
struct ImportCallPatch {
	mod  string // Module name of the import
	name string // Name of the imported function
mut:
	pos int // Position in the code buffer to patch
}

// FunctionCallPatch represents a patch for a local function call or reference.
struct FunctionCallPatch {
	name string // Name of the local function
mut:
	pos int // Position in the code buffer to patch
}

// CallPatch is a sum type for call-related patches.
type CallPatch = FunctionCallPatch | ImportCallPatch

// FunctionGlobalPatch represents a patch for a global variable reference.
struct FunctionGlobalPatch {
	idx GlobalIndex // Index of the global variable
mut:
	pos int // Position in the code buffer to patch
}

// FunctionPatch is a sum type for all patches in a function’s code.
type FunctionPatch = CallPatch | FunctionGlobalPatch

// FunctionLocal defines a local variable within a function.
struct FunctionLocal {
	typ  ValType // Type of the local (e.g., i32, v128)
	name ?string // Optional name for debug info
}

// Function represents a WebAssembly function within a module.
// It encapsulates code, locals, and metadata for compilation.
pub struct Function {
	tidx int // Index into module’s functypes array
	idx  int // Index among local functions
mut:
	patches []FunctionPatch // Patches for calls and globals, sorted by position
	label   int             // Current label index for control flow
	export  bool            // Whether the function is exported
	mod     &Module = unsafe { nil } // Reference to the parent module
	code    []u8            // Encoded instruction bytes
	locals  []FunctionLocal // Local variables, including parameters
pub:
	name string // Name of the function
pub mut:
	export_name ?string // Optional export name (defaults to name if unset)
}

// export_name sets the export name of the function.
// If not set, the function’s name is used for export.
pub fn (mut func Function) export_name(name string) {
	func.export_name = name
}
