// Shared type definitions used by both Erlang source and Core Erlang backends.
// FnInfo is used by CoreGen for export collection.
// Comptime types are used by CoreGen.comptime_stack.
module beam

import os
import v.ast

// NativeTarget selects native code emission backend.
// Set via VBEAM_TARGET env var (arm64, x86_64).
enum NativeTarget {
	none
	x86_64
	arm64
}

fn native_target_from_env() NativeTarget {
	return match os.getenv('VBEAM_TARGET') {
		'arm64', 'aarch64' { .arm64 }
		'x86_64', 'amd64' { .x86_64 }
		else { .none }
	}
}

// FnInfo tracks function declarations for exports
struct FnInfo {
	name   string
	arity  int
	is_pub bool
}

// ComptimeMeta represents compile-time metadata for reflection
// This is a tagged union of different meta types we can iterate over
pub type ComptimeMeta = FieldMeta | MethodMeta | EnumValueMeta | AttrMeta

pub struct FieldMeta {
pub:
	name   string
	typ    ast.Type
	attrs  []ast.Attr
	is_pub bool
	is_mut bool
}

pub struct MethodMeta {
pub:
	name        string
	params      []ast.Param
	return_type ast.Type
	attrs       []ast.Attr
	is_pub      bool
}

pub struct EnumValueMeta {
pub:
	name  string
	value i64
}

pub struct AttrMeta {
pub:
	name string
	arg  string // Single arg value (V attributes have single arg)
}

// ComptimeEnv tracks the current comptime loop variable bindings
struct ComptimeEnv {
	var_name string       // "field", "method", etc.
	value    ComptimeMeta // The current iteration value
	typ      ast.Type     // The type being iterated over (e.g., App for App.methods)
	kind     ast.ComptimeForKind
}
