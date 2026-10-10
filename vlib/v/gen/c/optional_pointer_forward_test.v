module c

import os
import v.cmdexec
import v.flat
import v.types

fn test_pruned_generic_pointer_results_and_options_compile_locally_and_across_imports() {
	root := os.join_path(os.vtmp_dir(), 'optional_pointer_forward_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	declarations := 'pub struct Box[T] {
pub:
	value T
}
pub fn make_box[T](value T) !&Box[T] {
	return &Box[T]{value: value}
}
pub fn maybe_box[T](value T) ?&Box[T] {
	return &Box[T]{value: value}
}
'
	for imported in [false, true] {
		path := os.join_path(root, if imported { 'imported' } else { 'local' })
		os.mkdir_all(path) or { panic(err) }
		prefix := if imported { 'boxes.' } else { '' }
		mut source := ''
		if imported {
			os.mkdir_all(os.join_path(path, 'boxes')) or { panic(err) }
			os.write_file(os.join_path(path, 'boxes', 'boxes.v'), 'module boxes\n${declarations}') or {
				panic(err)
			}
			source = 'import boxes\n'
		} else {
			source = declarations
		}
		source += 'interface Value {
	get() int
}
struct Number {
	n int
}
fn (number Number) get() int { return number.n }
type NumberPointer = &Number
fn identity(value NumberPointer) !NumberPointer { return value }
fn unused() int {
	boolean := ${prefix}make_box(false) or { panic(err) }
	optional := ${prefix}maybe_box(false) or { panic("missing") }
	contract := ${prefix}make_box(Value(Number{n: 2})) or { panic(err) }
	pointer := ${prefix}maybe_box(&Number{n: 3}) or { panic("missing") }
	return if boolean.value || optional.value { 1 } else { contract.value.get() + pointer.value.n }
}
fn main() {
	integer := ${prefix}make_box(42) or { panic(err) }
	text := ${prefix}maybe_box("live") or { panic("missing") }
	assert integer.value == 42
	assert text.value == "live"
	number := identity(&Number{n: 7}) or { panic(err) }
	assert number.n == 7
	println("optional-pointers-ok")
}
'
		main_file := os.join_path(path, 'main.v')
		os.write_file(main_file, source) or { panic(err) }
		for serial in [false, true] {
			mut args := ['-b', 'c', '-nocache', '-no-retry-compilation']
			if serial {
				args << '-no-parallel'
			}
			args << ['run', main_file]
			result := cmdexec.run(@VEXE, args)
			assert result.exit_code == 0, result.output
			assert result.output.trim_space().ends_with('optional-pointers-ok'), result.output
		}
	}
}

fn test_optional_pointer_aliases_forward_the_underlying_struct() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	pointee := types.Type(types.Struct{ name: 'payload.Missing' })
	pointer_alias := types.Type(types.Alias{
		name:      'PointerAlias'
		base_type: types.Type(types.Pointer{ base_type: pointee })
	})
	struct_alias := types.Type(types.Alias{ name: 'RecordAlias', base_type: pointee })
	g.optional_type_name(types.Type(types.ResultType{ base_type: pointer_alias }))
	g.optional_type_name(types.Type(types.OptionType{
		base_type: types.Type(types.Pointer{ base_type: struct_alias })
	}))
	g.optional_type_name(types.Type(types.ResultType{
		base_type: types.Type(types.Pointer{ base_type: pointer_alias })
	}))
	g.optional_typedefs()
	emitted := g.sb.str()
	assert emitted.contains('typedef struct payload__Missing payload__Missing;'), emitted
	assert emitted.contains('payload__Missing* value;'), emitted
	assert emitted.contains('payload__Missing** value;'), emitted
	assert !emitted.contains('typedef struct PointerAlias'), emitted
	assert !emitted.contains('typedef struct RecordAlias'), emitted
}

fn test_pruned_cached_pointer_wrappers_do_not_emit_unused_pointees() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	for name in ['Used', 'Unused'] {
		tc.structs['payload.${name}'] = []types.StructField{}
		tc.const_types['payload.${name.to_lower()}'] = types.ResultType{
			base_type: types.Pointer{ base_type: types.Type(types.Struct{ name: 'payload.${name}' }) }
		}
	}
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.collect_checker_declaration_signature_types()
	g.cache_decl_demand = true
	g.cache_decl_refs[g.const_ident_c_name('payload.used')] = true
	g.finish_cache_declaration_demand('')
	g.optional_typedefs()
	emitted := g.sb.str()
	assert emitted.contains('payload__Used* value;'), emitted
	assert !emitted.contains('payload__Unused'), emitted
}
