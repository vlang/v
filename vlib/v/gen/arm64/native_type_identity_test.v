module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_compiler_types_preserve_semantic_payload_identity() {
	$if macos && arm64 {
		type_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'types', 'type.v'))!
		identity_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'types', 'type_interner.v'))!
		assert identity_source.contains('pub fn semantic_type_hash(')
		assert identity_source.contains('pub fn semantic_types_equal(')
		assert identity_source.contains('pub fn (tc &TypeChecker) fn_type(')
		// Run the production semantic hash/equality over the real recursive Type
		// layout; copied inline descriptors no longer have boxed payload identity.
		identity_functions := 'pub fn semantic_type_hash(' + identity_source.all_after('pub fn semantic_type_hash(').all_before('pub fn (tc &TypeChecker) fn_type(')
		source := type_source.all_before('// clone_owned_type').replace('module types', 'module main').replace('import v.flat', '') +
			'\n' + identity_functions + r'
const builtin_void_type = Type(Void{})
const empty_type = &builtin_void_type
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.calloc(usize, usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(value voidptr, size isize) voidptr {
    result := C.malloc(usize(size))
    return C.memcpy(result, value, usize(size))
}
fn main() {
    integer := Type(Primitive{props: .integer, size: 32})
    boolean := Type(Primitive{props: .boolean, size: 8})
    copied_integer := integer
    if integer !is Primitive || boolean !is Primitive
        || semantic_types_equal(&integer, &boolean)
        || semantic_type_hash(&integer) == semantic_type_hash(&boolean) { C.exit(1) }
    if !semantic_types_equal(&copied_integer, &integer)
        || semantic_type_hash(&copied_integer) != semantic_type_hash(&integer) { C.exit(2) }
    array := Type(Array{elem_type: new_type_reference(integer)})
    optional_array := Type(OptionType{base_type: new_type_reference(array)})
    tuple := Type(MultiReturn{types: [integer, array, boolean]})
    optional_tuple := Type(OptionType{base_type: new_type_reference(tuple)})
    copied_optional := optional_array
    if optional_array !is OptionType || optional_tuple !is OptionType
        || semantic_types_equal(&optional_array, &optional_tuple)
        || semantic_type_hash(&optional_array) == semantic_type_hash(&optional_tuple) { C.exit(3) }
    if !semantic_types_equal(&copied_optional, &optional_array)
        || semantic_type_hash(&copied_optional) != semantic_type_hash(&optional_array) { C.exit(4) }
    first := Type(FnType{params: [FnParam{typ: new_type_reference(integer)}], return_type: new_type_reference(boolean)})
    second := Type(FnType{params: [FnParam{typ: new_type_reference(boolean)}], return_type: new_type_reference(boolean)})
    if first !is FnType || second !is FnType || semantic_types_equal(&first, &second)
        || semantic_type_hash(&first) == semantic_type_hash(&second) { C.exit(5) }
    zero := unsafe { &Type(C.calloc(1, usize(sizeof(Type)))) }
    if zero !is Void || !semantic_types_equal(zero, &builtin_void_type)
        || semantic_type_hash(zero) != semantic_type_hash(&builtin_void_type) { C.exit(6) }
    equivalent_integer := Type(Primitive{props: .integer, size: 32})
    equivalent_array := Type(Array{elem_type: new_type_reference(equivalent_integer)})
    equivalent_optional := Type(OptionType{base_type: new_type_reference(equivalent_array)})
    if !semantic_types_equal(&integer, &equivalent_integer)
        || semantic_type_hash(&integer) != semantic_type_hash(&equivalent_integer)
        || !semantic_types_equal(&optional_array, &equivalent_optional)
        || semantic_type_hash(&optional_array) != semantic_type_hash(&equivalent_optional) { C.exit(7) }
    mutable := Type(FnType{params: [FnParam{typ: new_type_reference(integer), is_mut: true}], return_type: new_type_reference(boolean)})
    if semantic_types_equal(&first, &mutable)
        || semantic_type_hash(&first) == semantic_type_hash(&mutable) { C.exit(8) }
    variadic := Type(FnType{params: [FnParam{typ: new_type_reference(integer)}], return_type: new_type_reference(boolean), is_variadic: true})
    if semantic_types_equal(&first, &variadic)
        || semantic_type_hash(&first) == semantic_type_hash(&variadic) { C.exit(9) }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_type_identity_${building_v}_${os.getpid()}.v')
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, source)!
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_file(path)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = building_v
			tc.collect(a)
			if !building_v {
				tc.annotate_types()
			}
			assert tc.errors.len == 0, tc.errors.str()
			if building_v {
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			} else {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
		}
	}
}
