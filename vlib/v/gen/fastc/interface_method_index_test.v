module fastc

import v.flat
import v.pref
import v.scanner
import os
import v.cmdexec

fn test_shared_interface_method_index_keeps_exact_owners_and_empty_interfaces() {
	reader := flat.source_type_id('fixture.Reader')
	empty := flat.source_type_id('fixture.Empty')
	structure := flat.source_type_id('fixture.Value')
	index := fastc_interface_method_names({
		'fixture.Reader.read':          FastcFunctionSignature{}
		'fixture.Reader.read_more':     FastcFunctionSignature{}
		'fixture.Reader.nested.helper': FastcFunctionSignature{}
		'fixture.ReaderExtra.read':     FastcFunctionSignature{}
		'fixture.Value.read':           FastcFunctionSignature{}
		'fixture.free_function':        FastcFunctionSignature{}
	}, {
		reader:    FastcDeclaredTypeKind.interface_
		empty:     FastcDeclaredTypeKind.interface_
		structure: FastcDeclaredTypeKind.struct_
	})
	mut methods := index[reader].clone()
	methods.sort()
	assert methods == ['read', 'read_more']
	assert empty in index && index[empty].len == 0
	assert structure !in index
	assert index.len == 2
}

fn test_interface_implementation_memo_keeps_canonical_types_across_modules() ! {
	prefs := pref.new_preferences()
	iface := flat.source_type_id('fixture.Reader')
	declared := {
		iface:                              true
		flat.source_type_id('alpha.Value'): true
		flat.source_type_id('beta.Value'):  true
	}
	mut g := Parser{
		prefs:                     prefs
		s:                         scanner.new_scanner(prefs, .normal)
		module_name:               'alpha'
		declared_types:            declared
		declared_type_c_names:     fastc_declared_type_c_names(declared)
		declared_type_key_by_name: fastc_declared_type_key_by_name(declared)
		functions:                 {
			'fixture.Reader.read': FastcFunctionSignature{}
			'alpha.Value.read':    FastcFunctionSignature{}
		}
		interface_method_names:    {
			iface: ['read']
		}
	}
	present := flat.source_type_id('alpha.Value')
	missing := flat.source_type_id('beta.Value')
	assert g.type_implements_interface(present, iface)
	assert !g.type_implements_interface(missing, iface)
	assert g.interface_implementation_memo.len == 2
	g.parse_mono_instance(FastcSourceFile{
		path:   'interface_memo_beta.v'
		header: FastcSourceHeader{ module_name: 'beta' }
	})!
	assert !g.type_implements_interface(missing, iface)
	assert g.type_implements_interface(present, iface)
	g.parse_mono_instance(FastcSourceFile{
		path:   'interface_memo_alpha.v'
		header: FastcSourceHeader{ module_name: 'alpha' }
	})!
	assert g.type_implements_interface(present, iface)
	assert !g.type_implements_interface(missing, iface)
}

fn test_indexed_interface_queries_keep_positive_and_negative_types_separate() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_interface_queries_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
interface Reader { read() int }
struct Present {}
fn (p Present) read() int { return 42 }
struct Missing {}
fn query[T](value T) int {
    _ = value
    mut score := 0
    $if T is Reader { score += 1 }
    $if T is Reader { score += 2 }
    return score
}
fn main() {
    assert query(Present{}) == 3
    assert query(Missing{}) == 0
    assert query(42) == 0
    assert query(&Present{}) == 3
    assert query("text") == 0
}
')!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
	}
}
