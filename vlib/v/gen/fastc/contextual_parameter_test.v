module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn test_fastc_contextual_keyword_parameters_keep_their_types_and_values() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn pair(module, type int) int {
	return module + type
}

fn label(
	module string,
	type int
) string {
	if type == 42 {
		return module
	}
	return "unexpected"
}

fn main() {
	module := "ok"
	type := pair(20, 22)
	println(label(module, type))
}
', 'contextual_parameters.v', prefs) or { panic(err) }
	test_dir := os.join_path(os.vtmp_dir(), 'fastc_contextual_parameters_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	c_file := os.join_path(test_dir, 'program.c')
	bin_file := os.join_path(test_dir, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(bin_file, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'ok\n'
}

fn test_fastc_contextual_parameter_names_in_function_and_interface_signatures() {
	prefs := pref.new_preferences()
	header := FastcSourceHeader{
		module_name: 'main'
	}
	source := 'type Callback = fn (
	module string,
	type int
) bool
interface Handler {
	handle(
		module string,
		type int
	) bool
}
'
	declared := {
		'Callback': true
		'Handler':  true
	}
	mut functions := map[string]FastcFunctionSignature{}
	collect_function_signatures(source, 'contextual_signatures.v', header, prefs, []int{},
		declared, flat.TypeTextBindings{}, map[string]bool{}, mut functions) or { panic(err) }
	callback := functions['Callback'] or { panic('missing callback signature') }
	assert callback.parameter_types.names() == ['string', 'int']
	assert callback.return_type_text() == 'bool'
	mut methods := map[string]bool{}
	mut fields := map[string]FastcInterfaceField{}
	mut field_paths := map[string]string{}
	mut embedders := []u32{}
	mut embedded := []u32{}
	collect_interface_method_signatures(source, 'contextual_signatures.v', header, prefs,
		[]int{}, declared, mut functions, mut methods, mut fields, mut field_paths,
		mut embedders, mut embedded) or { panic(err) }
	handler := functions['Handler.handle'] or { panic('missing handler signature') }
	assert handler.parameter_types.names() == ['Handler', 'string', 'int']
	assert handler.return_type_text() == 'bool'
	mut scan := scanner.new_scanner(prefs, .normal)
	function_source := 'fn (\nmodule string,\ntype int\n) bool'
	file := token.File.unindexed('contextual_function_type.v', function_source.len)
	scan.init(file, function_source)
	_ = scan.scan()
	function_type := fastc_peek_function_type(scan, 'contextual_function_type.v', 'main',
		map[string]string{}, declared, false) or { panic(err) }
	assert function_type.parameter_types.names() == ['string', 'int']
	assert function_type.return_type_text() == 'bool'
}
