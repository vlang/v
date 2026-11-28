import os
import v.tempname

struct InterfaceReturnCase {
	name     string
	source   string
	expected string
}

fn compile_interface_return_batch(output_path string, cases []InterfaceReturnCase) os.Result {
	root := os.dir(output_path)
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_dir := os.join_path(root, case.name)
		os.mkdir_all(module_dir) or { panic(err) }
		imports << 'import ${case.name}'
		calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
		fixture := 'module ${case.name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, '${case.name}.v'), fixture.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.v'), "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	return os.exec([@VEXE, '-nocache', '-b', 'c', '-nocolor', '-o', output_path, root])
}

fn run_interface_return_batch(output_path string, cases []InterfaceReturnCase) {
	compile := compile_interface_return_batch(output_path, cases)
	assert compile.exit_code == 0, compile.output
	for case in cases {
		run := os.exec([output_path, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn check_interface_return_rejections(output_path string, cases []InterfaceReturnCase) {
	compile := compile_interface_return_batch(output_path, cases)
	assert compile.exit_code != 0, compile.output
	mut errors := map[string][]string{}
	for line in compile.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		// Drop the line and column from the right to preserve any colon in the path.
		file := location.all_before_last(':').all_before_last(':')
		errors[os.file_name(file)] << message
	}
	for case in cases {
		assert errors['${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected `${case.expected}` in\n${compile.output}'
	}
}

fn test_interface_method_return_only_accepts_alias_equivalence() {
	root := os.join_path(os.vtmp_dir(), 'v3_interface_return_alias_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }

	run_interface_return_batch(os.join_path(root, 'accepted', 'program'), [
		InterfaceReturnCase{
			name:     'chained_alias'
			source:   'type Value = int\n\ntype ChainedValue = Value\n\ninterface Provider {\n\tvalue() int\n}\n\nstruct AliasedValue {}\n\nfn (a AliasedValue) value() ChainedValue {\n\treturn 42\n}\n\nfn main() {\n\tprovider := Provider(AliasedValue{})\n\tprintln(provider.value())\n}\n'
			expected: '42'
		},
		InterfaceReturnCase{
			name:     'wrapped_fn_alias'
			source:   'type MathOp = fn (int, int) int\n\ntype EquivalentMathOp = fn (int, int) int\n\ninterface Calculator {\n\tget_operation() ?MathOp\n}\n\nstruct SimpleCalc {}\n\nfn (s SimpleCalc) get_operation() ?EquivalentMathOp {\n\treturn fn (a int, b int) int {\n\t\treturn a + b\n\t}\n}\n\nfn main() {\n\tcalc := Calculator(SimpleCalc{})\n\toperation := calc.get_operation() or { panic("missing operation") }\n\tprintln(operation(2, 3))\n}\n'
			expected: '5'
		},
		InterfaceReturnCase{
			name:     'multi_return_wrapped_fn_alias'
			source:   'type Msg = string\n\ntype Cmd = fn () ?Msg\n\ntype EquivalentCmd = fn () ?Msg\n\ninterface Model {\n\tupdate(msg Msg) (Model, ?Cmd)\n}\n\nstruct ExampleModel {}\n\nfn (_ ExampleModel) update(msg Msg) (Model, ?EquivalentCmd) {\n\t_ = msg\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Model(ExampleModel{})\n\tprintln("ok")\n}\n'
			expected: 'ok'
		},
		InterfaceReturnCase{
			name:     'option_multi_return_wrapped_fn_alias'
			source:   'type Msg = string\n\ntype Cmd = fn () ?Msg\n\ntype EquivalentCmd = fn () ?Msg\n\ninterface Model {\n\tupdate(msg Msg) ?(Model, ?Cmd)\n}\n\nstruct ExampleModel {}\n\nfn (_ ExampleModel) update(msg Msg) ?(Model, ?EquivalentCmd) {\n\t_ = msg\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Model(ExampleModel{})\n\tprintln("ok")\n}\n'
			expected: 'ok'
		},
		InterfaceReturnCase{
			name:     'result_multi_return_wrapped_fn_alias'
			source:   'type Msg = string\n\ntype Cmd = fn () ?Msg\n\ntype EquivalentCmd = fn () ?Msg\n\ninterface Model {\n\tupdate(msg Msg) !(Model, ?Cmd)\n}\n\nstruct ExampleModel {}\n\nfn (_ ExampleModel) update(msg Msg) !(Model, ?EquivalentCmd) {\n\t_ = msg\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Model(ExampleModel{})\n\tprintln("ok")\n}\n'
			expected: 'ok'
		},
	])

	check_interface_return_rejections(os.join_path(root, 'rejected', 'program'), [
		InterfaceReturnCase{
			name:     'wrapped_fn_component_alias'
			source:   'type MyInt = int\n\ntype MathOp = fn (int, int) int\n\ntype AliasedMathOp = fn (MyInt, MyInt) MyInt\n\ninterface Calculator {\n\tget_operation() ?AliasedMathOp\n}\n\nstruct SimpleCalc {}\n\nfn (s SimpleCalc) get_operation() ?MathOp {\n\treturn fn (a int, b int) int {\n\t\treturn a + b\n\t}\n}\n\nfn main() {\n\t_ := Calculator(SimpleCalc{})\n}\n'
			expected: 'expected return type `?wrapped_fn_component_alias.AliasedMathOp`'
		},
		InterfaceReturnCase{
			name:     'multi_return_wrapped_fn_component_alias'
			source:   'type MyInt = int\n\ntype MathOp = fn (int, int) int\n\ntype AliasedMathOp = fn (MyInt, MyInt) MyInt\n\ninterface Calculator {\n\tget_operation() (int, ?AliasedMathOp)\n}\n\nstruct SimpleCalc {}\n\nfn (_ SimpleCalc) get_operation() (int, ?MathOp) {\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Calculator(SimpleCalc{})\n}\n'
			expected: 'expected return type `(int, ?multi_return_wrapped_fn_component_alias.AliasedMathOp)`'
		},
		InterfaceReturnCase{
			name:     'integer_conversion'
			source:   'interface Provider {\n\tvalue() int\n}\n\nstruct WideValue {}\n\nfn (w WideValue) value() i64 {\n\treturn 42\n}\n\nfn main() {\n\t_ := Provider(WideValue{})\n}\n'
			expected: 'expected return type `int`'
		},
		InterfaceReturnCase{
			name:     'float_conversion'
			source:   'interface Provider {\n\tvalue() f32\n}\n\nstruct WideValue {}\n\nfn (w WideValue) value() f64 {\n\treturn 42.0\n}\n\nfn main() {\n\t_ := Provider(WideValue{})\n}\n'
			expected: 'expected return type `f32`'
		},
	])
}
