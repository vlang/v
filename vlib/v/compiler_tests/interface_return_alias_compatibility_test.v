import os
import v.tempname

fn compile_interface_return(output_path string, source string) os.Result {
	source_path := output_path + '.v'
	os.write_file(source_path, source) or { panic(err) }
	return os.exec([@VEXE, '-nocache', '-b', 'c', '-o', output_path, source_path])
}

fn test_interface_method_return_only_accepts_alias_equivalence() {
	root := os.join_path(os.vtmp_dir(), 'v3_interface_return_alias_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }

	alias_bin := os.join_path(root, 'chained_alias')
	alias_compile := compile_interface_return(alias_bin,
		'type Value = int\n\ntype ChainedValue = Value\n\ninterface Provider {\n\tvalue() int\n}\n\nstruct AliasedValue {}\n\nfn (a AliasedValue) value() ChainedValue {\n\treturn 42\n}\n\nfn main() {\n\tprovider := Provider(AliasedValue{})\n\tprintln(provider.value())\n}\n')
	assert alias_compile.exit_code == 0, alias_compile.output
	alias_run := os.exec([alias_bin])
	assert alias_run.exit_code == 0, alias_run.output
	assert alias_run.output.trim_space() == '42', alias_run.output

	wrapped_fn_bin := os.join_path(root, 'wrapped_fn_alias')
	wrapped_fn_compile := compile_interface_return(wrapped_fn_bin,
		'type MathOp = fn (int, int) int\n\ntype EquivalentMathOp = fn (int, int) int\n\ninterface Calculator {\n\tget_operation() ?MathOp\n}\n\nstruct SimpleCalc {}\n\nfn (s SimpleCalc) get_operation() ?EquivalentMathOp {\n\treturn fn (a int, b int) int {\n\t\treturn a + b\n\t}\n}\n\nfn main() {\n\tcalc := Calculator(SimpleCalc{})\n\toperation := calc.get_operation() or { panic("missing operation") }\n\tprintln(operation(2, 3))\n}\n')
	assert wrapped_fn_compile.exit_code == 0, wrapped_fn_compile.output
	wrapped_fn_run := os.exec([wrapped_fn_bin])
	assert wrapped_fn_run.exit_code == 0, wrapped_fn_run.output
	assert wrapped_fn_run.output.trim_space() == '5', wrapped_fn_run.output

	multi_return_wrapped_fn_bin := os.join_path(root, 'multi_return_wrapped_fn_alias')
	multi_return_wrapped_fn_compile := compile_interface_return(multi_return_wrapped_fn_bin,
		'type Msg = string\n\ntype Cmd = fn () ?Msg\n\ntype EquivalentCmd = fn () ?Msg\n\ninterface Model {\n\tupdate(msg Msg) (Model, ?Cmd)\n}\n\nstruct ExampleModel {}\n\nfn (_ ExampleModel) update(msg Msg) (Model, ?EquivalentCmd) {\n\t_ = msg\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Model(ExampleModel{})\n\tprintln("ok")\n}\n')
	assert multi_return_wrapped_fn_compile.exit_code == 0, multi_return_wrapped_fn_compile.output
	multi_return_wrapped_fn_run := os.exec([multi_return_wrapped_fn_bin])
	assert multi_return_wrapped_fn_run.exit_code == 0, multi_return_wrapped_fn_run.output
	assert multi_return_wrapped_fn_run.output.trim_space() == 'ok', multi_return_wrapped_fn_run.output

	option_multi_return_wrapped_fn_bin := os.join_path(root, 'option_multi_return_wrapped_fn_alias')
	option_multi_return_wrapped_fn_compile := compile_interface_return(option_multi_return_wrapped_fn_bin,
		'type Msg = string\n\ntype Cmd = fn () ?Msg\n\ntype EquivalentCmd = fn () ?Msg\n\ninterface Model {\n\tupdate(msg Msg) ?(Model, ?Cmd)\n}\n\nstruct ExampleModel {}\n\nfn (_ ExampleModel) update(msg Msg) ?(Model, ?EquivalentCmd) {\n\t_ = msg\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Model(ExampleModel{})\n\tprintln("ok")\n}\n')
	assert option_multi_return_wrapped_fn_compile.exit_code == 0, option_multi_return_wrapped_fn_compile.output
	option_multi_return_wrapped_fn_run :=
		os.exec([option_multi_return_wrapped_fn_bin])
	assert option_multi_return_wrapped_fn_run.exit_code == 0, option_multi_return_wrapped_fn_run.output
	assert option_multi_return_wrapped_fn_run.output.trim_space() == 'ok', option_multi_return_wrapped_fn_run.output

	result_multi_return_wrapped_fn_bin := os.join_path(root, 'result_multi_return_wrapped_fn_alias')
	result_multi_return_wrapped_fn_compile := compile_interface_return(result_multi_return_wrapped_fn_bin,
		'type Msg = string\n\ntype Cmd = fn () ?Msg\n\ntype EquivalentCmd = fn () ?Msg\n\ninterface Model {\n\tupdate(msg Msg) !(Model, ?Cmd)\n}\n\nstruct ExampleModel {}\n\nfn (_ ExampleModel) update(msg Msg) !(Model, ?EquivalentCmd) {\n\t_ = msg\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Model(ExampleModel{})\n\tprintln("ok")\n}\n')
	assert result_multi_return_wrapped_fn_compile.exit_code == 0, result_multi_return_wrapped_fn_compile.output
	result_multi_return_wrapped_fn_run :=
		os.exec([result_multi_return_wrapped_fn_bin])
	assert result_multi_return_wrapped_fn_run.exit_code == 0, result_multi_return_wrapped_fn_run.output
	assert result_multi_return_wrapped_fn_run.output.trim_space() == 'ok', result_multi_return_wrapped_fn_run.output

	wrapped_component_alias_bin := os.join_path(root, 'wrapped_fn_component_alias')
	wrapped_component_alias_compile := compile_interface_return(wrapped_component_alias_bin,
		'type MyInt = int\n\ntype MathOp = fn (int, int) int\n\ntype AliasedMathOp = fn (MyInt, MyInt) MyInt\n\ninterface Calculator {\n\tget_operation() ?AliasedMathOp\n}\n\nstruct SimpleCalc {}\n\nfn (s SimpleCalc) get_operation() ?MathOp {\n\treturn fn (a int, b int) int {\n\t\treturn a + b\n\t}\n}\n\nfn main() {\n\t_ := Calculator(SimpleCalc{})\n}\n')
	assert wrapped_component_alias_compile.exit_code != 0, wrapped_component_alias_compile.output
	assert wrapped_component_alias_compile.output.contains('expected return type `?AliasedMathOp`'), wrapped_component_alias_compile.output

	multi_return_wrapped_component_alias_bin := os.join_path(root, 'multi_return_wrapped_fn_component_alias')
	multi_return_wrapped_component_alias_compile := compile_interface_return(multi_return_wrapped_component_alias_bin,
		'type MyInt = int\n\ntype MathOp = fn (int, int) int\n\ntype AliasedMathOp = fn (MyInt, MyInt) MyInt\n\ninterface Calculator {\n\tget_operation() (int, ?AliasedMathOp)\n}\n\nstruct SimpleCalc {}\n\nfn (_ SimpleCalc) get_operation() (int, ?MathOp) {\n\tpanic("not implemented")\n}\n\nfn main() {\n\t_ := Calculator(SimpleCalc{})\n}\n')
	assert multi_return_wrapped_component_alias_compile.exit_code != 0, multi_return_wrapped_component_alias_compile.output
	assert multi_return_wrapped_component_alias_compile.output.contains('expected return type `(int, ?AliasedMathOp)`'), multi_return_wrapped_component_alias_compile.output

	int_bin := os.join_path(root, 'integer_conversion')
	int_compile := compile_interface_return(int_bin,
		'interface Provider {\n\tvalue() int\n}\n\nstruct WideValue {}\n\nfn (w WideValue) value() i64 {\n\treturn 42\n}\n\nfn main() {\n\t_ := Provider(WideValue{})\n}\n')
	assert int_compile.exit_code != 0, int_compile.output
	assert int_compile.output.contains('expected return type `int`'), int_compile.output

	float_bin := os.join_path(root, 'float_conversion')
	float_compile := compile_interface_return(float_bin,
		'interface Provider {\n\tvalue() f32\n}\n\nstruct WideValue {}\n\nfn (w WideValue) value() f64 {\n\treturn 42.0\n}\n\nfn main() {\n\t_ := Provider(WideValue{})\n}\n')
	assert float_compile.exit_code != 0, float_compile.output
	assert float_compile.output.contains('expected return type `f32`'), float_compile.output
}
