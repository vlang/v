module main

fn test_wasm_input_preserves_argument_boundaries() {
	assert compiler_args_before_program(['guest.wasm', '-cc', 'tcc', '-prod', '-usecache']) == ['guest.wasm']
	assert !compiler_option_requested(['guest.wasm', '-usecache'], '-usecache')
	assert wasm_input(['run', 'app.wasm', 'test'])? == 1
	assert wasm_input(['app.wasm', '--help'])? == 0
	assert wasm_input(['-g', 'run', 'space name.wasm'])? == 2
	for args in [
		['-o', 'out.wasm', 'main.v'],
		['run', 'main.v', 'input.wasm'],
		['-e', 'println(1)', 'input.wasm'],
		['-d', 'file.wasm', 'main.v'],
		['test', 'input.wasm'],
	] {
		assert wasm_input(args) == none
	}
	assert external_tool_runtime_args('wasm', [], ['wasm', '--invoke', 'add', 'x.wasm', '2']) == [
		'--invoke',
		'add',
		'x.wasm',
		'2',
	]
}
