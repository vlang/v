module main

import os

fn test_workflow_check_reports_each_failed_gate_and_continues() {
	root := os.join_path(os.vtmp_dir(), 'v workflow check review ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	ext := $if windows { '.exe' } $else { '' }
	compiler := os.join_path(root, 'v' + ext)
	gate := os.join_path(root, 'gate' + ext)
	probe := os.join_path(root, 'compiler_probe.v')
	os.write_file(probe, 'module main\nimport os\nfn main() {\n\tstep := if os.args[1] == "-check" { "type-check" } else if os.args[1] == "fmt" { "formatted" } else { "vet" }\n\tprintln("visited " + step)\n\tif step == os.getenv("CHECK_FAIL_STEP") || os.getenv("CHECK_FAIL_STEP") == "all" { exit(1) }\n}\n')!
	build_probe := os.exec([@VEXE, '-o', compiler, probe])
	assert build_probe.exit_code == 0, build_probe.output
	// A .vsh target runs immediately; compile its source as .v to reuse the gate
	// with a different failing compiler step in each case.
	script := os.read_file(os.join_path(@VEXEROOT, 'vlib', 'v', 'skills', 'v-workflow',
		'scripts', 'check.vsh'))!
	gate_source := os.join_path(root, 'check.v')
	os.write_file(gate_source, script.all_after('\n'))!
	build_gate := os.exec([@VEXE, '-o', gate, gate_source])
	assert build_gate.exit_code == 0, build_gate.output
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	os.write_file(first, 'module main\nfn main() {}\n')!
	os.write_file(second, 'module main\nfn main() {}\n')!
	for step in ['', 'type-check', 'formatted', 'vet', 'all'] {
		mut environment := os.environ()
		environment['PATH'] = root + os.path_delimiter + os.getenv('PATH')
		environment['CHECK_FAIL_STEP'] = step
		mut process := os.new_process(gate)
		process.set_environment(environment)
		process.set_args([first, second])
		process.set_redirect_stdio_merged()
		process.run()
		output := process.stdout_slurp()
		process.wait()
		exit_code := process.code
		process.close()
		if step == '' {
			assert exit_code == 0, output
			assert output.contains('check.vsh: everything passed'), output
		} else {
			assert exit_code == 1, output
			assert !output.contains('check.vsh: everything passed'), output
			expected := if step == 'all' { 6 } else { 2 }
			assert output.contains('check.vsh: ${expected} check(s) failed'), output
		}
		for label in ['type-check', 'formatted', 'vet'] {
			for target in [first, second] {
				assert output.contains('${label}: ${target}'), output
			}
		}
	}
}
