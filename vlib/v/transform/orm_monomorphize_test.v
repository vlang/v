module transform

import os
import strings
import time

fn test_orm_monomorphization_with_many_fields_finishes() {
	root := os.join_path(os.vtmp_dir(), 'orm_monomorphize_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut source := strings.new_builder(20_000)
	source.write_string('module main\nimport db.sqlite\nstruct Big {\n')
	for i in 0 .. 1000 {
		source.writeln('\tf${i} int')
	}
	source.write_string("}\nfn main() {\nmut db := sqlite.connect(':memory:') or { panic(err) }\n_ := sql db { select from Big where f0 == 1 }!\n}\n")
	input := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(input, source.str())!
	mut child := os.new_process(@VEXE)
	child.set_args(['-new-compiler', '-no-retry-compilation', '-nocache', '-o', output, input])
	mut environment := os.environ()
	environment.delete('VFLAGS')
	environment.delete('VOSARGS')
	environment['VJOBS'] = '1'
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	child.set_environment(environment)
	child.set_redirect_stdio_merged()
	child.run()
	defer { child.close() }
	deadline := time.now().add(120 * time.second)
	for child.is_alive() && time.now() < deadline {
		time.sleep(20 * time.millisecond)
	}
	if child.is_alive() {
		child.signal_kill()
		child.wait()
		assert false, 'ORM monomorphization did not finish within 120 seconds'
		return
	}
	child.wait()
	assert child.code == 0, child.stdout_slurp()
	assert os.exists(output)
}
