// vtest build: macos
import os

fn test_sqlite_system_library_without_pkgconfig() {
	$if macos {
		dir := os.join_path(os.vtmp_dir(), 'sqlite_no_pkgconfig_${os.getpid()}')
		os.mkdir_all(dir)!
		defer {
			os.rmdir_all(dir) or {}
		}
		marker := os.join_path(dir, 'pkgconfig_called')
		stub := os.join_path(dir, 'pkg-config')
		os.write_file(stub, '#!/bin/sh\nprintf probed > ${os.quoted_path(marker)}\nexit 1\n')!
		os.chmod(stub, 0o755)!
		source := os.join_path(dir, 'main.c.v')
		output := os.join_path(dir, 'main')
		os.write_file(source, "import db.sqlite\nfn main() {\n mut db := sqlite.connect(':memory:')!\n assert db.q_int('select 42')! == 42\n assert int(C.SQLITE_VERSION_NUMBER) > 0\n db.close()!\n}\n")!
		mut environment := os.environ()
		environment.delete('VEXE')
		environment.delete('VFLAGS')
		environment['PATH'] = dir + os.path_delimiter + os.getenv('PATH')
		environment['V_MACOS_V3_NO_FALLBACK'] = '1'
		mut compiler := os.new_process(@VEXE)
		compiler.set_args(['-cc', 'clang', '-nocache', '-o', output, source])
		compiler.set_environment(environment)
		compiler.set_redirect_stdio()
		compiler.run()
		compiler.wait()
		stdout := compiler.stdout_slurp()
		stderr := compiler.stderr_slurp()
		assert compiler.code == 0, stdout + stderr
		compiler.close()
		assert os.exists(marker), 'pkg-config must be disabled for the fallback build'
		result := os.exec([output])
		assert result.exit_code == 0, result.output
	}
}
