// vtest build: !windows
module pg

import os

const loaded_runtime_symbols = [
	'PQconnectdb',
	'PQstatus',
	'PQtransactionStatus',
	'PQerrorMessage',
	'PQexec',
	'PQgetisnull',
	'PQgetvalue',
	'PQresultStatus',
	'PQntuples',
	'PQnfields',
	'PQfname',
	'PQftype',
	'PQfmod',
	'PQfsize',
	'PQfformat',
	'PQftable',
	'PQftablecol',
	'PQexecParams',
	'PQputCopyData',
	'PQputCopyEnd',
	'PQgetCopyData',
	'PQprepare',
	'PQexecPrepared',
	'PQclear',
	'PQfreemem',
	'PQfinish',
	'PQnotifies',
	'PQconsumeInput',
	'PQsocket',
	'PQescapeLiteral',
]

fn test_pg_build_does_not_require_libpq_development_files() ! {
	dir := os.join_path(os.vtmp_dir(), 'pg_no_dev_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'app.v')
	generated_c := os.join_path(dir, 'app.c')
	executable := os.join_path(dir, 'app')
	cc_wrapper := os.join_path(dir, 'cc-without-libpq-dev')
	os.write_file(source, 'import db.pg\nfn main() {\n\tprintln(pg.Config{}.port)\n}\n')!
	os.write_file(cc_wrapper, '#!/bin/sh
case " $* " in
	*" -lpq "*) echo "unexpected libpq link flag" >&2; exit 86 ;;
	*" -I/usr/include/postgresql "*) echo "unexpected libpq header path" >&2; exit 87 ;;
esac
exec cc "$@"
')!
	os.chmod(cc_wrapper, 0o755)!
	mut environment := os.environ()
	environment.delete('VFLAGS')
	environment.delete('VEXE')
	build := os.new_process(@VEXE)
	build.set_args(['-nocache', '-cc', cc_wrapper, '-o', executable, source])
	build.set_environment(environment)
	build.set_redirect_stdio()
	build.run()
	build.wait()
	stdout := build.stdout_slurp()
	stderr := build.stderr_slurp()
	assert build.code == 0, stdout + stderr
	build.close()

	build_c := os.new_process(@VEXE)
	build_c.set_args(['-nocache', '-o', generated_c, source])
	build_c.set_environment(environment)
	build_c.set_redirect_stdio()
	build_c.run()
	build_c.wait()
	stdout_c := build_c.stdout_slurp()
	stderr_c := build_c.stderr_slurp()
	assert build_c.code == 0, stdout_c + stderr_c
	build_c.close()
	c_source := os.read_file(generated_c)!
	assert !c_source.contains('libpq-fe.h')
	assert !c_source.contains('pg_config.h')
	assert !c_source.contains('PG_VERSION_NUM')
	assert !c_source.contains('#pkgconfig')
	assert !c_source.contains('-lpq')
}

fn test_pg_runtime_loader_does_not_need_link_time_symbols() ! {
	dir := os.join_path(os.vtmp_dir(), 'pg_runtime_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	mut fake_source := ''
	for symbol in loaded_runtime_symbols {
		fake_source += 'void *${symbol}(void) { return 0; }\n'
	}
	c_path := os.join_path(dir, 'fake_libpq.c')
	mut lib_name := 'libpq.so.5'
	$if macos {
		lib_name = 'libpq.5.dylib'
	}
	lib_path := os.join_path(dir, lib_name)
	os.write_file(c_path, fake_source)!
	mut cc_args := ['-o', lib_path]
	$if macos {
		cc_args << ['-dynamiclib', c_path]
	} $else {
		cc_args << ['-shared', '-fPIC', c_path]
	}
	cc_result := os.exec(['cc', ...cc_args])
	assert cc_result.exit_code == 0, cc_result.output

	old_lib := os.getenv_opt('V_PQ_LIB')
	os.setenv('V_PQ_LIB', lib_path, true)
	defer {
		if value := old_lib {
			os.setenv('V_PQ_LIB', value, true)
		} else {
			os.unsetenv('V_PQ_LIB')
		}
	}
	ensure_pg_runtime()!
	assert pg_runtime.connectdb != unsafe { nil }
	assert pg_runtime.exec_params != unsafe { nil }
	assert pg_runtime.finish != unsafe { nil }
}
