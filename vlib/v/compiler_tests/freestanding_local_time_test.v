import os
import time

fn local_time_run(command []string, variables map[string]string) os.Result {
	mut child := os.new_process(command[0])
	child.set_args(command[1..])
	child.set_redirect_stdio()
	mut env := os.environ()
	for key, value in variables { env[key] = value }
	child.set_environment(env)
	child.run()
	watch := time.new_stopwatch()
	for child.is_alive() && watch.elapsed() < 30 * time.second { time.sleep(time.millisecond) }
	expired := child.is_alive()
	if expired { child.signal_kill() }
	child.wait()
	output := child.stdout_slurp() + child.stderr_slurp()
	code := if expired { -1 } else { child.code }
	child.close()
	return os.Result{ exit_code: code, output: output }
}

fn test_linux_freestanding_local_timezone() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'bare_local_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VMODROOT, 'vlib/time/testdata/freestanding_local')
	zip := os.join_path(@VMODROOT, 'vlib/time/tzdata/zoneinfo.zip')
	zone_file := os.join_path(root, 'zone.tzif')
	os.cp(os.join_path(fixture, 'test.tzif'), zone_file)!
	os.write_file(os.join_path(root, 'invalid.tzif'), 'invalid')!
	os.mkdir_all(os.join_path(root, 'Custom'))!
	os.cp(zone_file, os.join_path(root, 'Custom/NewYork'))!
	for prod in [false, true] {
		for name in ['main', 'arena', 'prealloc', 'registry', 'concurrent'] {
			output := os.join_path(root, '${name}_${prod}')
			mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc',
				'clang', '-o', output]
			if prod { command << '-prod' }
			if name == 'prealloc' { command << '-prealloc' }
			if name == 'concurrent' { command << ['-d', 'vlibc'] }
			command << if name == 'main' {
				os.join_path(fixture, 'main.c.v')
			} else {
				os.join_path(fixture, name, 'main.v')
			}
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			if name == 'main' {
				cases := [
					{
						'TZ':             'America/New_York'
						'ZONEINFO':       zip
						'TIME_TEST_MODE': 'ny'
					},
					{
						'TZ':             'EST5EDT,M3.2.0/2,M11.1.0/2'
						'TIME_TEST_MODE': 'ny'
					},
					{
						'TZ':             'AEST-10AEDT,M10.1.0/2,M4.1.0/3'
						'TIME_TEST_MODE': 'south'
					},
					{
						'TZ':             '<NPT>-5:45'
						'TIME_TEST_MODE': 'fraction'
					},
					{
						'TZ': 'UTC0'
					},
					{
						'TZ': ''
					},
					{
						'TZ': 'Vile/Unknown'
					},
					{
						'TZ': '/vile/missing/timezone'
					},
					{
						'TZ': os.join_path(root, 'invalid.tzif')
					},
					{
						'TZ':             zone_file
						'TIME_TEST_MODE': 'ny-file'
					},
					{
						'TZ':             ':' + zone_file
						'TIME_TEST_MODE': 'ny-file'
					},
					{
						'TZ':             'Custom/NewYork'
						'ZONEINFO':       root
						'TIME_TEST_MODE': 'ny-file'
					},
				]
				for setting in cases {
					mut variables := {
						'TIME_TEST_MODE': 'utc'
						'ZONEINFO':       zip
					}
					for key, value in setting { variables[key] = value }
					ran := local_time_run([output], variables)
					assert ran.exit_code == 0 && ran.output.trim_space() == 'freestanding local time ok', '${setting}: ${ran.output}'
				}
			} else {
				ran := local_time_run([output], {
					'TZ':       'Custom/Persistent'
					'ZONEINFO': zip
				})
				expected := match name {
					'registry' { 'timezone registry ok' }
					else { '${name} local time ok' }
				}
				assert ran.exit_code == 0 && ran.output.trim_space() == expected, '${name} prod=${prod} exit=${ran.exit_code}: ${ran.output}'
			}
		}
	}
}

fn test_zoneinfo_registry_survives_scopes_with_autofree() ! {
	root := os.join_path(os.vtmp_dir(), 'zone_registry_autofree_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for prealloc in [false, true] {
		output := os.join_path(root, 'registry_${prealloc}')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-autofree', '-cc', 'clang',
			'-o', output]
		if prealloc { command << '-prealloc' }
		fixture := if prealloc { 'registry_autofree_prealloc' } else { 'registry_autofree' }
		command << os.join_path(@VMODROOT, 'vlib/time/testdata/freestanding_local', fixture, 'main.v')
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := local_time_run([output], {})
		assert ran.exit_code == 0 && ran.output.trim_space() == 'scoped registry autofree ok', ran.output
	}
}
