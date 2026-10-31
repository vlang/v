module main

import os
import time

fn test_cache_restart_cleans_its_first_build_directory() ! {
	root := os.join_path(os.vtmp_dir(), 'cache_restart_cleanup_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	with_os := os.join_path(root, 'with_os.v')
	plain := os.join_path(root, 'plain.v')
	script := os.join_path(root, 'script.vsh')
	os.write_file(with_os, 'import os\nfn main() { println(os.args.len) }\n')!
	os.write_file(plain, 'fn main() { println(1) }\n')!
	os.write_file(script, 'println(3)\n')!
	for order, sources in [[with_os, plain], [plain, with_os], [plain, script]] {
		cache := os.join_path(root, 'cache_${order}')
		for source in sources {
			mut process := os.new_process(@VEXE)
			process.set_args(['run', source])
			mut environment := os.environ()
			environment.delete('VFLAGS')
			environment.delete('VEXE')
			environment['V3CACHE'] = cache
			environment['V_MACOS_V3_NO_FALLBACK'] = '1'
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.wait()
			stdout := process.stdout_slurp()
			stderr := process.stderr_slurp()
			output := stdout + stderr
			assert process.code == 0, output
			expected := if source == script { '3' } else { '1' }
			assert stdout.trim_space() == expected, output
			process.close()
			for entry in os.ls(root)! {
				assert !entry.contains('.v3cc.'), 'leaked C build directory: ${entry}'
			}
		}
	}
}
