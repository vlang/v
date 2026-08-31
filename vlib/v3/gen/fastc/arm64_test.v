module fastc

import os
import v3.pref

fn test_fastc_parser_emits_arm64_without_c() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, "\$if arm64 ? {\n\tfn selected() int {\n\t\treturn 2\n\t}\n} \$else {\n\tfn selected() int {\n\t\treturn 100\n\t}\n}\n\nfn add(a int, b int) int {\n\treturn a + b\n}\n\nfn is_letter(c u8) bool {\n\treturn (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)\n}\n\nfn main() {\n\tmut sum := 0\n\tfor sum < 3 {\n\t\tsum += 1\n\t}\n\tif add(sum, selected()) == 5 && is_letter(111) && native_features() {\n\t\t\$if arm64 ? {\n\t\t\tprintln('native')\n\t\t} \$else {\n\t\t\tprintln('wrong')\n\t\t}\n\t} else {\n\t\tprintln('wrong')\n\t}\n}\n\nenum FastArm64Mode {\n\tcold\n\twarm\n}\n\nstruct FastArm64Counter {\nmut:\n\tvalue int\n}\n\nfn (mut counter FastArm64Counter) bump() {\n\tcounter.value += 1\n}\n\nfn native_features() bool {\n\tmut counter := FastArm64Counter{}\n\tcounter.bump()\n\tmut labels := map[string]string{}\n\tlabels['backend'] = 'native'\n\tmut modes := map[string]FastArm64Mode{}\n\tmodes['backend'] = FastArm64Mode.warm\n\treturn counter.value == 1 && labels['backend'] == 'native' && modes['backend'] == .warm && 0x100000000 > 0xffffffff && `'` == 39\n}\n") or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		result := generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		assert result.source_paths.len > 0
		assert os.is_executable(output_path)
		assert !os.exists(output_path + '.c')
		run_result := os.execute(output_path)
		assert run_result.exit_code == 0
		assert run_result.output == 'native\n'
	}
}
