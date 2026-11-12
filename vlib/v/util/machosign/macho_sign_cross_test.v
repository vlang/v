module machosign

import os

fn test_portable_signer_uses_the_available_hash_and_file_implementations() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macho_sign_cross_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'sign.v')
	output := os.join_path(root, 'sign.c')
	conditions := os.join_path(root, 'conditions.c')
	cc := os.getenv_opt('CC') or { 'cc' }
	os.write_file(source, 'import os\nimport v.util.machosign\nfn main() { machosign.sign_adhoc(os.args[1]) or { panic(err) } }\n')!
	for variant in ['portable', 'portable_macos', 'native_macos'] {
		portable := variant != 'native_macos'
		mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-o', output]
		if portable {
			command << '-cross'
		}
		if variant != 'portable' {
			command << ['-os', 'macos']
		}
		command << source
		generated := os.exec(command)
		assert generated.exit_code == 0, generated.output
		code := os.read_file(output)!
		if portable {
			// Portable C retains target-dependent branches. Preprocess the
			// generated bodies as macOS without requiring SDK headers.
			defines := code.split_into_lines().filter(it.starts_with('#define CUSTOM_DEFINE_cross'))
			assert defines.len == 1
			bodies := machosign_c_functions(code)
			assert bodies.len > 0
			os.write_file(conditions, defines.join('\n') + '\n' + bodies)!
			selected := os.exec([cc, '-E', '-P', '-D__APPLE__',
				'-U__ENVIRONMENT_IPHONE_OS_VERSION_MIN_REQUIRED__', conditions])
			assert selected.exit_code == 0, selected.output
			assert !selected.output.contains('fastc_hash_code_pages_native(')
			assert !selected.output.contains('fastc_sign_macho_adhoc_mapped(')
			assert selected.output.contains('machosign__fastc_hash_code_pages(')
			assert selected.output.contains('machosign__fastc_sign_macho_adhoc_buffered(')
			// Generating on a Darwin target must not retain native definitions
			// that would then make the snapshot require Darwin APIs elsewhere.
			assert !code.contains('void machosign__fastc_hash_code_pages_native(')
			assert !code.contains('__v_result machosign__fastc_sign_macho_adhoc_mapped(')
		} else {
			// Native macOS builds retain mapped signing and the system digest.
			assert code.contains('machosign__fastc_hash_code_pages_native(')
			assert code.contains('machosign__fastc_sign_macho_adhoc_mapped(')
			assert code.contains('CC_SHA256(')
		}
	}
}

fn machosign_c_functions(code string) string {
	mut bodies := []string{}
	mut inside := false
	for line in code.split_into_lines() {
		if !inside && !(line.contains(' machosign__') && line.contains('(')
			&& line.ends_with(' {')) {
			continue
		}
		inside = true
		bodies << line
		if line == '}' {
			inside = false
		}
	}
	return bodies.join('\n') + '\n'
}
