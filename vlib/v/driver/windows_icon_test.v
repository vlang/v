module driver

import os
import v.cmdexec

fn test_windows_icon_option_aliases() {
	for prefix in ['-icon=', '--icon=', '-seticon=', '--seticon='] {
		assert (windows_icon_inline_option_value(prefix + 'icon.png') or { '' }) == 'icon.png'
		assert (windows_icon_inline_option_value(prefix) or { 'missing' }) == ''
	}
	assert (windows_icon_inline_option_value('-other=icon.png') or { 'missing' }) == 'missing'
}

fn test_windows_icon_png_conversion() {
	png_path := os.join_path(@VEXEROOT, 'examples', 'assets', 'logo.png')
	png_bytes := os.read_bytes(png_path)!
	ico_bytes := png_to_ico_bytes(png_bytes)!
	images := parse_ico_bytes(ico_bytes)!
	assert images.len == 1
	assert images[0].bytes_in_res == png_bytes.len
	assert images[0].image_data == png_bytes
}

fn test_windows_icon_option_validation() {
	icon := os.join_path(@VEXEROOT, 'examples', 'assets', 'logo.png')
	validate_windows_icon_option(icon, 'windows', 'c', false, false, false, '')!
	for target in ['macos', 'linux'] {
		mut rejected := false
		validate_windows_icon_option(icon, target, 'c', false, false, false, '') or {
			rejected = true
			assert err.msg().contains('Windows executables')
		}
		assert rejected
	}
	mut rejected := false
	validate_windows_icon_option(icon, 'windows', 'c', false, false, true, '') or {
		rejected = true
		assert err.msg().contains('generated C')
	}
	assert rejected
}

fn test_windows_icon_cli_builds_with_all_aliases() {
	mut compiler := ''
	$if !windows {
		compiler = os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc') or { return }
		find_windows_windres(compiler) or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_icon_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	ico := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vdoc', 'theme', 'favicons', 'favicon.ico')
	png := os.join_path(@VEXEROOT, 'examples', 'assets', 'logo.png')
	for idx, flag in ['-icon', '--icon', '-seticon', '--seticon'] {
		output := os.join_path(root, 'icon_${idx}.exe')
		icon := if idx % 2 == 0 { ico } else { png }
		mut args := ['-new-compiler', '-nocache', '-os', 'windows']
		if compiler != '' {
			args << ['-cc', compiler]
		}
		if idx % 2 == 0 {
			args << [flag, icon]
		} else {
			args << '${flag}=${icon}'
		}
		args << ['-o', output, source]
		result := cmdexec.run(@VEXE, args)
		assert result.exit_code == 0, result.output
		assert os.is_file(output)
	}
}

fn test_windows_icon_failed_update_does_not_publish_executable() {
	$if windows {
		root := os.join_path(os.vtmp_dir(), 'v3_windows_icon_invalid_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		icon := os.join_path(root, 'invalid.ico')
		os.write_file(icon, 'invalid icon')!
		source := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
		for existing in [false, true] {
			output := os.join_path(root, 'result.exe')
			if existing {
				os.write_file(output, 'previous output')!
			}
			result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-os', 'windows', '-icon',
				icon, '-o', output, source])
			assert result.exit_code != 0
			assert result.output.contains('invalid icon file'), result.output
			if existing {
				assert os.read_file(output)! == 'previous output'
			} else {
				assert !os.exists(output)
			}
		}
	}
}
