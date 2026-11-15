module c

import os
import v.pref

fn windows_entry_point_gen(subsystem pref.Subsystem, c_flags []string) FlatGen {
	mut g := FlatGen.new()
	g.set_target(pref.target_from('windows', 'amd64') or { panic(err) })
	g.set_subsystem(subsystem)
	g.c_flags = c_flags
	return g
}

fn test_windows_entry_point_uses_gui_subsystem_for_gdi32() {
	mut g := windows_entry_point_gen(.auto, ['-lgdi32'])
	declaration := g.c_main_declaration(false)
	assert declaration.starts_with('int WINAPI wWinMain(')
	assert g.generated_windows_gui_entry_point()? == true
	assert declaration.contains('GetCommandLineW()')
	assert declaration.contains('CommandLineToArgvW(full_cmd_line, &argc)')
	assert declaration.contains('AttachConsole(ATTACH_PARENT_PROCESS)')
	assert declaration.contains('freopen_s(&res_fp, "NUL", "w", stderr)')
}

fn test_windows_entry_point_honors_console_attribute_and_subsystem() {
	mut auto := windows_entry_point_gen(.auto, ['-lgdi32'])
	assert auto.c_main_declaration(true).starts_with('int wmain(')
	assert auto.generated_windows_gui_entry_point()? == false

	mut console := windows_entry_point_gen(.console, ['-lgdi32'])
	assert console.c_main_declaration(false).starts_with('int wmain(')
	assert console.generated_windows_gui_entry_point()? == false

	mut windows := windows_entry_point_gen(.windows, []string{})
	windows_declaration := windows.c_main_declaration(true)
	assert windows_declaration.starts_with('int WINAPI wWinMain(')
	assert windows.generated_windows_gui_entry_point()? == true
	assert windows_declaration.contains('AllocConsole()')
}

fn test_windows_entry_point_defaults_to_console() {
	mut g := windows_entry_point_gen(.auto, []string{})
	assert g.c_main_declaration(false).starts_with('int wmain(')
	assert g.generated_windows_gui_entry_point()? == false
}

fn test_windows_tests_keep_graphics_library_diagnostics_capturable() {
	root := os.join_path(os.vtmp_dir(), 'windows_test_console_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'capture_test.v')
	os.write_file(source, 'module main

#flag windows -lgdi32

fn test_captured_failure() {
	println("captured test stdout")
	eprintln("captured test stderr")
	assert false, "captured test assertion"
}
')!
	for subsystem in ['auto', 'console', 'windows'] {
		output := os.join_path(root, '${subsystem}.c')
		generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc',
			'none', '-os', 'windows', '-arch', 'amd64', '-subsystem', subsystem, '-run-only',
			'test_captured_failure', '-o', output, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(output)!
		if subsystem == 'windows' {
			assert code.contains('int WINAPI wWinMain('), 'explicit Windows subsystem must keep its GUI entry'
			assert code.contains('AttachConsole(ATTACH_PARENT_PROCESS)'), 'explicit GUI tests should keep the normal console attachment'
			assert !code.contains('AllocConsole()'), 'explicit GUI tests should not create a console'
		} else {
			assert code.contains('int wmain('), '${subsystem} tests must use the console entry'
			assert !code.contains('int WINAPI wWinMain('), '${subsystem} tests must not use the GUI entry'
			assert !code.contains('freopen_s(&res_fp,'), '${subsystem} tests must preserve captured streams'
		}
	}
	$if windows {
		output := os.join_path(root, 'capture_test.exe')
		compiled := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc',
			'none', '-cc', @CCOMPILER, '-subsystem', 'auto', '-run-only', 'test_captured_failure',
			'-skip-running', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		ran := os.exec([output])
		assert ran.exit_code == 1, ran.output
		for marker in ['captured test stdout', 'captured test stderr', 'captured test assertion'] {
			assert ran.output.contains(marker), ran.output
		}
	}
}
