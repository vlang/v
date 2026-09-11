module c

import v3.pref

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
