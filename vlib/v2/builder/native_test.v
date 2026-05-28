module builder

import v2.abi
import v2.gen.x64
import v2.pref

fn test_native_x64_windows_selects_coff_and_windows_abi() {
	assert is_windows_x64_native_target(.x64, 'windows')
	assert native_x64_object_format_for_os('windows') == x64.ObjectFormat.coff
	assert native_x64_codegen_abi_for_os('windows') == x64.X64Abi.windows
	assert native_x64_lowering_abi_for_os('windows') == abi.X64Abi.windows
}

fn test_native_x64_non_windows_keeps_existing_object_formats_and_sysv() {
	assert !is_windows_x64_native_target(.x64, 'linux')
	assert native_x64_object_format_for_os('linux') == x64.ObjectFormat.elf
	assert native_x64_codegen_abi_for_os('linux') == x64.X64Abi.sysv
	assert native_x64_lowering_abi_for_os('linux') == abi.X64Abi.sysv

	assert !is_windows_x64_native_target(.x64, 'macos')
	assert native_x64_object_format_for_os('macos') == x64.ObjectFormat.macho
	assert native_x64_codegen_abi_for_os('macos') == x64.X64Abi.sysv
	assert native_x64_lowering_abi_for_os('macos') == abi.X64Abi.sysv
	assert is_macos_native_target('macos')
	assert is_macos_native_target('darwin')
	assert native_x64_object_format_for_os('darwin') == x64.ObjectFormat.macho
	assert native_x64_codegen_abi_for_os('darwin') == x64.X64Abi.sysv
	assert native_x64_lowering_abi_for_os('darwin') == abi.X64Abi.sysv
}

fn test_minimal_windows_runtime_is_not_enabled_for_linux_or_macos() {
	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64

	prefs.target_os = 'linux'
	linux_builder := new_builder(&prefs)
	assert !linux_builder.uses_minimal_windows_x64_runtime()

	prefs.target_os = 'macos'
	macos_builder := new_builder(&prefs)
	assert !macos_builder.uses_minimal_windows_x64_runtime()

	prefs.target_os = 'darwin'
	darwin_builder := new_builder(&prefs)
	assert !darwin_builder.uses_minimal_windows_x64_runtime()

	prefs.target_os = 'windows'
	windows_builder := new_builder(&prefs)
	assert windows_builder.uses_minimal_windows_x64_runtime()
}
