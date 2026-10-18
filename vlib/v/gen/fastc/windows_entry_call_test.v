module fastc

import v.pref

fn entry_call_prefs(target_os string, is_test bool) &pref.Preferences {
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.is_test = is_test
	prefs.target = pref.target_from(target_os, 'amd64') or { panic(err) }
	return prefs
}

// A reference to the program's `main` must resolve to the same C symbol its
// definition uses: `wmain` on Windows (the -municode entry), `main` elsewhere.
fn test_main_references_resolve_to_the_windows_entry_name() {
	source := 'module main\n\nfn again() {\n\tmain()\n}\n\nfn main() {\n\tf := main\n\tf()\n\tagain()\n}\n'
	windows := generate(source, 'entry_call.v', entry_call_prefs('windows', false)) or {
		panic(err)
	}
	assert windows.contains('int wmain(void) {'), windows
	assert windows.contains('\twmain();'), windows
	assert windows.contains('= (wmain);'), windows
	assert !windows.contains('\tmain();') && !windows.contains('(main)'), windows

	linux := generate(source, 'entry_call.v', entry_call_prefs('linux', false)) or { panic(err) }
	assert linux.contains('int main(void) {'), linux
	assert linux.contains('\tmain();'), linux
	assert !linux.contains('wmain'), linux
}

// In a test build the synthesized test main is the entry (`wmain` on Windows) and
// the program's `main` is an ordinary function: its definition and its references
// keep sharing the ordinary program-main symbol, never the entry name.
fn test_main_stays_ordinary_in_test_builds() {
	source := 'module main\n\nfn main() {\n\tprintln("ordinary")\n}\n\nfn test_calls_main() {\n\tmain()\n}\n'
	windows := generate(source, 'entry_call_test.v', entry_call_prefs('windows', true)) or {
		panic(err)
	}
	assert windows.contains('int wmain(void) {'), windows
	assert windows.contains('void v_fastc_program_main(void) {'), windows
	assert windows.contains('\tv_fastc_program_main();'), windows
	assert !windows.contains('\twmain();'), windows
}
