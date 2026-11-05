module fastc

import os
import v.pref
import v.cmdexec

fn test_fastc_full_runtime_honors_module_search_paths() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_module_search_path_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'encoding')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'consumer')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'encoding', 'encoder.v'), r'module encoding
pub struct TextEncoder {}
pub fn (_ TextEncoder) encode() string { return "project encoding" }
') or { panic(err) }
	os.write_file(os.join_path(root, 'consumer', 'consumer.v'), r'module consumer
import encoding
pub fn encode(value encoding.TextEncoder) string { return value.encode() }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import encoding
import consumer
fn main() { println(consumer.encode(encoding.TextEncoder{})) }
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.module_search_paths = [root, os.join_path(prefs.vroot, 'vlib')]
	for selfhost in [false, true] {
		prefs.selfhost = selfhost
		mut cache := map[string]string{}
		for source in ['main.v', 'consumer/consumer.v'] {
			path := os.join_path(root, source)
			key := fastc_module_cache_key(prefs, path, 'encoding')
			resolved := fastc_resolve_module_dir(key, 'encoding', path, prefs, '', mut cache)
			assert resolved == os.real_path(os.join_path(root, 'encoding')), resolved
		}
		assert cache.len == 2
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-path', '${root}|@vmodules|@vlib', '-nocache',
		'-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'project encoding', run.output
}

fn test_fastc_resolve_memos_distinguish_search_path_order_and_selfhosting() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	paths := [os.join_path(os.vtmp_dir(), 'fastc_memo_search', 'main.v')]
	default_memo := fastc_resolve_memo_path_of_real(paths, prefs)
	prefs.selfhost = true
	selfhost_memo := fastc_resolve_memo_path_of_real(paths, prefs)
	assert selfhost_memo != default_memo
	prefs.module_search_paths = ['/first', '/second']
	ordered_memo := fastc_resolve_memo_path_of_real(paths, prefs)
	assert ordered_memo != selfhost_memo
	prefs.module_search_paths = ['/second', '/first']
	assert fastc_resolve_memo_path_of_real(paths, prefs) != ordered_memo
}
