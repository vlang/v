import json2
import os
import log
import rand

fn slog_setup() (string, string) {
	lfolder := os.join_path(os.vtmp_dir(), rand.ulid())
	lpath := os.join_path(lfolder, 'current.log')
	os.mkdir_all(lfolder) or { panic('mkdir: ${err}') }
	return lfolder, lpath
}

fn test_structured_json_fields() {
	lfolder, lpath := slog_setup()
	mut l := log.new_thread_safe_log()
	l.set_level(.debug)
	l.set_full_logpath(lpath)
	l.info_with('request done', {
		'method': 'GET'
		'path':   '/api/users'
		'status': '200'
	})
	l.flush()
	l.close()
	content := os.read_file(lpath) or { panic('read: ${err}') }
	assert content.starts_with('{'), 'structured output must be a JSON object'
	assert content.contains('"msg":"request done"')
	assert content.contains('"method":"GET"')
	assert content.contains('"path":"/api/users"')
	assert content.contains('"status":"200"')
	assert content.contains('"level":"info"')
	assert content.ends_with('\n'), 'one record per line'
	os.rmdir_all(lfolder) or {}
}

fn test_structured_stable_field_order() {
	lfolder, lpath := slog_setup()
	mut l := log.new_thread_safe_log()
	l.set_full_logpath(lpath)
	l.warn_with('m', {
		'zeta':  '1'
		'alpha': '2'
	})
	l.flush()
	l.close()
	content := os.read_file(lpath) or { panic('read: ${err}') }
	a := content.index('"alpha"') or { -1 }
	z := content.index('"zeta"') or { -1 }
	assert a >= 0 && z >= 0 && a < z, 'fields must be sorted by key'
	os.rmdir_all(lfolder) or {}
}

fn test_structured_escapes_values() {
	lfolder, lpath := slog_setup()
	mut l := log.new_thread_safe_log()
	l.set_full_logpath(lpath)
	l.error_with('say "hi" \\ path', {
		'key': 'v"a\\l'
	})
	l.flush()
	l.close()
	content := os.read_file(lpath) or { panic('read: ${err}') }
	assert content.contains(r'"msg":"say \"hi\" \\ path"'), 'quotes and backslashes must be escaped'
	assert content.contains(r'"key":"v\"a\\l"')
	assert content.trim_space().all_before('\n') == content.trim_space(), 'escaping must not break the single-line record'
	os.rmdir_all(lfolder) or {}
}

fn test_structured_respects_level() {
	lfolder, lpath := slog_setup()
	mut l := log.new_thread_safe_log()
	l.set_level(.warn)
	l.set_full_logpath(lpath)
	l.info_with('hidden', {
		'k': 'v'
	})
	l.warn_with('shown', {
		'k': 'v'
	})
	l.flush()
	l.close()
	content := os.read_file(lpath) or { panic('read: ${err}') }
	assert !content.contains('hidden'), 'level filtering must apply to structured calls'
	assert content.contains('shown')
	os.rmdir_all(lfolder) or {}
}

fn test_plain_text_mode_unchanged() {
	lfolder, lpath := slog_setup()
	mut l := log.new_thread_safe_log()
	l.set_full_logpath(lpath)
	l.info('plain message')
	l.flush()
	l.close()
	content := os.read_file(lpath) or { panic('read: ${err}') }
	assert !content.starts_with('{'), 'plain methods must stay text-formatted'
	assert content.contains('plain message')
	assert content.contains('[INFO'), 'plain tag must be unchanged'
	os.rmdir_all(lfolder) or {}
}

fn test_structured_control_characters_and_reserved_keys() {
	lfolder, lpath := slog_setup()
	defer { os.rmdir_all(lfolder) or {} }
	mut l := log.new_thread_safe_log()
	l.set_full_logpath(lpath)
	controls := []u8{len: 32, init: u8(index)}.bytestr()
	l.info_with(controls, {
		'time':   'fake time'
		'level':  'fake level'
		'msg':    'fake message'
		controls: controls
	})
	l.close()
	content := os.read_file(lpath)!
	record := json2.decode[map[string]string](content)!
	assert record['msg'] == controls
	assert record['level'] == 'info'
	assert record['time'] != 'fake time'
	assert record[controls] == controls
	assert content.count('\n') == 1
}

fn test_structured_thread_safe_records() {
	lfolder, lpath := slog_setup()
	defer { os.rmdir_all(lfolder) or {} }
	mut l := log.new_thread_safe_log()
	l.set_full_logpath(lpath)
	mut tasks := []thread{}
	for worker in 0 .. 4 {
		tasks << spawn fn [mut l, worker] () {
			for entry in 0 .. 25 {
				l.info_with('parallel', {
					'worker': worker.str()
					'entry':  entry.str()
				})
			}
		}()
	}
	tasks.wait()
	l.close()
	lines := os.read_lines(lpath)!
	assert lines.len == 100
	mut seen := map[string]bool{}
	for line in lines {
		record := json2.decode[map[string]string](line)!
		assert record['msg'] == 'parallel'
		seen[record['worker'] + ':' + record['entry']] = true
	}
	assert seen.len == 100
}
