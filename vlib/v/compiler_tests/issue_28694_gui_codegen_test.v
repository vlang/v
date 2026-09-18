import os

// These probes isolate patterns from #28694 without installing gui or vglyph.
// Each subprocess explicitly selects the new compiler, so fallback cannot mask
// a generated-C failure.
fn run_issue_28694_probe(name string, source string, files map[string]string) {
	temp_dir := os.join_path(os.temp_dir(), 'v_issue_28694_${name}_${os.getpid()}')
	os.rmdir_all(temp_dir) or {}
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	old_report_disabled := os.getenv('V_C_ERROR_BUG_REPORT_DISABLED')
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	defer {
		os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', old_report_disabled, true)
	}
	os.write_file(os.join_path(temp_dir, 'v.mod'), "Module { name: 'issue_28694' }\n") or {
		panic(err)
	}
	for relative_path, contents in files {
		path := os.join_path(temp_dir, relative_path)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, contents) or { panic(err) }
	}
	source_path := os.join_path(temp_dir, 'main.c.v')
	output_path := os.join_path(temp_dir, if os.user_os() == 'windows' { 'probe.exe' } else { 'probe' })
	os.write_file(source_path, source) or { panic(err) }
	build := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert build.exit_code == 0, '${name}: ${build.output}'
	run := os.execute(os.quoted_path(output_path))
	assert run.exit_code == 0, '${name}: ${run.output}'
}

fn test_issue_28694_urllib_result_bool() {
	run_issue_28694_probe('urllib', 'import net.urllib

fn main() {
	mut url := urllib.parse(\'https://example.com/a%20b\') or { panic(err) }
	assert url.path == \'/a b\'
	ok := url.set_path(\'/next%2Fpath\') or { panic(err) }
	assert ok
	assert url.path == \'/next/path\'
	if _ := url.set_path(\'/bad%ZZ\') {
		assert false, \'invalid escaping must return an error\'
	} else {
		assert err.msg().len > 0
	}
}
', map[string]string{})
}

fn test_issue_28694_imported_fixed_array_constant() {
	run_issue_28694_probe('constant', 'import tables

fn main() {
	for i in 0 .. 256 {
		assert tables.lookup(u8(i)) == u8(255 - i)
	}
}
', {
		'tables/tables.v': 'module tables

const gamma_table = make_gamma_table()

fn make_gamma_table() [256]u8 {
	mut table := [256]u8{}
	for i in 0 .. table.len {
		table[i] = u8(255 - i)
	}
	return table
}

pub fn lookup(value u8) u8 {
	return gamma_table[value]
}
'
	})
}

fn test_issue_28694_forwarded_c_callbacks() {
	run_issue_28694_probe('callbacks', '#include "@VMODROOT/callbacks.h"

@[typedef]
struct C.GuiCallbacks {
	on_marked_text fn (&char, int, voidptr)
	on_insert_text fn (&char, voidptr)
	on_clause      fn (int, int, int, voidptr)
}

fn C.gui_invoke_callbacks(callbacks &C.GuiCallbacks, user_data voidptr)

struct CallbackState {
mut:
	marked_byte   u8
	selection     int
	inserted_byte u8
	clause_start  int
	clause_end    int
	clause_style  int
}

fn marked_text(text &char, selection int, data voidptr) {
	mut state := unsafe { &CallbackState(data) }
	state.marked_byte = unsafe { u8(text[0]) }
	state.selection = selection
}

fn insert_text(text &char, data voidptr) {
	mut state := unsafe { &CallbackState(data) }
	state.inserted_byte = unsafe { u8(text[0]) }
}

fn clause(start int, end int, style int, data voidptr) {
	mut state := unsafe { &CallbackState(data) }
	state.clause_start = start
	state.clause_end = end
	state.clause_style = style
}

fn make_callbacks(on_marked_text fn (&char, int, voidptr),
	on_insert_text fn (&char, voidptr),
	on_clause fn (int, int, int, voidptr)) C.GuiCallbacks {
	return C.GuiCallbacks{
		on_marked_text: on_marked_text
		on_insert_text: on_insert_text
		on_clause: on_clause
	}
}

fn main() {
	callbacks := make_callbacks(marked_text, insert_text, clause)
	mut state := CallbackState{}
	C.gui_invoke_callbacks(&callbacks, &state)
	assert state.marked_byte == `m`
	assert state.selection == -7
	assert state.inserted_byte == `i`
	assert state.clause_start == -5
	assert state.clause_end == 9
	assert state.clause_style == -11
}
', {
		'callbacks.h': '#ifndef ISSUE_28694_CALLBACKS_H
#define ISSUE_28694_CALLBACKS_H

typedef struct GuiCallbacks {
    void (*on_marked_text)(const char *, int, void *);
    void (*on_insert_text)(const char *, void *);
    void (*on_clause)(int, int, int, void *);
} GuiCallbacks;

static void gui_invoke_callbacks(const GuiCallbacks *callbacks, void *user_data) {
    callbacks->on_marked_text("marked", -7, user_data);
    callbacks->on_insert_text("inserted", user_data);
    callbacks->on_clause(-5, 9, -11, user_data);
}

#endif
'
	})
}
