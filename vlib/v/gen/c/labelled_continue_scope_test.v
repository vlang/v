import os

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const labelled_continue_scope_testdata = os.join_path(vroot,
	'vlib/v/gen/c/testdata/labelled_continue_scope.vv')

const named_break_continue_testdata = os.join_path(vroot,
	'vlib/v/tests/named_break_continue_test.v')

const goto_testdata = os.join_path(vroot, 'vlib/v/tests/goto_test.v')

const cstruct_goto_label_testdata = os.join_path(vroot,
	'vlib/v/tests/c_structs/cstruct_anon_test.c.v')

struct LabelledContinueCase {
	fn_name  string
	label    string
	var_name string
}

fn function_window(text string, marker string) string {
	pos := text.index(marker) or {
		assert false, 'missing marker: ${marker}'
		return ''
	}
	rest := text[pos + marker.len..]
	end_offset := rest.index('\nVV_LOC ') or { rest.len }
	return text[pos..pos + marker.len + end_offset]
}

fn function_window_containing(text string, marker string) string {
	pos := text.index(marker) or {
		assert false, 'missing marker: ${marker}'
		return ''
	}
	prefix := text[..pos]
	start := prefix.last_index('\nVV_LOC ') or { 0 }
	rest := text[start + 1..]
	end_offset := rest.index('\nVV_LOC ') or { rest.len }
	return rest[..end_offset]
}

fn braced_function_window(text string, marker string) string {
	start := text.index(marker) or {
		assert false, 'missing marker: ${marker}'
		return ''
	}
	open_offset := text[start..].index_u8(`{`)
	assert open_offset >= 0, 'missing function body for: ${marker}'
	mut depth := 0
	for i in start + open_offset .. text.len {
		if text[i] == `{` {
			depth++
		} else if text[i] == `}` {
			depth--
			if depth == 0 {
				return text[start..i + 1]
			}
		}
	}
	panic('unterminated function body for: ${marker}')
}

fn generate_c(path string, extra_flags string) string {
	os.chdir(vroot) or {}
	cmd := '${os.quoted_path(vexe)} -gc none ${extra_flags} -o - ${os.quoted_path(path)}'
	compilation := os.execute(cmd)
	assert compilation.exit_code == 0, '${cmd}\n${compilation.output}'
	return compilation.output.replace('\r\n', '\n')
}

fn assert_short_user_goto_identifiers(generated_c string) {
	mut found := false
	for token in generated_c.split_any(' \t\r\n;,:{}()[]=!*') {
		if token.starts_with('__v_user_goto_') {
			found = true
			assert token.len < 247, token
		}
	}
	assert found
}

fn assert_labeled_loop_control_family(fn_c string, base string) {
	continue_flag := '${base}__continue_flag'
	continue_entry := '${base}__continue_entry'
	continue_label := '${base}__continue'
	break_label := '${base}__break'
	assert fn_c.contains('${base}:')
	assert fn_c.contains('bool ${continue_flag} = false;')
	assert fn_c.contains('${continue_entry}: {}')
	assert fn_c.contains('if (${continue_flag}) goto ${continue_label};')
	assert fn_c.contains('${continue_flag} = true;')
	assert fn_c.contains('goto ${continue_entry};')
	assert fn_c.contains('${continue_label}: {}')
	assert fn_c.contains('goto ${break_label};')
	assert fn_c.contains('${break_label}: {}')
}

fn test_labelled_continue_targets_reenter_at_the_loop_gate() {
	generated_c := generate_c(labelled_continue_scope_testdata, '')
	cases := [
		LabelledContinueCase{'range_loop', 'range_outer', 'issue_19973_range_var'},
		LabelledContinueCase{'cond_loop', 'cond_outer', 'issue_19973_cond_var'},
		LabelledContinueCase{'c_loop', 'c_outer', 'issue_19973_c_var'},
		LabelledContinueCase{'c_multi_loop', 'c_multi_outer', 'issue_19973_c_multi_var'},
	]
	if generated_c.contains('void range_loop(void) {') {
		for tc in cases {
			fn_c := braced_function_window(generated_c, 'void ${tc.fn_name}(void) {')
			base := '__v_user_goto_0'
			continue_flag := '${base}__continue_flag'
			flag_decl := 'bool ${continue_flag} = false;'
			var_decl := 'string ${tc.var_name} ='
			continue_label := '${base}__continue: ;'
			assert fn_c.contains('${base}: ;')
			assert fn_c.contains(flag_decl)
			assert fn_c.contains('${continue_flag} = true;')
			assert fn_c.contains('goto ${base}__continue;')
			assert fn_c.contains(continue_label)
			assert fn_c.contains('${base}__break: ;')
			assert !fn_c.contains('${tc.label}:')
			assert fn_c.index(flag_decl)? < fn_c.index(var_decl)?
			assert fn_c.index(var_decl)? < fn_c.index(continue_label)?
		}
		return
	}
	for tc in cases {
		fn_c := function_window(generated_c, 'void main__${tc.fn_name}(void) {')
		base := '__v_user_goto_0'
		continue_flag := '${base}__continue_flag'
		continue_entry_label := '${base}__continue_entry: {}'
		continue_label := '${base}__continue: {}'
		continue_gate := 'if (${continue_flag}) goto ${base}__continue;'
		continue_assignment := '${continue_flag} = true;'
		continue_goto := 'goto ${base}__continue_entry;'
		var_decl := 'string ${tc.var_name} ='
		assert fn_c.contains('bool ${continue_flag} = false;')
		assert fn_c.contains(continue_entry_label)
		assert fn_c.contains(continue_gate)
		assert fn_c.contains(continue_assignment)
		assert fn_c.contains(continue_goto)
		assert !fn_c.contains('${tc.label}__')
		assert !fn_c.contains('v__labeled_continue_${tc.label}')
		flag_idx := fn_c.index('bool ${continue_flag} = false;') or {
			panic('missing continue flag for `${tc.label}`')
		}
		entry_idx := fn_c.index(continue_entry_label) or {
			panic('missing continue entry label for `${tc.label}`')
		}
		var_idx := fn_c.index(var_decl) or { panic('missing declaration for `${tc.var_name}`') }
		label_idx := fn_c.index(continue_label) or {
			panic('missing continue label for `${tc.label}`')
		}
		assert flag_idx < entry_idx
		assert entry_idx < var_idx
		assert var_idx < label_idx
	}
	assert_short_user_goto_identifiers(generated_c)
}

fn test_all_labeled_loop_forms_share_short_ordinal_control_names() {
	generated_c := generate_c(named_break_continue_testdata, '')
	if generated_c.contains('void test_labelled_for(void) {') {
		fn_c := braced_function_window(generated_c, 'void test_labelled_for(void) {')
		for i, source_label in ['L1', 'L2', 'L3', 'L4'] {
			base := '__v_user_goto_${i}'
			continue_flag := '${base}__continue_flag'
			assert fn_c.contains('goto ${base};')
			assert fn_c.contains('${base}: ;')
			assert fn_c.contains('bool ${continue_flag} = false;')
			assert fn_c.contains('${continue_flag} = true;')
			assert fn_c.contains('goto ${base}__continue;')
			assert fn_c.contains('${base}__continue: ;')
			assert fn_c.contains('${base}__break: ;')
			assert !fn_c.contains('${source_label}:')
		}
		return
	}
	fn_c := function_window(generated_c, 'void main__test_labelled_for(void) {')
	for i, source_label in ['L1', 'L2', 'L3', 'L4'] {
		base := '__v_user_goto_${i}'
		assert fn_c.contains('goto ${base};')
		assert_labeled_loop_control_family(fn_c, base)
		assert !fn_c.contains('${source_label}__')
		assert !fn_c.contains('v__labeled_continue_${source_label}')
	}
	assert_short_user_goto_identifiers(fn_c)
}

fn test_ordinary_goto_labels_reset_for_each_function() {
	generated_c := generate_c(goto_testdata, '')
	if generated_c.contains('void test_goto(void) {') {
		for signature, source_label in {
			'void test_goto(void) {':                    'a'
			'void test_goto_after_return(void) {':       'finally_ok'
			'void test_goto_with_comptime_tmpl(void) {': 'label'
		} {
			fn_c := braced_function_window(generated_c, signature)
			assert fn_c.contains('goto __v_user_goto_0;')
			assert fn_c.contains('__v_user_goto_0: ;')
			assert !fn_c.contains('goto ${source_label};')
		}
		return
	}
	for signature in [
		'void main__test_goto(void) {',
		'void main__test_goto_after_return(void) {',
		'void main__test_goto_with_comptime_tmpl(void) {',
	] {
		fn_c := function_window(generated_c, signature)
		assert fn_c.contains('goto __v_user_goto_0;')
		assert fn_c.contains('__v_user_goto_0: {}')
		assert !fn_c.contains('__v_user_goto_1')
	}
	assert_short_user_goto_identifiers(generated_c)
}

fn test_cstruct_goto_labels_are_short_collision_free_and_scoped() {
	generated_c := generate_c(cstruct_goto_label_testdata, '-cc msvc -os windows')
	if generated_c.contains('int ordinary_labels_with_c_name_collisions_and_hostile_macro(void) {') {
		ordinary_fn := braced_function_window(generated_c,
			'int ordinary_labels_with_c_name_collisions_and_hostile_macro(void) {')
		for i in 0 .. 3 {
			base := '__v_user_goto_${i}'
			assert ordinary_fn.count('goto ${base};') == 1
			assert ordinary_fn.count('${base}: ;') == 1
		}
		assert !ordinary_fn.contains('goto class;')
		assert !ordinary_fn.contains('goto __v_class;')
		assert !ordinary_fn.contains('macro_target')

		loop_fn := braced_function_window(generated_c,
			'int loop_head_labels_with_c_name_collisions(void) {')
		for i in 0 .. 3 {
			base := '__v_user_goto_${i}'
			assert loop_fn.contains('goto ${base};')
			assert loop_fn.contains('${base}: ;')
			assert loop_fn.contains('bool ${base}__continue_flag = false;')
			assert loop_fn.contains('goto ${base}__continue;')
			assert loop_fn.contains('${base}__continue: ;')
			assert loop_fn.contains('${base}__break: ;')
		}

		long_fn := braced_function_window(generated_c,
			'int long_labels_and_source_generated_name_collision(void) {')
		for i in 0 .. 3 {
			base := '__v_user_goto_${i}'
			assert long_fn.contains('${base}: ;')
			assert long_fn.count('goto ${base};') == 1
		}
		assert !long_fn.contains('long_label_aaaaaaaa')

		for signature in [
			'int labels_reset_in_first_function(void) {',
			'int labels_reset_in_second_function(void) {',
		] {
			fn_c := braced_function_window(generated_c, signature)
			assert fn_c.contains('__v_user_goto_0: ;')
			assert !fn_c.contains('__v_user_goto_1')
		}

		generic_string_fn := braced_function_window(generated_c,
			'int generic_labeled_loop_with_selected_goto_T_string(void) {')
		generic_int_fn := braced_function_window(generated_c,
			'int generic_labeled_loop_with_selected_goto_T_v_int(void) {')
		assert !generic_string_fn.contains('goto __v_user_goto_0;')
		assert generic_string_fn.contains('__v_user_goto_0:')
		assert generic_int_fn.contains('goto __v_user_goto_0;')
		assert generic_int_fn.contains('__v_user_goto_0:')
		assert_short_user_goto_identifiers(generated_c)
		return
	}

	ordinary_fn := function_window(generated_c,
		'int main__ordinary_labels_with_c_name_collisions_and_hostile_macro(void) {')
	for i in 0 .. 3 {
		base := '__v_user_goto_${i}'
		assert ordinary_fn.count('goto ${base};') == 1
		assert ordinary_fn.count('${base}: {}') == 1
	}
	assert !ordinary_fn.contains('goto class;')
	assert !ordinary_fn.contains('goto __v_class;')
	assert !ordinary_fn.contains('v__user_goto_macro_target')

	loop_fn := function_window(generated_c,
		'int main__loop_head_labels_with_c_name_collisions(void) {')
	for i in 0 .. 3 {
		base := '__v_user_goto_${i}'
		assert loop_fn.contains('goto ${base};')
		assert_labeled_loop_control_family(loop_fn, base)
	}

	long_left := 'long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax'
	long_right := 'long_label_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay'
	assert long_left.len >= 127
	assert long_right.len >= 127
	legacy_left := '__v_user_goto_${long_left.bytes().hex()}'
	legacy_right := '__v_user_goto_${long_right.bytes().hex()}'
	assert legacy_left.len > 247
	assert legacy_right.len > 247
	assert legacy_left[..247] == legacy_right[..247]
	long_fn := function_window(generated_c,
		'int main__long_labels_and_source_generated_name_collision(void) {')
	for i in 0 .. 3 {
		base := '__v_user_goto_${i}'
		assert long_fn.contains('${base}: {}')
	}
	assert long_fn.count('goto __v_user_goto_0;') == 1
	assert long_fn.count('goto __v_user_goto_1;') == 1
	assert long_fn.count('goto __v_user_goto_2;') == 1
	assert !long_fn.contains(long_left)
	assert !long_fn.contains(long_right)

	for signature in [
		'int main__labels_reset_in_first_function(void) {',
		'int main__labels_reset_in_second_function(void) {',
	] {
		fn_c := function_window(generated_c, signature)
		assert fn_c.contains('__v_user_goto_0: {}')
		assert !fn_c.contains('__v_user_goto_1')
	}

	outer_fn := function_window_containing(generated_c, 'reached += 10001;')
	closure_fn := function_window_containing(generated_c, 'inner += 101;')
	nested_fn := function_window_containing(generated_c, 'value += 1001;')
	assert outer_fn.contains('__v_user_goto_0: {}')
	assert outer_fn.contains('__v_user_goto_1: {}')
	assert !outer_fn.contains('__v_user_goto_2')
	assert closure_fn.contains('__v_user_goto_0: {}')
	assert closure_fn.contains('__v_user_goto_1: {}')
	assert !closure_fn.contains('__v_user_goto_2')
	assert nested_fn.contains('__v_user_goto_0: {}')
	assert !nested_fn.contains('__v_user_goto_1')

	generic_string_fn := function_window(generated_c,
		'int main__generic_labeled_loop_with_selected_goto_T_string(void) {')
	generic_int_fn := function_window(generated_c,
		'int main__generic_labeled_loop_with_selected_goto_T_int(void) {')
	assert !generic_string_fn.contains('goto __v_user_goto_0;')
	assert generic_string_fn.contains('__v_user_goto_0:')
	assert generic_int_fn.contains('goto __v_user_goto_0;')
	assert generic_int_fn.contains('__v_user_goto_0:')

	assert_short_user_goto_identifiers(generated_c)
}
