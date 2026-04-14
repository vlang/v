import os

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const labelled_continue_scope_testdata = os.join_path(vroot, 'vlib/v/gen/c/testdata/labelled_continue_scope.vv')

struct LabelledContinueCase {
	label    string
	var_name string
}

fn test_labelled_continue_targets_reenter_at_the_loop_gate() {
	os.chdir(vroot) or {}
	cmd := '${os.quoted_path(vexe)} -o - ${os.quoted_path(labelled_continue_scope_testdata)}'
	compilation := os.execute(cmd)
	assert compilation.exit_code == 0
	generated_c := compilation.output.replace('\r\n', '\n')
	cases := [
		LabelledContinueCase{'range_outer', 'issue_19973_range_var'},
		LabelledContinueCase{'cond_outer', 'issue_19973_cond_var'},
		LabelledContinueCase{'c_outer', 'issue_19973_c_var'},
		LabelledContinueCase{'c_multi_outer', 'issue_19973_c_multi_var'},
	]
	for tc in cases {
		continue_flag := 'v__labeled_continue_${tc.label}'
		continue_entry_label := '${tc.label}__continue_entry: {}'
		continue_label := '${tc.label}__continue: {}'
		continue_gate := 'if (${continue_flag}) goto ${tc.label}__continue;'
		continue_assignment := '${continue_flag} = true;'
		continue_goto := 'goto ${tc.label}__continue_entry;'
		var_decl := 'string ${tc.var_name} ='
		assert generated_c.contains('bool ${continue_flag} = false;')
		assert generated_c.contains(continue_entry_label)
		assert generated_c.contains(continue_gate)
		assert generated_c.contains(continue_assignment)
		assert generated_c.contains(continue_goto)
		flag_idx := generated_c.index('bool ${continue_flag} = false;') or {
			panic('missing continue flag for `${tc.label}`')
		}
		entry_idx := generated_c.index(continue_entry_label) or {
			panic('missing continue entry label for `${tc.label}`')
		}
		var_idx := generated_c.index(var_decl) or {
			panic('missing declaration for `${tc.var_name}`')
		}
		label_idx := generated_c.index(continue_label) or {
			panic('missing continue label for `${tc.label}`')
		}
		assert flag_idx < entry_idx
		assert entry_idx < var_idx
		assert var_idx < label_idx
	}
}
