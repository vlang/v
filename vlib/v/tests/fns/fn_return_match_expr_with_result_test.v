fn test_fn_return_match_expr_with_result() {
	install_command(['github:some/thing'], 1) or { panic(err) }
	assert true
}

pub fn install_command(args []string, verbosity int) ! {
	if args.len == 0 {
		return error('${@FN} requires an argument')
	}
	component := args[0]
	if component.count(':') == 0 {
		return install_command(['github:${component}'], verbosity)
	}
	source := component.all_before(':')
	if source != 'github' {
		return error('${@FN} unknown source `${source}`')
	}
	unit := component.all_after(':')
	return match source {
		'github' {
			install_from_github(unit, verbosity)
		}
		else {
			error('${@FN} unknown source `${source}`')
		}
	}
}

fn install_from_github(unit string, verbosity int) ! {
	dump(unit)
}

fn branch_result(fail bool) !int {
	if fail {
		return error('branch failure')
	}
	return 42
}

fn returned_branch_result(n int) !int {
	return match n {
		1 {
			if n > 0 { branch_result(false) } else { error('inner failure') }
		}
		else {
			branch_result(true)
		}
	}
}

fn test_return_match_expr_result_preserves_success_and_error() {
	assert (returned_branch_result(1) or { panic(err) }) == 42
	mut message := ''
	value := returned_branch_result(2) or {
		message = err.msg()
		-1
	}
	assert value == -1
	assert message == 'branch failure'
}
