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
