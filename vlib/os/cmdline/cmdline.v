module cmdline

pub fn many_values(args []string, optname string) []string {
	mut flags := []string
	for ci, cv in args {
		if cv == optname {
			if ci + 1 < args.len {
				flags << args[ci + 1]
			}
		}
	}
	return flags
}

pub fn option(args []string, param string, def string) string {
	mut found := false
	for arg in args {
		if found {
			return arg
		}
		else if param == arg {
			found = true
		}
	}
	return def
}

pub fn before(args []string, what []string) []string {
	mut found := false
	mut args_before := []string
	for a in args {
		if a in what {
			found = true
			break
		}
		args_before << a
	}
	return args_before
}

pub fn after(args []string, what []string) []string {
	mut found := false
	mut args_after := []string
	for a in args {
		if a in what {
			found = true
			continue
		}
		if found {
			args_after << a
		}
	}
	return args_after
}

pub fn only_non_options(args []string) []string {
	return args.filter(!it.starts_with('-'))
}

pub fn only_options(args []string) []string {
	return args.filter(it.starts_with('-'))
}
