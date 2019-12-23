module os

pub fn get_cmdline_multiple_values(args []string, optname string) []string {
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

pub fn get_cmdline_option(args []string, param string, def string) string {
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

pub fn get_args_before(args []string, what []string) []string {
	mut found := false
	mut args_before := []string
	for i, a in args {
		if a in what {
			found = true
			break
		}
		args_before << a
	}
	return args_before
}

pub fn get_args_after(args []string, what []string) []string {
	mut found := false
	mut args_after := []string
	for i, a in args {
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

pub fn get_non_options(args []string) []string {
	return args.filter(!it.starts_with('-'))
}

pub fn get_options(args []string) []string {
	return args.filter(it.starts_with('-'))
}
