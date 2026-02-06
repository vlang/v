module cmdline

// Fetch multiple option by param, e.g.
// args: ['v', '-d', 'aa', '-d', 'bb', '-d', 'cc']
// param: '-d'
// ret: ['aa', 'bb', 'cc']
pub fn options(args []string, param string) []string {
	mut flags := []string{}
	for i, v in args {
		if v == param {
			if i + 1 < args.len {
				flags << args[i + 1]
			}
		}
	}
	return flags
}

// Fetch option by param, e.g.
// args: ['v', '-d', 'aa']
// param: '-d'
// def: ''
// ret: 'aa'
pub fn option(args []string, param string, def string) string {
	mut found := false
	for arg in args {
		if found {
			return arg
		} else if param == arg {
			found = true
		}
	}
	return def
}

// Fetch all options before what params, e.g.
// args: ['-stat', 'test', 'aaa.v']
// what: ['test']
// ret: ['-stat']
pub fn options_before(args []string, what []string) []string {
	mut args_before := []string{}
	for a in args {
		if a in what {
			break
		}
		args_before << a
	}
	return args_before
}

// Fetch all options after what params, e.g.
// args: ['-stat', 'test', 'aaa.v']
// what: ['test']
// ret: ['aaa.v']
pub fn options_after(args []string, what []string) []string {
	mut found := false
	mut args_after := []string{}
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

// Fetch all options not start with '-', e.g.
// args: ['-d', 'aa', '--help', 'bb']
// ret: ['aa', 'bb']
pub fn only_non_options(args []string) []string {
	return args.filter(!it.starts_with('-'))
}

// Fetch all options start with '-', e.g.
// args: ['-d', 'aa', '--help', 'bb']
// ret: ['-d', '--help']
pub fn only_options(args []string) []string {
	return args.filter(it.starts_with('-'))
}
