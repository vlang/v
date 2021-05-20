import readline { Readline }

fn no_lines(s string) string {
	return s.replace('\n', ' ')
}

fn test_struct_readline() {
	// mut rl := Readline{}
	// eprintln('rl: $rl')
	// line := rl.read_line('Please, enter your name: ') or { panic(err) }
	// eprintln('line: $line')
	mut methods := []string{}
	$for method in Readline.methods {
		// eprintln('  method: $method.name | ' + no_lines('$method'))
		methods << method.name
	}
	// eprintln('methods: $methods')
	assert 'read_line_utf8' in methods
	assert 'read_line' in methods
}
