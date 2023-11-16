module help

import os

// Topics whose module uses the cli module.
const cli_topics = ['new', 'init']

fn hdir(base string) string {
	return os.join_path(base, 'vlib', 'v', 'help')
}

fn help_dir() string {
	mut vexe := os.getenv('VEXE')
	if vexe == '' {
		vexe = os.executable()
	}
	if vexe != '' {
		return hdir(os.dir(vexe))
	}
	// use @VEXEROOT, but only if everything else fails; the negative of using it,
	// is that it will depend on the filesystem of the host that built v
	return hdir(@VEXEROOT)
}

@[params]
pub struct ExitOptions {
	exit_code int
}

// print_and_exit prints the help topic and exits.
@[noreturn]
pub fn print_and_exit(topic string, opts ExitOptions) {
	if topic == 'topics' {
		print_known_topics()
		exit(opts.exit_code)
	}
	fail_code := if opts.exit_code != 0 { opts.exit_code } else { 1 }
	for c in topic {
		if !c.is_letter() && !c.is_digit() && c != `-` {
			print_topic_unkown(topic)
			exit(fail_code)
		}
	}
	if topic in help.cli_topics {
		os.system('${@VEXE} ${topic} --help')
		exit(opts.exit_code)
	}
	mut topic_path := ''
	for path in os.walk_ext(help_dir(), '.txt') {
		if topic == os.file_name(path).all_before('.txt') {
			topic_path = path
			break
		}
	}
	if topic_path == '' {
		print_topic_unkown(topic)
		print_known_topics()
		exit(fail_code)
	}
	println(os.read_file(topic_path) or {
		eprintln('error: failed reading topic file: ${err}')
		exit(fail_code)
	})
	exit(opts.exit_code)
}

fn print_topic_unkown(topic string) {
	eprintln('error: unknown help topic "${topic}". Use `v help` for usage information.')
}

fn print_known_topics() {
	mut res := 'Known help topics: '
	topic_paths := os.walk_ext(help_dir(), '.txt')
	for i, path in topic_paths {
		topic := os.file_name(path).all_before('.txt')
		if topic != 'default' {
			res += topic + if i != topic_paths.len - 1 { ', ' } else { '.' }
		}
	}
	println(res)
}
