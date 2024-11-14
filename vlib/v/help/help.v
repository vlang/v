module help

import os

// Topics whose module uses the cli or flag modules (both support --help).
const cli_topics = ['new', 'init', 'repeat']

fn hdir(base string) string {
	return os.join_path(base, 'vlib', 'v', 'help')
}

fn help_dir() string {
	vexe := get_vexe()
	if vexe != '' {
		return hdir(os.dir(vexe))
	}
	// use @VEXEROOT, but only if everything else fails; the negative of using it,
	// is that it will depend on the filesystem of the host that built v
	return hdir(@VEXEROOT)
}

@[params]
pub struct ExitOptions {
pub:
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
			print_topic_unknown(topic)
			exit(fail_code)
		}
	}
	if topic in cli_topics {
		vexe := get_vexe()
		os.system('${os.quoted_path(vexe)} ${topic} --help')
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
		print_topic_unknown(topic)
		print_known_topics()
		exit(fail_code)
	}
	topic_content := os.read_file(topic_path) or {
		msg := err.str()
		eprintln('error: failed reading topic file: ${msg}')
		exit(fail_code)
	}
	cleaned := topic_content.trim_space()
	println(cleaned)
	exit(opts.exit_code)
}

fn print_topic_unknown(topic string) {
	eprintln('error: unknown help topic "${topic}". Use `v help` for usage information.')
}

fn print_known_topics() {
	topic_paths := os.walk_ext(help_dir(), '.txt')
	mut res := []string{}
	for path in topic_paths {
		topic := os.file_name(path).all_before('.txt')
		if topic != 'default' {
			res << topic
		}
	}
	sorted_topics := res.sorted()
	println('Known help topics: ${sorted_topics}')
}

fn get_vexe() string {
	mut vexe := os.getenv('VEXE')
	if vexe == '' {
		vexe = os.executable()
	}
	return vexe
}
