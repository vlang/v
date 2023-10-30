module help

import os

const help_dir = os.join_path(@VEXEROOT, 'vlib', 'v', 'help')

[params]
pub struct ExitOptions {
	exit_code int
}

// print_and_exit prints the help topic and exits.
[noreturn]
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
	mut topic_path := ''
	for path in os.walk_ext(help.help_dir, '.txt') {
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
	topic_paths := os.walk_ext(help.help_dir, '.txt')
	for i, path in topic_paths {
		topic := os.file_name(path).all_before('.txt')
		if topic != 'default' {
			res += topic + if i != topic_paths.len - 1 { ', ' } else { '.' }
		}
	}
	println(res)
}
