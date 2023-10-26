module help

import os

const help_dir = os.join_path(@VEXEROOT, 'vlib', 'v', 'help')

// print_and_exit prints the help topic and exits.
pub fn print_and_exit(topic string) {
	if topic == 'topics' {
		print_known_topics()
		exit(0)
	}

	for c in topic {
		if !c.is_letter() && !c.is_digit() && c != `-` {
			print_topic_unkown(topic)
			exit(1)
		}
	}

	mut topic_path := ''
	for t in os.walk_ext(help.help_dir, '.txt') {
		if topic == t.all_after_last(os.path_separator)#[..-4] {
			topic_path = t
			break
		}
	}
	if topic_path == '' {
		print_topic_unkown(topic)
		print_known_topics()
		exit(1)
	}

	println(os.read_file(topic_path) or {
		eprintln('error: failed reading topic file: ${err}')
		exit(1)
	})
	exit(0)
}

fn print_topic_unkown(topic string) {
	eprintln('error: unknown help topic "${topic}". Use `v help` for usage information.')
}

fn print_known_topics() {
	mut res := 'Known help topics: '
	for t in os.walk_ext(help.help_dir, '.txt') {
		if t != 'default.txt' {
			res += os.file_name(t).replace('.txt', '') + ', '
		}
	}
	println(res#[..-2] + '.')
}
