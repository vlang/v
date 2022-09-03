module vhelp

import os

pub fn show_topic(topic string) {
	vexe := os.real_path(os.getenv('VEXE'))
	vroot := os.dir(vexe)
	target_topic := os.join_path(vroot, 'cmd', 'v', 'help', '${topic}.txt')
	content := os.read_file(target_topic) or {
		eprintln('Unknown topic: $topic')
		exit(1)
	}
	println(content)
}
