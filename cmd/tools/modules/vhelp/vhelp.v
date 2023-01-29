module vhelp

import os

pub fn show_topic(topic string) {
	vexe := os.real_path(os.getenv('VEXE'))
	vroot := os.dir(vexe)

	mut target_topic := ''
	for category, item in categories {
		if topic in item {
			target_topic = os.join_path(vroot, 'vlib', 'v', 'help', '${category}/${topic}.txt')
			break
		} else if topic == category {
			target_topic = os.join_path(vroot, 'vlib', 'v', 'help', '$category/${category}.txt')
			break
		}
	}

	content := os.read_file(target_topic) or {
		eprintln('Unknown topic: ${topic}')
		exit(1)
	}
	println(content)
}
