module vhelp

import os

pub fn show_topic(topic string) {
	vexe := os.real_path(os.getenv('VEXE'))
	vroot := os.dir(vexe)
	mut topics := []string{}
	mut ptopics := &topics

	mut target_topic := ''

	topics_dir := os.join_path(vroot, 'vlib', 'v', 'help')

	os.walk(topics_dir, fn [mut ptopics] (topic string) {
		if os.file_ext(topic) == '.txt' {
			ptopics << os.file_name(topic).replace('.txt', '').replace('default,', '')
		}
	})
	for category, item in categories {
		if topic in item {
			target_topic = os.join_path(vroot, 'vlib', 'v', 'help', '$category/${topic}.txt')
			break
		}
	}
	// target_topic := os.join_path(vroot, 'vlib', 'v', 'help', '${topic}.txt')
	content := os.read_file(target_topic) or {
		eprintln('Unknown topic: ${topic}')
		exit(1)
	}
	println(content)
}
