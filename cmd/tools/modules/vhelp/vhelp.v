module vhelp

import os

pub fn show_topic(topic string) {
	vexe := os.real_path(os.getenv('VEXE'))
	vroot := os.dir(vexe)
	topicdir := os.join_path(vroot, 'vlib', 'v', 'help')

	mut path_to := topic
	mut topics := os.walk_ext(topicdir, '.txt')
	mut items := [][]string{}

	// Getting the directory, splitting at `/`, reversing,
	// trimming to only indexes 0 and 1, and reversing that into the items array
	for mut item in topics {
		mut item_rev := item.split('/').reverse()
		item_rev.trim(2)
		items << item_rev.reverse()
	}

	// Getting the path to the help topic text file
	for cmds in items {
		if '${topic}.txt' in cmds {
			path_to = '${cmds[0]}/${cmds[1].replace('.txt', '')}'
			break
		}
	}

	topic_dir := if topic == 'default' {
		os.join_path(topicdir, 'default.txt')
	} else {
		os.join_path(topicdir, '${path_to}.txt')
	}

	content := os.read_file(topic_dir) or {
		eprintln('Unknown topic: ${topic}')
		exit(1)
	}
	println(content)
}
