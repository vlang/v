module help

import os

const (
	unknown_topic = '`v help`: unknown help topic provided. Use `v help` for usage information.'
)

// print_and_exit Prints the help topic and exits
pub fn print_and_exit(topic string) {
	vexe := @VEXE
	vroot := os.dir(vexe)
	topicdir := os.join_path(vroot, 'vlib', 'v', 'help')

	for b in topic {
		if (b >= `a` && b <= `z`) || b == `-` || (b >= `0` && b <= `9`) {
			continue
		}
		eprintln(help.unknown_topic)
		exit(1)
	}

	mut path_to := topic
	mut topics := os.walk_ext(topicdir, '.txt')
	mut items := [][]string{}

	mut item_rev := []string{}
	mut delim := ''

	// Getting the directory, splitting at `/`, trimming to only indexes 0 and 1,
	// and reversing that into the items array
	for mut item in topics {
		$if windows {
			delim = '\\'
		} $else {
			delim = '/'
		}
		item_rev = item.split(delim).reverse()
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

	if topic == 'topics' {
		println(known_topics(topicdir))
		exit(0)
	}

	content := os.read_file(topic_dir) or {
		eprintln(help.unknown_topic)
		eprintln(known_topics(topicdir))
		exit(1)
	}
	println(content)
	exit(0)
}

// known_topics Getting topics known to V
fn known_topics(topicdir string) string {
	mut res := []string{}
	res << 'Known help topics: '

	mut topics := os.walk_ext(topicdir, '.txt').map(os.file_name(it).replace('.txt', ''))
	topics.sort()
	res << topics.join(', ')
	res << '.'
	return res.join('').replace('default, ', '')
}
