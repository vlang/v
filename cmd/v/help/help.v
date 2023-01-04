module help

// TODO: move this file outside internal, and merge it with cmd/tools/modules/vhelp/vhelp.v .
import os
import v.pref

const (
	unknown_topic = '`v help`: unknown help topic provided. Use `v help` for usage information.'
)

pub fn print_and_exit(topic string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	topicdir := os.join_path(vroot, 'cmd', 'v', 'help')

	for b in topic {
		if (b >= `a` && b <= `z`) || b == `-` || (b >= `0` && b <= `9`) {
			continue
		}
		eprintln(help.unknown_topic)
		exit(1)
	}
	// `init` has the same help topic as `new`
	name := if topic == 'init' { 'new' } else { topic }
	if topic == 'topics' {
		println(known_topics(topicdir))
		exit(0)
	}
	target_topic := os.join_path(topicdir, '${name}.txt')
	content := os.read_file(target_topic) or {
		eprintln(help.unknown_topic)
		eprintln(known_topics(topicdir))
		exit(1)
	}
	println(content)
	exit(0)
}

fn known_topics(topicdir string) string {
	mut res := []string{}
	res << 'Known help topics: '
	topic_files := os.glob(os.join_path(topicdir, '*.txt')) or { [] }
	mut topics := topic_files.map(os.file_name(it).replace('.txt', ''))
	topics.sort()
	res << topics.join(', ')
	res << '.'
	return res.join('')
}
