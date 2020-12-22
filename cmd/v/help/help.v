module help

// TODO: move this file outside internal, and merge it with cmd/tools/modules/vhelp/vhelp.v .
import os
import v.pref

const (
	unknown_topic = 'V Error: Unknown help topic provided. Use `v help` for usage information.'
)

pub fn print_and_exit(topic string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	for b in topic {
		if (b >= `a` && b <= `z`) || b == `-` || (b >= `0` && b <= `9`) {
			continue
		}
		eprintln(unknown_topic)
		exit(1)
	}
	target_topic := os.join_path(vroot, 'cmd', 'v', 'help', '${topic}.txt')
	content := os.read_file(target_topic) or {
		eprintln(unknown_topic)
		exit(1)
	}
	println(content)
	exit(0)
}
