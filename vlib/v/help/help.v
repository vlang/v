module help

// TODO: move this file outside internal, and merge it with cmd/tools/modules/vhelp/vhelp.v .
import os
import v.pref

const (
	categories = {
		'build':        [
			'build-c',
			'build-js',
			'build-native',
		]
		'common':       [
			'doc',
			'fmt',
			'missdoc',
			'repl',
			'run',
			'test',
			'vet',
			'watch',
			'where',
		]
		'installation': [
			'self',
			'symlink',
			'up',
			'version',
		]
		'other':        [
			'ast',
			'bin2v',
			'bug',
			'bump',
			'check-md',
			'complete',
			'doctor',
			'gret',
			'ls',
			'other',
			'shader',
			'tracev',
		]
		'scaffolding':  [
			'init',
			'new',
		]
		'vpm':          [
			'install',
			'list',
			'outdated',
			'remove',
			'search',
			'show',
			'update',
			'upgrade',
			'vpm',
		]
	}
	unknown_topic = '`v help`: unknown help topic provided. Use `v help` for usage information.'
)

/// Printing the topic menu and exiting
pub fn print_and_exit(topic string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	topicdir := os.join_path(vroot, 'vlib', 'v', 'help')

	for b in topic {
		if (b >= `a` && b <= `z`) || b == `-` || (b >= `0` && b <= `9`) {
			continue
		}
		eprintln(help.unknown_topic)
		exit(1)
	}
	mut search_category := false
	mut path_to := topic

	// Making sure the topic chosen isn't `default`
	if path_to != 'default' {
		for category, item in help.categories {
			if topic in item {
				path_to = '${category}/${topic}'
				break
			} else if topic == category {
				path_to = '${category}/${category}.txt'
				search_category = true
				break
			}
		}
	}

	topic_dir := if search_category {
		os.join_path(topicdir, '${path_to}')
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

/// Getting topics known to V
fn known_topics(topicdir string) string {
	mut res := []string{}
	res << 'Known help topics: '

	mut topics := os.walk_ext(topicdir, '.txt').map(os.file_name(it).replace('.txt', ''))

	topics.sort()
	res << topics.join(', ')
	res << '.'
	return res.join('').replace('default, ', '')
}
