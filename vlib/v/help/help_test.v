import os

const vexe = os.getenv('VEXE')

fn test_help() {
	res := os.execute('${os.quoted_path(vexe)} help')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_short_option() {
	res := os.execute('${os.quoted_path(vexe)} -h')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_help_as_long_option() {
	res := os.execute('${os.quoted_path(vexe)} --help')
	assert res.exit_code == 0
	assert res.output.starts_with('V is a tool for managing V source code.')
}

fn test_all_help() {
	vroot := os.dir(vexe)
	topicdir := os.join_path(vroot, 'vlib', 'v', 'help')
	mut topics := os.walk_ext(topicdir, '.txt')

	mut items := []string{}
	mut delim := ''

	for mut item in topics {
		$if windows {
			delim = '\\'
		} $else {
			delim = '/'
		}
		mut item_rev := item.replace('.txt', '').split(delim).reverse()
		item_rev.trim(2)
		items << item_rev.reverse()
	}

	for topic in items {
		res := os.execute('${os.quoted_path(vexe)} help ${topic}')

		if topic == 'help' {
			continue
		}

		assert res.exit_code == 0
		assert res.output != ''
	}
}
