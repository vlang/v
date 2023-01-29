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
	mut topics := []string{}
	mut ptopics := &topics
	topicdir := os.join_path(vroot, 'vlib', 'v', 'help')

	os.walk(topicdir, fn [mut ptopics] (topic string) {
		if os.file_ext(topic) == '.txt' {
			ptopics << os.file_name(topic).replace('.txt', '').replace('default,', '')
		}
	})

	for topic in topics {
		res := os.execute('${os.quoted_path(vexe)} help ${topic}')
		assert res.exit_code == 0
		assert res.output != ''
	}
}
