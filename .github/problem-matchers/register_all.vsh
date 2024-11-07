dump(getenv('GITHUB_JOB'))
chdir(@VEXEROOT)!
for f in walk_ext('.github/problem-matchers/', '.json').sorted() {
	println('::add-matcher::${real_path(f)}')
}
