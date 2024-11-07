chdir(@VEXEROOT)!
for f in walk_ext('.github/problem-matchers/', '.json').sorted() {
	println('::add-matcher::${f}')
}
