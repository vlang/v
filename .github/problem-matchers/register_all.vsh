// The task of this program, is to register all available .json files
// in this folder with Github Actions, so that it can make annotations
// for all found notices/warnings/errors produced by the V compiler
// while running the CI jobs.
// Those annotations provide a way to get a quick overview for failures,
// without having to scroll into the detailed logs produced by each job.

const github_job = getenv('GITHUB_JOB')

if github_job == '' {
	exit(0)
}

dump(github_job)
chdir(@VEXEROOT)!
for f in walk_ext('.github/problem-matchers/', '.json').sorted() {
	println('::add-matcher::${real_path(f)}')
	println('registered matcher: `${f}`')
}
