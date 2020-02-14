import os.cmdline

fn test_options() {
	args := ['v', '-d', 'aa', '-d', 'bb', '-d', 'cc']
	ret := cmdline.options(args, '-d')
	assert ret.eq(['aa', 'bb', 'cc'])
}

fn test_option() {
	args := ['v', '-d', 'aa']
	ret := cmdline.option(args, '-d', '')
	assert ret == 'aa'
}

fn test_before_options() {
	args := ['-stat', 'test', 'aaa.v']
	ret := cmdline.before_options(args, ['test'])
	assert ret.eq(['-stat'])
}

fn test_after_options() {
	args := ['-stat', 'test', 'aaa.v']
	ret := cmdline.after_options(args, ['test'])
	assert ret.eq(['aaa.v'])
}

fn test_only_non_options() {
	args := ['-d', 'aa', '--help', 'bb']
	ret := cmdline.only_non_options(args)
	assert ret.eq(['aa', 'bb'])
}

fn test_only_options() {
	args := ['-d', 'aa', '--help', 'bb']
	ret := cmdline.only_options(args)
	assert ret.eq(['-d', '--help'])
}