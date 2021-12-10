import os.cmdline

fn test_options() {
	args := ['v', '-d', 'aa', '-d', 'bb', '-d', 'cc']
	ret := cmdline.options(args, '-d')
	assert ret == ['aa', 'bb', 'cc']
}

fn test_option() {
	args := ['v', '-d', 'aa']
	ret := cmdline.option(args, '-d', '')
	assert ret == 'aa'
}

fn test_options_before() {
	args := ['-stat', 'test', 'aaa.v']
	ret := cmdline.options_before(args, ['test'])
	assert ret == ['-stat']
}

fn test_options_after() {
	args := ['-stat', 'test', 'aaa.v']
	ret := cmdline.options_after(args, ['test'])
	assert ret == ['aaa.v']
}

fn test_only_non_options() {
	args := ['-d', 'aa', '--help', 'bb']
	ret := cmdline.only_non_options(args)
	assert ret == ['aa', 'bb']
}

fn test_only_options() {
	args := ['-d', 'aa', '--help', 'bb']
	ret := cmdline.only_options(args)
	assert ret == ['-d', '--help']
}
