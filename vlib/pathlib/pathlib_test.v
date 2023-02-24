import os
import pathlib { cwd, home, path }

// this test uses `/` as path separators, but if run on windows it convert it
// to `\`.
fn conv_seps(path_str string) string {
	return path_str.replace('/', os.path_separator)
}

fn test_constructors() {
	assert path('///////').str() == conv_seps('/')
	assert path('').str() == '.'
	assert path('.').str() == '.'
	assert path('/a/b/c.txt').str() == conv_seps('/a/b/c.txt')

	// should skip single dot in path
	assert path("/a/./c.txt").str() == "/a/c.txt"
	assert path("/a/.").str() == "/a"

	// skip trailing separator
	assert path("./").str() == "."
	assert path("a/b/c/").str() == "a/b/c"

	// root testing
	assert path('/a/b/c.txt').root == conv_seps('/')
	assert path('//a/b/c.txt').root == conv_seps('//')
	assert path(r'\\a/b/c.txt').root == conv_seps('//')
	assert path('\\\\a/b/c.txt').root == conv_seps('//')
	assert path('///a/b').root == conv_seps('/')
	assert path(r'\\\a/b').root == conv_seps(r'/')

	assert path('c:\\test').drive == 'c:'
	assert path('c:\\test').root == conv_seps('/')

	assert path('D:test').drive == 'D:'
	assert path('D:test').root == ''
	assert path('D:test').name == 'test'

	assert cwd().str() == path('.').absolute().str()
	assert home().str() == path($env('HOME')).str()
}

fn test_path_fields() {
	test_path_1 := path('a/b/c.txt')
	assert test_path_1.root == ''
	assert test_path_1.name == 'c.txt'
	assert test_path_1.stem == 'c'
	assert test_path_1.suffix == '.txt'
	assert test_path_1.sep == os.path_separator

	test_path_2 := path('/name.suf1.suf2')
	assert test_path_2.root == '/'
	assert test_path_2.name == 'name.suf1.suf2'
	assert test_path_2.stem == 'name.suf1'
}

fn test_absolute() {
	assert path('/////').is_absolute()
	assert path('\\').is_absolute()
	assert path('\\\\').is_absolute()
	assert path('c:\\').is_absolute()
	assert path('D:/test').is_absolute()
	assert path('/etc').is_absolute()
	assert !path('.').is_absolute()
}

// fn test_glob() ! {
// 	assert path('.').glob('*')!.len == os.ls('.')!.len
// 	assert path('.').glob('*', '.*')!.len >= os.ls('.')!.len
// }

fn test_as_posix() {
	// posix paths
	assert path('/a/b/c').as_posix() == conv_seps('/a/b/c')
	assert path('.').as_posix() == '.'
	assert path('//').as_posix() == conv_seps('//')

	// and windows paths
	assert path('\\').as_posix() == conv_seps('/')
	assert path('\\\\').as_posix() == conv_seps('//')
	assert path('c:\\test').as_posix() == conv_seps('c:/test')
	assert path('c:test').as_posix() == 'c:test'
}

fn test_as_uri() {
	assert path('/a/b/c d.txt').as_uri() == 'file:///a/b/c%20d.txt'
	// assert path('c:\\test').as_uri() == 'file:///c:/test'
	assert path('test').as_uri() == 'file://' + os.getwd() + '/test'
}

fn test_is_relative_to() {
	assert path('/a/b/c').is_relative_to(path('/a/b'))
	assert path('/').is_relative_to(path('/'))
	assert path('/a/b/c').is_relative_to(path('/'))
	assert !path('/a/b/c').is_relative_to(path('/a/b/d'))
	assert !path('/a').is_relative_to(path('/a/b/d'))
}
