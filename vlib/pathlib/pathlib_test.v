import os
import pathlib { cwd, home, path }

fn test_constructors() {
	// assert path('///////').str() == '/'
	assert path('').str() == '.'
	assert path('.').str() == '.'
	assert path('/a/b/c.txt').str() == '/a/b/c.txt'
	assert path('/a/b/c.txt').root == '/'

	// TODO: 'wrong' separator on system

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
	// assert path('/////').is_absolute()
	assert path('/').is_absolute()
	assert !path('.').is_absolute()
}

// fn test_glob() ! {
// 	assert path('.').glob('*')!.len == os.ls('.')!.len
// 	assert path('.').glob('*', '.*')!.len >= os.ls('.')!.len
// }

fn test_is_relative_to() {
	assert path('/a/b/c').is_relative_to(path('/a/b'))
	assert path('/').is_relative_to(path('/'))
	assert path('/a/b/c').is_relative_to(path('/'))
	assert !path('/a/b/c').is_relative_to(path('/a/b/d'))
	assert !path('/a').is_relative_to(path('/a/b/d'))
}

fn test_relative_to() {
	assert path('/a/b/c').is_relative_to(path('/a/b'))
	assert path('/').is_relative_to(path('/'))
	assert path('/a/b/c').is_relative_to(path('/'))
	assert !path('/a/b/c').is_relative_to(path('/a/b/d'))
	assert !path('/a').is_relative_to(path('/a/b/d'))
}
