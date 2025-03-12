import v.build_constraint

const benv = build_constraint.new_environment(['linux', 'tinyc'], ['abc', 'def'])

fn test_eval_fact() {
	assert benv.is_fact('tinyc')
	assert benv.is_fact('linux')
	assert !benv.is_fact('macos')
	assert !benv.is_fact('windows')
}

fn test_eval_define() {
	assert benv.is_define('abc')
	assert benv.is_define('def')
	assert !benv.is_define('xyz')
}

fn test_eval_true() {
	assert benv.eval('true')!
}

fn test_eval_false() {
	assert !benv.eval('false')!
}

fn test_eval_comment() {
	assert benv.eval('true // some comment')!
	assert benv.eval(' true// another comment  ...')!
	assert !benv.eval('false // some comment')!
	assert !benv.eval(' false// another comment  ...')!
}

fn test_eval_platforms_and_compilers() {
	assert benv.eval('tinyc')!
	assert benv.eval(' tinyc')!
	assert benv.eval('tinyc ')!
	assert benv.eval(' tinyc ')!
	assert !benv.eval('gcc')!
	assert !benv.eval('clang')!
	assert !benv.eval('msvc')!
	assert benv.eval('linux')!
	assert benv.eval(' linux')!
	assert benv.eval('linux ')!
	assert benv.eval(' linux ')!
	assert !benv.eval('windows')!
	assert !benv.eval('macos')!
	assert !benv.eval('freebsd')!
}

fn test_eval_defines() {
	assert benv.eval('abc?')!
	assert benv.eval(' abc?')!
	assert benv.eval('abc? ')!
	assert benv.eval(' abc? ')!
	assert benv.eval('abc ?')!
	assert benv.eval(' abc ?')!
	assert benv.eval('abc ? ')!
	assert benv.eval(' abc ? ')!
	assert benv.eval('def?')!
}

fn test_eval_not() {
	assert benv.eval('!gcc')!
	assert benv.eval('!clang')!
	assert benv.eval('!msvc')!
	assert !benv.eval('!tinyc')!
	assert !benv.eval(' !tinyc')!
	assert !benv.eval('!tinyc ')!
	assert !benv.eval(' !tinyc ')!
	assert benv.eval('!xyz?')!
}

fn test_eval_and() {
	assert benv.eval('linux && tinyc')!
	assert !benv.eval('macos && tinyc')!
	assert !benv.eval('windows && tinyc')!
	assert !benv.eval('linux && gcc')!
	//
	assert benv.eval('linux && tinyc && abc?')!
	assert benv.eval('linux && tinyc && def?')!
	assert !benv.eval('linux && tinyc && xyz?')!
	//
	assert benv.eval('linux && !gcc')!
	assert benv.eval('linux && !clang')!
	assert benv.eval('!gcc && !windows')!
	assert !benv.eval('!windows && tcc')!
	assert !benv.eval('windows && gcc')!
	assert !benv.eval('gcc && !windows')!
}

fn test_eval_or() {
	assert benv.eval('windows||tinyc')!
	assert benv.eval('windows || macos || tinyc')!
	assert benv.eval('windows || macos || tinyc')!
	assert benv.eval('windows || macos || gcc || abc?')!
	assert benv.eval('!windows||gcc')!
}

fn test_complex() {
	assert benv.eval('  (windows || tinyc) &&  linux ')!
	assert !benv.eval(' (windows || gcc)   &&  linux ')!
	assert benv.eval('  (windows || tinyc) && !macos ')!
	assert !benv.eval(' (windows || tinyc) &&  macos ')!
}

fn test_precedence() {
	assert benv.eval(' tinyc && !windows ')! == benv.eval(' tinyc && (!windows)')!
	assert benv.eval(' tinyc && !windows ')! == benv.eval(' (!windows) && tinyc')!
	assert benv.eval(' !windows && tinyc')! == benv.eval(' (!windows) && tinyc')!
	assert benv.eval(' !windows || tinyc')! == benv.eval(' (!windows) || tinyc')!
	assert benv.eval(' !linux   && tinyc')! == benv.eval(' (!linux)   && tinyc')!
	assert benv.eval(' !linux   || tinyc')! == benv.eval(' (!linux)   || tinyc')!
	assert benv.eval(' !windows && gcc  ')! == benv.eval(' (!windows) && gcc  ')!
	assert benv.eval(' !windows || gcc  ')! == benv.eval(' (!windows) || gcc  ')!
	assert benv.eval(' !linux   && gcc  ')! == benv.eval(' (!linux)   && gcc  ')!
	assert benv.eval(' !linux   || gcc  ')! == benv.eval(' (!linux)   || gcc  ')!
}
