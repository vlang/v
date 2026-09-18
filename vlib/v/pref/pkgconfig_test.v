module pref

fn test_pkgconfig_flag_queries_keep_explicit_output_filters() {
	for option in ['--cflags', '--cflags-only-I', '--cflags-only-other', '--libs', '--libs-only-L',
		'--libs-only-l', '--libs-only-other'] {
		assert pkgconfig_flags_args('${option} library')! == [option, 'library']
		assert pkgconfig_flags_args('library ${option}')! == ['library', option]
		assert pkgconfig_flags_args('--static ${option} library')! == ['--static', option, 'library']
	}
	assert pkgconfig_flags_args('--libs-only-l --libs-only-L one two')! == [
		'--libs-only-l',
		'--libs-only-L',
		'one',
		'two',
	]
	assert pkgconfig_flags_args('--cflags --libs one two')! == ['--cflags', '--libs', 'one', 'two']
}

fn test_pkgconfig_flag_queries_retain_default_actions_and_modifiers() {
	assert pkgconfig_flags_args('library')! == ['--cflags', '--libs', 'library']
	assert pkgconfig_flags_args('one two')! == ['--cflags', '--libs', 'one', 'two']
	assert pkgconfig_flags_args('--static library')! == ['--cflags', '--libs', '--static', 'library']
	assert pkgconfig_flags_args('--define-prefix library')! == ['--cflags', '--libs', '--define-prefix',
		'library']
	assert pkgconfig_flags_args('--dont-define-prefix --static library')! == [
		'--cflags',
		'--libs',
		'--dont-define-prefix',
		'--static',
		'library',
	]
	assert pkgconfig_flags_args('-- --libs')! == ['--cflags', '--libs', '--', '--libs']
	assert pkgconfig_flags_args('--libs -- --cflags')! == ['--libs', '--', '--cflags']
}

fn test_pkgconfig_flag_queries_keep_literal_argument_boundaries() {
	assert pkgconfig_flags_args('--cflags "package name"')! == ['--cflags', 'package name']
	assert pkgconfig_flags_args('--libs --define-variable=prefix="café dir" library')! == [
		'--libs',
		'--define-variable=prefix=café dir',
		'library',
	]
	assert pkgconfig_flags_args('--define-variable=filter=--libs library')! == [
		'--cflags',
		'--libs',
		'--define-variable=filter=--libs',
		'library',
	]
	assert pkgconfig_flags_args('"--libs-only-later" library')! == ['--cflags', '--libs',
		'--libs-only-later', 'library']
	assert pkgconfig_flags_args('--cflags library ">=" 1.2')! == ['--cflags', 'library', '>=',
		'1.2']
	assert pkgconfig_flags_args('--libs ""')! == ['--libs', '']
	assert pkgconfig_flags_args(r'--libs ";" "$(not-a-command)"')! == ['--libs', ';', r'$(not-a-command)']
}

fn test_pkgconfig_flag_queries_reject_malformed_quotes_and_keep_empty_input_empty() {
	assert pkgconfig_flags_args('')! == []string{}
	assert pkgconfig_flags_args(' \t\n')! == []string{}
	for raw in ['--libs "unfinished', "--cflags 'unfinished"] {
		if _ := pkgconfig_flags_args(raw) {
			assert false, 'accepted unterminated quote: ${raw}'
		}
	}
}
