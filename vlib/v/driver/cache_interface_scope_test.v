module driver

// The object of a cached module is found under the implementer lists that the
// code of the module can depend on. These tests give module_signature() the
// signature of a program and the modules that the code of one module can name.

const scope_signature = [
	'IError=Error,MessageError,io.Eof,MyErr',
	'Shape=Circle,Square',
	'io.Reader=io.BufferedReader,os.File,Feed',
	'io.Writer=os.File',
	'log.Logger=log.Log',
	'#module Circle=main',
	'#module Error=builtin',
	'#module Feed=main',
	'#module IError=builtin',
	'#module MessageError=builtin',
	'#module MyErr=main',
	'#module Shape=main',
	'#module Square=main',
].join('\n')

fn visible_modules(names ...string) map[string]bool {
	mut visible := map[string]bool{}
	visible['builtin'] = true
	for name in names {
		visible[name] = true
	}
	return visible
}

fn test_module_signature_holds_the_interfaces_of_the_visible_modules() {
	scopes := parse_v3_interface_scopes(scope_signature)
	assert scopes.names == ['IError', 'Shape', 'io.Reader', 'io.Writer', 'log.Logger']
	// `builtin` sees its own interface, and neither those of the modules that it
	// does not import nor the one of the program.
	assert scopes.module_signature(visible_modules()) == 'IError=Error,MessageError,io.Eof,MyErr'
	assert scopes.module_signature(visible_modules('io')) == [
		'IError=Error,MessageError,io.Eof,MyErr',
		'io.Reader=io.BufferedReader,os.File,Feed',
		'io.Writer=os.File',
	].join('\n')
	assert scopes.module_signature(visible_modules('io', 'os', 'log')) == [
		'IError=Error,MessageError,io.Eof,MyErr',
		'io.Reader=io.BufferedReader,os.File,Feed',
		'io.Writer=os.File',
		'log.Logger=log.Log',
	].join('\n')
}

fn test_module_signature_does_not_change_with_the_interfaces_of_the_program() {
	scopes := parse_v3_interface_scopes(scope_signature)
	other := parse_v3_interface_scopes(scope_signature.replace('Shape=Circle,Square', 'Shape=Circle,Square,Triangle') +
		'\nPlugin=Circle\n#module Plugin=main\n#module Triangle=main')
	for visible in [visible_modules(), visible_modules('io'), visible_modules('io', 'os', 'log')] {
		assert scopes.module_signature(visible) == other.module_signature(visible)
	}
}

fn test_module_signature_changes_with_an_implementer_of_a_visible_interface() {
	scopes := parse_v3_interface_scopes(scope_signature)
	other := parse_v3_interface_scopes(scope_signature.replace('io.Writer=os.File', 'io.Writer=os.File,Sink') +
		'\n#module Sink=main')
	assert scopes.module_signature(visible_modules()) == other.module_signature(visible_modules())
	assert scopes.module_signature(visible_modules('io')) != other.module_signature(visible_modules('io'))
}

// An implementer that the module cannot name brings the interfaces among its fields
// with it: the code that tells the implementers of `io.Reader` apart, to compare or
// to print them, handles the `Shape` inside a `Feed` as well.
fn test_module_signature_follows_the_fields_of_implementers_from_outside() {
	signature := scope_signature + '\n#reach Feed=Shape\n#reach io.BufferedReader=io.Reader\n#reach os.File=log.Logger'
	scopes := parse_v3_interface_scopes(signature)
	assert scopes.reach['Feed'] == ['Shape']
	// `os.File` is not visible from `io`, so its `log.Logger` counts too.
	assert scopes.module_signature(visible_modules('io')) == [
		'IError=Error,MessageError,io.Eof,MyErr',
		'Shape=Circle,Square',
		'io.Reader=io.BufferedReader,os.File,Feed',
		'io.Writer=os.File',
		'log.Logger=log.Log',
	].join('\n')
	// From a module that imports `os`, the fields of `os.File` have types that the
	// module sees anyway; `log` is not among its imports here.
	assert scopes.module_signature(visible_modules('io', 'os')) == [
		'IError=Error,MessageError,io.Eof,MyErr',
		'Shape=Circle,Square',
		'io.Reader=io.BufferedReader,os.File,Feed',
		'io.Writer=os.File',
	].join('\n')
	// `builtin` reaches none of this: no implementer of `IError` holds an interface.
	assert scopes.module_signature(visible_modules()) == 'IError=Error,MessageError,io.Eof,MyErr'
}

fn test_module_signature_adds_an_interface_that_implements_a_visible_one() {
	signature := [
		'IError=Error',
		'Stream=Feed',
		'io.Reader=os.File,Stream',
		'#module Error=builtin',
		'#module Feed=main',
		'#module IError=builtin',
		'#module Stream=main',
	].join('\n')
	scopes := parse_v3_interface_scopes(signature)
	assert scopes.module_signature(visible_modules('io')) == [
		'IError=Error',
		'Stream=Feed',
		'io.Reader=os.File,Stream',
	].join('\n')
}

fn test_module_signature_keeps_every_list_when_a_fact_is_missing() {
	// Nothing says which module declares `Mystery`: every module may see it.
	unknown_owner := parse_v3_interface_scopes('IError=Error\nMystery=Thing\n#module Error=builtin\n#module IError=builtin')
	assert unknown_owner.module_signature(visible_modules()) == 'IError=Error\nMystery=Thing'
	// An implementer reaches an interface that has no list.
	signature := 'IError=Error,MyErr\nShape=Circle\n#module Circle=main\n#module Error=builtin\n#module IError=builtin\n#module MyErr=main\n#module Shape=main\n#reach MyErr=Missing'
	unknown_reach := parse_v3_interface_scopes(signature)
	assert unknown_reach.module_signature(visible_modules()) == signature
}

fn test_interface_scopes_split_generic_names_at_the_right_commas() {
	assert v3_split_type_names('a,b[c, d],e[f[g,h]],i') == ['a', 'b[c, d]', 'e[f[g,h]]', 'i']
	assert v3_split_type_names('') == []string{}
	assert v3_interface_line_name('Box[map[string]int]=Crate') == 'Box[map[string]int]'
	assert v3_interface_line_name('#module Box=main') == '#module Box'
	scopes := parse_v3_interface_scopes('lists.Seq[K, V]=lists.Pair[K, V],Mine[int, string]\n#module Mine[int, string]=main\n#reach Mine[int, string]=lists.Seq[K, V]')
	assert scopes.names == ['lists.Seq[K, V]']
	assert scopes.modules['Mine[int, string]'] == 'main'
	assert scopes.declaring_module('lists.Seq[K, V]') == 'lists'
	assert scopes.module_signature(visible_modules('lists')) == 'lists.Seq[K, V]=lists.Pair[K, V],Mine[int, string]'
	assert scopes.module_signature(visible_modules()) == ''
}

// The object itself is named by the C generated for the module, whatever program
// that C was generated for.
fn test_cached_object_content_signature_depends_on_the_generated_c_only() {
	base := v3_cached_object_compile_signature(V3CachedObjectCompiler{}, 'c11', '', '', '', []string{},
		false, '')
	assert v3_cached_object_content_signature(base, 'aa') != v3_cached_object_content_signature(base,
		'ab')
	prod := v3_cached_object_compile_signature(V3CachedObjectCompiler{}, 'c11', '-O2', '', '',
		[]string{}, false, '')
	assert v3_cached_object_content_signature(base, 'aa') != v3_cached_object_content_signature(prod,
		'aa')
}
