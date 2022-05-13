import v.builder
import v.parser
import v.pref

struct T01 {
	a int
	b byte
	c int
}

type T02 = string
type T03 = int | string
type T04 = []T03
type T05 = [47]T03

interface T06 {
	a int
}

interface T07 {
	T06
	b int
}

struct T08 {
	T01
	x string
}

fn test_type_size() ? {
	mut pref := pref.new_preferences()
	$if x64 {
		pref.m64 = true
	}
	mut b := builder.new_builder(pref)
	mut files := b.get_builtin_files()
	b.set_module_lookup_paths()
	parser.parse_files(files, b.table, b.pref)
	b.parse_imports()
	parser.parse_file(@FILE, b.table, .parse_comments, b.pref)

	mut t := b.table

	size01, _ := t.type_size(t.type_idxs['main.T01']?)
	assert sizeof(T01) == size01
	size02, _ := t.type_size(t.type_idxs['main.T02']?)
	assert sizeof(T02) == size02
	size03, _ := t.type_size(t.type_idxs['main.T03']?)
	assert sizeof(T03) == size03
	size04, _ := t.type_size(t.type_idxs['main.T04']?)
	assert sizeof(T04) == size04
	size05, _ := t.type_size(t.type_idxs['main.T05']?)
	assert sizeof(T05) == size05
	size06, _ := t.type_size(t.type_idxs['main.T06']?)
	assert sizeof(T06) == size06
	size07, _ := t.type_size(t.type_idxs['main.T07']?)
	assert sizeof(T07) == size07
	size08, _ := t.type_size(t.type_idxs['main.T08']?)
	assert sizeof(T08) == size08

	println('done')
}
