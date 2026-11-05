import os

fn test_scalar_model_fields_do_not_instantiate_relationship_loading() {
	dir := os.join_path(os.vtmp_dir(), 'orm_scalar_mapping_codegen_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'models.v')
	out := os.join_path(dir, 'models.c')
	os.write_file(src, "import db.sqlite
import orm
import time

type ScalarName = string
type ScalarCount = int
type ScalarTime = time.Time

enum ScalarState {
	ready
}

struct ScalarModel {
	id int @[primary]
	name string
	count int
	ratio f64
	enabled bool
	state ScalarState
	alias_name ScalarName
	alias_count ScalarCount
	optional_count ?int
	optional_name ?string
	optional_alias ?ScalarCount
	optional_state ?ScalarState
	created time.Time
	deleted ?time.Time
	alias_time ScalarTime
	optional_alias_time ?ScalarTime
}

fn main() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	_ = sql db { select from ScalarModel }!
}
")!
	res := os.exec([@VEXE, '-nocache', '-o', out, src])
	assert res.exit_code == 0, res.output
	c := os.read_file(out)!
	assert c.contains('orm__QueryBuilder_ScalarModel__map_row(')
	// At most a shared declaration and definition remain; scalar mappers need no call sites.
	assert c.count('orm__include_child_paths(') <= 2
	assert c.count('orm__relation_should_load(') <= 2
	assert !c.contains('orm__query_relation_optional_array_like_')
}
