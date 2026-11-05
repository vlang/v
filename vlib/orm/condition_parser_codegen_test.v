import os

fn test_condition_parser_is_shared_across_models() {
	dir := os.join_path(os.vtmp_dir(), 'orm_condition_codegen_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'models.v')
	out := os.join_path(dir, 'models.c')
	os.write_file(src, "import db.sqlite
import orm

struct First {
	id int
}

struct Second {
	id int
}

struct Third {
	id int
}

fn main() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut first := orm.new_query[First](db)
	mut second := orm.new_query[Second](db)
	mut third := orm.new_query[Third](db)
	first.where('id = ?', 1)!
	second.where('id = ?', 2)!
	third.where('id = ?', 3)!
	_ = first.query()!
	_ = second.query()!
	_ = third.query()!
}
") or { panic(err) }
	res := os.exec([@VEXE, '-nocache', '-o', out, src])
	assert res.exit_code == 0, res.output
	c := os.read_file(out) or { panic(err) }
	// One declaration, one definition, and one scanner call in the shared parser.
	// Each additional model previously generated another scanner call site.
	count := c.count('orm__MyTextScanner__next_tok(')
	assert count > 0 && count <= 3, count.str()
}
