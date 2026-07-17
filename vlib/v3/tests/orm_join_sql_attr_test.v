import os

const orm_join_sql_attr_vexe = @VEXE
const orm_join_sql_attr_tests_dir = os.dir(@FILE)
const orm_join_sql_attr_v3_dir = os.dir(orm_join_sql_attr_tests_dir)
const orm_join_sql_attr_vlib_dir = os.dir(orm_join_sql_attr_v3_dir)
const orm_join_sql_attr_v3_src = os.join_path(orm_join_sql_attr_v3_dir, 'v3.v')

fn orm_join_sql_attr_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_orm_join_sql_attr_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${orm_join_sql_attr_vexe} -gc none -path "${orm_join_sql_attr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${orm_join_sql_attr_v3_src}')
	assert build.exit_code == 0, build.output
	assert os.exists(v3_bin), build.output
	return v3_bin
}

fn orm_join_sql_attr_run(v3_bin string, name string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src_path, src) or { panic(err) }
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}_program_${os.getpid()}')
	os.rm(bin_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn orm_join_sql_attr_compile(v3_bin string, name string, src string) os.Result {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src_path, src) or { panic(err) }
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}_program_${os.getpid()}')
	os.rm(bin_path) or {}
	return os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
}

fn orm_join_sql_attr_write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn orm_join_sql_attr_run_project(v3_bin string, name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, src in files {
		orm_join_sql_attr_write_project_file(root, rel, src)
	}
	bin_path := os.join_path(os.temp_dir(), 'v3_${name}_program_${os.getpid()}')
	os.rm(bin_path) or {}
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_v3_static_where_and_join_sql_attribute_regressions() {
	v3_bin := orm_join_sql_attr_build_v3()
	limit_out := orm_join_sql_attr_run(v3_bin, 'orm_limit_zero', "import db.sqlite

struct LimitUser {
	id int @[primary; sql: serial]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table LimitUser
	}!

	user := LimitUser{
		name: 'Alice'
	}
	sql db {
		insert user into LimitUser
	}!

	static_rows := sql db {
		select from LimitUser limit 0
	}!
	assert static_rows.len == 0

	zero := 0
	runtime_rows := sql db {
		select from LimitUser limit zero
	}!
	assert runtime_rows.len == 0

	negative := -1
	negative_rows := sql db {
		select from LimitUser limit negative
	} or {
		assert err.msg().contains('non-negative')
		return
	}
	panic('expected negative runtime LIMIT to fail, got \${negative_rows.len} rows')
}
")
	assert limit_out == ''

	selector_db_out := orm_join_sql_attr_run(v3_bin, 'orm_selector_db_receiver', "import db.sqlite

struct SelectorDbApp {
mut:
	db sqlite.DB
}

struct SelectorDbUser {
	id int @[primary]
	name string
}

fn main() {
	mut app := SelectorDbApp{
		db: sqlite.connect(':memory:') or { panic(err) }
	}
	defer {
		app.db.close() or {}
	}

	sql app.db {
		create table SelectorDbUser
	}!

	user := SelectorDbUser{
		id: 1
		name: 'Alice'
	}
	sql app.db {
		insert user into SelectorDbUser
	}!

	rows := sql app.db {
		select from SelectorDbUser
	}!
	assert rows.len == 1
	assert rows[0].name == 'Alice'
}
")
	assert selector_db_out == ''

	mutated_insert_out := orm_join_sql_attr_run(v3_bin, 'orm_mutated_insert_fields', 'import db.sqlite

struct MutatedInsertUser {
	id int @[primary; sql: serial]
	name string @[default: \'"db_default"\']
}

fn main() {
	mut db := sqlite.connect(\':memory:\') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table MutatedInsertUser
	}!

	mut user := MutatedInsertUser{}
	user.name = \'Ada\'
	sql db {
		insert user into MutatedInsertUser
	}!

	rows := sql db {
		select from MutatedInsertUser
	}!
	assert rows.len == 1
	assert rows[0].name == \'Ada\'
}
')
	assert mutated_insert_out == ''

	selector_insert_out := orm_join_sql_attr_run(v3_bin, 'orm_selector_insert_value', 'import db.sqlite

struct SelectorInsertUser {
	id int @[primary]
	name string @[default: \'"db_default"\']
}

struct SelectorInsertRequest {
	user SelectorInsertUser
}

fn main() {
	mut db := sqlite.connect(\':memory:\') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table SelectorInsertUser
	}!

	request := SelectorInsertRequest{
		user: SelectorInsertUser{
			id: 1
			name: \'Ada\'
		}
	}
	sql db {
		insert request.user into SelectorInsertUser
	}!

	first := sql db {
		select from SelectorInsertUser where id == 1
	}!
	assert first.len == 1
	assert first[0].name == \'Ada\'

	update_request := SelectorInsertRequest{
		user: SelectorInsertUser{
			id: 1
			name: \'Grace\'
		}
	}
	sql db {
		upsert update_request.user into SelectorInsertUser
	}!

	updated := sql db {
		select from SelectorInsertUser where id == 1
	}!
	assert updated.len == 1
	assert updated[0].name == \'Grace\'
}
')
	assert selector_insert_out == ''

	invalid_out := orm_join_sql_attr_run(v3_bin, 'orm_invalid_static_where', "import db.sqlite

struct InvalidWhereUser {
	id int @[primary; sql: serial]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table InvalidWhereUser
	}!

	user := InvalidWhereUser{
		name: 'Alice'
	}
	sql db {
		insert user into InvalidWhereUser
	}!

	rows := sql db {
		select from InvalidWhereUser where typo == 1
	} or {
		assert err.msg().contains('typo')
		return
	}
	panic('expected invalid static WHERE to fail, got \${rows.len} rows')
}
")
	assert invalid_out == ''

	signed_out := orm_join_sql_attr_run(v3_bin, 'orm_signed_static_where', "import db.sqlite

struct SignedScoreUser {
	id int @[primary; sql: serial]
	score int
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table SignedScoreUser
	}!

	negative := SignedScoreUser{
		score: -1
	}
	positive := SignedScoreUser{
		score: 1
	}

	sql db {
		insert negative into SignedScoreUser
		insert positive into SignedScoreUser
	}!

	rows := sql db {
		select from SignedScoreUser where score == -1
	}!

	assert rows.len == 1
	assert rows[0].score == -1
}
")
	assert signed_out == ''

	mixed_result_out := orm_join_sql_attr_run(v3_bin, 'orm_mixed_result_failure', "import db.sqlite

struct MixedResultUser {
	id int @[primary]
	email string @[unique]
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table MixedResultUser
	}!

	first := MixedResultUser{
		id: 1
		email: 'alice@example.com'
	}
	duplicate := MixedResultUser{
		id: 2
		email: 'alice@example.com'
	}

	sql db {
		insert first into MixedResultUser
	}!

	sql db {
		insert duplicate into MixedResultUser
		select from MixedResultUser
	} or {
		assert err.msg().len > 0
		return
	}
	panic('expected failed mixed-result SQL block to fail')
}
")
	assert mixed_result_out == ''

	final_type_out := orm_join_sql_attr_run(v3_bin, 'orm_sql_final_statement_type', "import db.sqlite

struct FinalTypeUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table FinalTypeUser
	}!

	user := FinalTypeUser{
		id: 1
		name: 'Alice'
	}

	rows := sql db {
		insert user into FinalTypeUser
		select from FinalTypeUser
	}!
	assert rows.len == 1
	assert rows[0].name == 'Alice'

	next := FinalTypeUser{
		id: 1
		name: 'Bob'
	}
	upsert_id := sql db {
		upsert next into FinalTypeUser
	}!
	assert upsert_id >= 0
}
")
	assert final_type_out == ''

	expr_out := orm_join_sql_attr_run(v3_bin, 'orm_static_where_expr_rhs', "import db.sqlite

struct ExprWhereUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table ExprWhereUser
	}!

	user := ExprWhereUser{
		id: 41
		name: 'Alice'
	}
	sql db {
		insert user into ExprWhereUser
	}!

	base := 40
	rows := sql db {
		select from ExprWhereUser where id == base + 1
	}!

	assert rows.len == 1
	assert rows[0].name == 'Alice'
}
")
	assert expr_out == ''

	indexed_expr_out := orm_join_sql_attr_run(v3_bin, 'orm_static_where_index_expr', "import db.sqlite

struct IndexedExprWhereUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table IndexedExprWhereUser
	}!

	user := IndexedExprWhereUser{
		id: 1
		name: 'Alice'
	}
	sql db {
		insert user into IndexedExprWhereUser
	}!

	ids := [1]
	rows := sql db {
		select from IndexedExprWhereUser where id == ids[0]
	}!

	assert rows.len == 1
	assert rows[0].name == 'Alice'
}
")
	assert indexed_expr_out == ''

	static_is_out := orm_join_sql_attr_run(v3_bin, 'orm_static_is_rejects_non_null', "import db.sqlite

struct StaticIsUser {
	id int @[primary]
	active bool
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table StaticIsUser
	}!

	user := StaticIsUser{
		id: 1
		active: false
	}
	sql db {
		insert user into StaticIsUser
	}!

	rows := sql db {
		select from StaticIsUser where active is false
	} or {
		assert err.msg().contains('unsupported static SQL')
		return
	}
	panic('expected static IS with non-null value to fail, got \${rows.len} rows')
}
")
	assert static_is_out == ''

	unfiltered_mutation_out := orm_join_sql_attr_run(v3_bin, 'orm_unfiltered_update_delete', "import db.sqlite

struct UnfilteredMutationUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table UnfilteredMutationUser
	}!

	user := UnfilteredMutationUser{
		id: 1
		name: 'Ada'
	}
	sql db {
		insert user into UnfilteredMutationUser
	}!

	mut update_failed := false
	_ := sql db {
		update UnfilteredMutationUser set name = 'Grace'
	} or {
		assert err.msg().contains('WHERE')
		update_failed = true
		0
	}
	assert update_failed

	mut delete_failed := false
	_ := sql db {
		delete from UnfilteredMutationUser
	} or {
		assert err.msg().contains('WHERE')
		delete_failed = true
		0
	}
	assert delete_failed

	rows := sql db {
		select from UnfilteredMutationUser
	}!
	assert rows.len == 1
	assert rows[0].name == 'Ada'
}
")
	assert unfiltered_mutation_out == ''

	mutation_tail_out := orm_join_sql_attr_run(v3_bin, 'orm_mutation_tail_rejected', "import db.sqlite

struct TailMutationUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table TailMutationUser
	}!

	first := TailMutationUser{
		id: 1
		name: 'Ada'
	}
	second := TailMutationUser{
		id: 2
		name: 'Bob'
	}
	sql db {
		insert first into TailMutationUser
		insert second into TailMutationUser
	}!

	mut update_failed := false
	_ := sql db {
		update TailMutationUser set name = 'Grace' where id > 0 limit 1
	} or {
		assert err.msg().contains('LIMIT')
		update_failed = true
		0
	}
	assert update_failed

	after_update := sql db {
		select from TailMutationUser order by id
	}!
	assert after_update.map(it.name) == ['Ada', 'Bob']

	mut delete_failed := false
	_ := sql db {
		delete from TailMutationUser where id > 0 limit 1
	} or {
		assert err.msg().contains('LIMIT')
		delete_failed = true
		0
	}
	assert delete_failed

	after_delete := sql db {
		select from TailMutationUser order by id
	}!
	assert after_delete.len == 2
}
")
	assert mutation_tail_out == ''

	static_or_fallback_out := orm_join_sql_attr_run(v3_bin, 'orm_static_where_or_fallback', "import db.sqlite

struct StaticOrFallbackUser {
	id int @[primary]
	name string
}

fn lookup_id() !int {
	return error('missing')
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table StaticOrFallbackUser
	}!

	user := StaticOrFallbackUser{
		id: 1
		name: 'Ada'
	}
	sql db {
		insert user into StaticOrFallbackUser
	}!

	rows := sql db {
		select from StaticOrFallbackUser where id == lookup_id() or { 1 }
	}!
	assert rows.len == 1
	assert rows[0].name == 'Ada'
}
")
	assert static_or_fallback_out == ''

	generic_after_select_compile := orm_join_sql_attr_compile(v3_bin, 'orm_generic_after_select', "import db.sqlite

struct FirstGenericSqlRow[T] {
	id int @[primary]
	value T
}

struct SecondGenericSqlRow[T] {
	id int @[primary]
	value T
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table FirstGenericSqlRow[int]
		create table SecondGenericSqlRow[int]
	}!

	first := FirstGenericSqlRow[int]{
		id: 1
		value: 7
	}
	sql db {
		insert first into FirstGenericSqlRow[int]
	}!

	_ := sql db {
		select from FirstGenericSqlRow[int]
		drop table SecondGenericSqlRow[int]
	}!
}
")
	assert generic_after_select_compile.exit_code == 0, generic_after_select_compile.output

	dynamic_is_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_is_rejects_non_null', "import db.sqlite

struct DynamicIsUser {
	id int @[primary]
	active bool
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicIsUser
	}!

	user := DynamicIsUser{
		id: 1
		active: false
	}
	sql db {
		insert user into DynamicIsUser
	}!

	rows := sql db {
		dynamic select from DynamicIsUser where {
			active is false
		}
	} or {
		assert err.msg().contains('unsupported dynamic SQL')
		return
	}
	panic('expected dynamic IS with non-null value to fail, got \${rows.len} rows')
}
")
	assert dynamic_is_out == ''

	dynamic_indexed_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_where_index_expr', "import db.sqlite

struct DynamicIndexedUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicIndexedUser
	}!

	first := DynamicIndexedUser{
		id: 1
		name: 'Alice'
	}
	second := DynamicIndexedUser{
		id: 2
		name: 'Bob'
	}
	sql db {
		insert first into DynamicIndexedUser
		insert second into DynamicIndexedUser
	}!

	ids := [2]
	rows := sql db {
		dynamic select from DynamicIndexedUser where {
			id == ids[0]
		}
	}!

	assert rows.len == 1
	assert rows[0].name == 'Bob'
}
")
	assert dynamic_indexed_out == ''

	dynamic_count_aggregate_out := orm_join_sql_attr_run(v3_bin,
		'orm_dynamic_count_aggregate_where', "import db.sqlite
import orm

struct DynamicAggregateUser {
	id int @[primary]
	score int
}

fn dynamic_total(mut db sqlite.DB, wanted int) ?int {
	return sql db {
		dynamic select sum(score) from DynamicAggregateUser where {
			id == wanted
		}
	}!
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicAggregateUser
	}!

	first := DynamicAggregateUser{
		id: 1
		score: 10
	}
	second := DynamicAggregateUser{
		id: 2
		score: 20
	}

	sql db {
		insert first into DynamicAggregateUser
		insert second into DynamicAggregateUser
	}!

	wanted := 2
	count := sql db {
		dynamic select count from DynamicAggregateUser where {
			id == wanted
		}
	}!
	assert count == 1

	total := sql db {
		dynamic select sum(score) from DynamicAggregateUser where {
			id == wanted
		}
	} or {
		panic(err)
	}
	value := total.as_int() or { panic('expected aggregate int') }
	assert value == 20

	returned := dynamic_total(mut db, wanted) or { panic('expected returned aggregate int') }
	assert returned == 20
}
")
	assert dynamic_count_aggregate_out == ''

	dynamic_alias_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_alias_table_types', "import db.sqlite

enum AliasUserState {
	inactive
	active
}

struct AliasDepartment {
	id int @[primary]
	name string
}

struct AliasStateUser {
	id int @[primary]
	state AliasUserState
}

struct AliasRelationUser {
	id int @[primary]
	department AliasDepartment
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table AliasDepartment
		create table AliasStateUser
		create table AliasRelationUser
	}!

	department := AliasDepartment{
		id: 2
		name: 'Engineering'
	}
	active_user := AliasStateUser{
		id: 1
		state: .active
	}
	inactive_user := AliasStateUser{
		id: 2
		state: .inactive
	}
	relation_user := AliasRelationUser{
		id: 3
		department: department
	}

	sql db {
		insert active_user into AliasStateUser
		insert inactive_user into AliasStateUser
		insert relation_user into AliasRelationUser
	}!

	where_filter := {
		state == .active
	}
	active_rows := sql db {
		dynamic select from AliasStateUser where where_filter
	}!
	assert active_rows.len == 1
	assert active_rows[0].id == 1

	relation_filter := {
		department == department
	}
	relation_rows := sql db {
		dynamic select from AliasRelationUser where relation_filter
	}!
	assert relation_rows.len == 1
	assert relation_rows[0].id == 3
}
")
	assert dynamic_alias_out == ''

	join_count_aggregate_out := orm_join_sql_attr_run(v3_bin, 'orm_join_count_aggregate', "import db.sqlite

struct JoinAggregateDepartment {
	id int @[primary]
	name string
}

struct JoinAggregateUser {
	id int @[primary]
	department_id int
	score int
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table JoinAggregateDepartment
		create table JoinAggregateUser
	}!

	department := JoinAggregateDepartment{
		id: 4
		name: 'Engineering'
	}
	first := JoinAggregateUser{
		id: 1
		department_id: 4
		score: 10
	}
	second := JoinAggregateUser{
		id: 2
		department_id: 4
		score: 30
	}

	sql db {
		insert department into JoinAggregateDepartment
		insert first into JoinAggregateUser
		insert second into JoinAggregateUser
	}!

	wanted := 2
	count := sql db {
		select count from JoinAggregateUser
		join JoinAggregateDepartment on JoinAggregateUser.department_id == JoinAggregateDepartment.id
		where id == wanted
	}!
	assert count == 1

	total := sql db {
		select sum(score) from JoinAggregateUser
		join JoinAggregateDepartment on JoinAggregateUser.department_id == JoinAggregateDepartment.id
		where id == wanted
	}!
	if value := total {
		assert value == 30
	} else {
		panic('expected joined aggregate int')
	}

	minimum := sql db {
		select min(id) from JoinAggregateUser
		join JoinAggregateDepartment on JoinAggregateUser.department_id == JoinAggregateDepartment.id
	}!
	if value := minimum {
		assert value == 1
	} else {
		panic('expected joined aggregate min id')
	}
}
")
	assert join_count_aggregate_out == ''

	chained_join_out := orm_join_sql_attr_run(v3_bin, 'orm_chained_join_predicate', "import db.sqlite

struct ChainCompany {
	id int @[primary]
	name string
}

struct ChainDepartment {
	id int @[primary]
	company_id int
	name string
}

struct ChainUser {
	id int @[primary]
	department_id int
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table ChainCompany
		create table ChainDepartment
		create table ChainUser
	}!

	company := ChainCompany{
		id: 3
		name: 'Acme'
	}
	department := ChainDepartment{
		id: 2
		company_id: 3
		name: 'Engineering'
	}
	user := ChainUser{
		id: 1
		department_id: 2
		name: 'Ada'
	}
	sql db {
		insert company into ChainCompany
		insert department into ChainDepartment
		insert user into ChainUser
	}!

	rows := sql db {
		select from ChainUser
		join ChainDepartment on ChainUser.department_id == ChainDepartment.id
		join ChainCompany on ChainDepartment.company_id == ChainCompany.id
	}!
	assert rows.len == 1
	assert rows[0].name == 'Ada'
}
")
	assert chained_join_out == ''

	optional_relation_where_out := orm_join_sql_attr_run(v3_bin, 'orm_optional_relation_where', 'import db.sqlite

struct OptionalRelationAuthor {
	id int @[primary]
	name string
}

struct OptionalRelationPost {
	id int @[primary]
	title string
	author ?OptionalRelationAuthor @[fkey: \'id\']
}

fn main() {
	mut db := sqlite.connect(\':memory:\') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table OptionalRelationAuthor
		create table OptionalRelationPost
	}!

	author := OptionalRelationAuthor{
		id: 7
		name: \'Ada\'
	}
	db.exec("insert into OptionalRelationAuthor (id, name) values (7, \'Ada\')")!
	db.exec("insert into OptionalRelationPost (id, title, author) values (1, \'Written\', 7)")!
	db.exec("insert into OptionalRelationPost (id, title, author) values (2, \'Draft\', NULL)")!

	static_count := sql db {
		select count from OptionalRelationPost where author == author
	}!
	assert static_count == 1

	static_none_count := sql db {
		select count from OptionalRelationPost where author == none
	}!
	assert static_none_count == 1

	filter := {
		author == author
	}
	dynamic_count := sql db {
		dynamic select count from OptionalRelationPost where filter
	}!
	assert dynamic_count == 1

	none_filter := {
		author == none
	}
	dynamic_none_count := sql db {
		dynamic select count from OptionalRelationPost where none_filter
	}!
	assert dynamic_none_count == 1
}
')
	assert optional_relation_where_out == ''

	dynamic_update_empty_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_update_empty_data', "import db.sqlite

struct DynamicEmptyUpdateUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicEmptyUpdateUser
	}!

	user := DynamicEmptyUpdateUser{
		id: 1
		name: 'Alice'
	}
	sql db {
		insert user into DynamicEmptyUpdateUser
	}!

	include := false
	wanted := 1
	empty_set := {
		if include {
			name == 'Bob'
		}
	}
	mut empty_set_failed := false
	sql db {
		dynamic update DynamicEmptyUpdateUser set empty_set where {
			id == wanted
		}
	} or {
		assert err.msg().contains('assignment')
		empty_set_failed = true
	}
	assert empty_set_failed

	update_expr := {
		name == 'Bob'
	}
	empty_where := {
		if include {
			id == wanted
		}
	}
	mut empty_where_failed := false
	sql db {
		dynamic update DynamicEmptyUpdateUser set update_expr where empty_where
	} or {
		assert err.msg().contains('WHERE')
		empty_where_failed = true
	}
	assert empty_where_failed
}
")
	assert dynamic_update_empty_out == ''

	invalid_dynamic_set_compile := orm_join_sql_attr_compile(v3_bin,
		'orm_dynamic_invalid_set_operator', "import db.sqlite

struct DynamicInvalidSetUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		dynamic update DynamicInvalidSetUser set {
			name > 'Bob'
		} where {
			id == 1
		}
	}!
}
")
	assert invalid_dynamic_set_compile.exit_code != 0
	assert invalid_dynamic_set_compile.output.contains('unsupported dynamic SQL SET operator `>`')

	dynamic_final_type_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_final_result_type', "import db.sqlite

struct DynamicFinalUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicFinalUser
	}!

	first := DynamicFinalUser{
		id: 1
		name: 'Alice'
	}
	second := DynamicFinalUser{
		id: 2
		name: 'Bob'
	}
	sql db {
		insert first into DynamicFinalUser
	}!

	wanted := 2
	rows := sql db {
		insert second into DynamicFinalUser
		dynamic select from DynamicFinalUser where {
			id == wanted
		}
	}!
	assert rows.len == 1
	assert rows[0].name == 'Bob'

	update_expr := {
		name == 'Bobby'
	}
	updated := sql db {
		select from DynamicFinalUser
		dynamic update DynamicFinalUser set update_expr where {
			id == wanted
		}
	}!
	assert updated == 0
	assert db.q_string('select name from DynamicFinalUser where id = 2')! == 'Bobby'
}
")
	assert dynamic_final_type_out == ''

	dynamic_guard_failure_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_guard_after_failure', "import db.sqlite

__global calls int

struct DynamicGuardUser {
	id int @[primary]
	name string @[unique]
}

fn next_id() int {
	calls++
	return 1
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicGuardUser
	}!

	first := DynamicGuardUser{
		id: 1
		name: 'duplicate'
	}
	duplicate := DynamicGuardUser{
		id: 2
		name: 'duplicate'
	}
	sql db {
		insert first into DynamicGuardUser
	}!

	calls = 0
	mut failed := false
	sql db {
		insert duplicate into DynamicGuardUser
		dynamic select count from DynamicGuardUser where {
			id == next_id()
		}
	} or {
		failed = true
	}
	assert failed
	assert calls == 0
}
")
	assert dynamic_guard_failure_out == ''

	offset_out := orm_join_sql_attr_run(v3_bin, 'orm_offset_without_limit', "import db.sqlite

struct OffsetUser {
	id int @[primary; sql: serial]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table OffsetUser
	}!

	user := OffsetUser{
		name: 'Alice'
	}
	sql db {
		insert user into OffsetUser
	}!

	rows := sql db {
		select from OffsetUser offset 1
	} or {
		assert err.msg().contains('OFFSET requires LIMIT')
		return
	}
	panic('expected offset without limit to fail, got \${rows.len} rows')
}
")
	assert offset_out == ''

	negative_offset_out := orm_join_sql_attr_run(v3_bin, 'orm_negative_runtime_offset', "import db.sqlite

struct NegativeOffsetUser {
	id int @[primary; sql: serial]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table NegativeOffsetUser
	}!

	user := NegativeOffsetUser{
		name: 'Alice'
	}
	sql db {
		insert user into NegativeOffsetUser
	}!

	offset := -1
	rows := sql db {
		select from NegativeOffsetUser limit 10 offset offset
	} or {
		assert err.msg().contains('offset')
		assert err.msg().contains('non-negative')
		return
	}
	panic('expected negative runtime OFFSET to fail, got \${rows.len} rows')
}
")
	assert negative_offset_out == ''

	static_negative_limit_out := orm_join_sql_attr_run(v3_bin, 'orm_static_negative_limit', "import db.sqlite

struct StaticNegativeLimitUser {
	id int @[primary; sql: serial]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table StaticNegativeLimitUser
	}!

	user := StaticNegativeLimitUser{
		name: 'Alice'
	}
	sql db {
		insert user into StaticNegativeLimitUser
	}!

	rows := sql db {
		select from StaticNegativeLimitUser limit -1
	} or {
		assert err.msg().contains('limit')
		assert err.msg().contains('non-negative')
		return
	}
	panic('expected static negative LIMIT to fail, got \${rows.len} rows')
}
")
	assert static_negative_limit_out == ''

	static_negative_offset_out := orm_join_sql_attr_run(v3_bin, 'orm_static_negative_offset', "import db.sqlite

struct StaticNegativeOffsetUser {
	id int @[primary; sql: serial]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table StaticNegativeOffsetUser
	}!

	user := StaticNegativeOffsetUser{
		name: 'Alice'
	}
	sql db {
		insert user into StaticNegativeOffsetUser
	}!

	rows := sql db {
		select from StaticNegativeOffsetUser limit 10 offset -1
	} or {
		assert err.msg().contains('offset')
		assert err.msg().contains('non-negative')
		return
	}
	panic('expected static negative OFFSET to fail, got \${rows.len} rows')
}
")
	assert static_negative_offset_out == ''

	infix_update_out := orm_join_sql_attr_run(v3_bin, 'orm_update_infix_sql_attr', "import db.sqlite

struct UpdateInfixSqlAttrUser {
	id int @[primary]
	score int @[sql: 'user_score']
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table UpdateInfixSqlAttrUser
	}!

	user := UpdateInfixSqlAttrUser{
		id: 1
		score: 4
	}
	sql db {
		insert user into UpdateInfixSqlAttrUser
	}!

	sql db {
		update UpdateInfixSqlAttrUser set score = score + 3 where id == 1
	}!
	rows := sql db {
		select from UpdateInfixSqlAttrUser where id == 1
	}!
	assert rows.len == 1
	assert rows[0].score == 7
}
")
	assert infix_update_out == ''

	aggregate_out := orm_join_sql_attr_run(v3_bin, 'orm_aggregate_sql_attr', "import db.sqlite

struct AggregateSqlAttrUser {
	id int @[primary]
	score int @[sql: 'points']
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table AggregateSqlAttrUser
	}!

	first := AggregateSqlAttrUser{
		id: 1
		score: 10
	}
	second := AggregateSqlAttrUser{
		id: 2
		score: 15
	}

	sql db {
		insert first into AggregateSqlAttrUser
		insert second into AggregateSqlAttrUser
	}!

	total := sql db {
		select sum(score) from AggregateSqlAttrUser
	}!
	if value := total {
		assert value == 25
	} else {
		assert false
	}

	third := AggregateSqlAttrUser{
		id: 3
		score: 5
	}
	multi_total := sql db {
		insert third into AggregateSqlAttrUser
		select sum(score) from AggregateSqlAttrUser
	}!
	if value := multi_total {
		assert value == 30
	} else {
		assert false
	}
}
")
	assert aggregate_out == ''

	malformed_dynamic_out := orm_join_sql_attr_run(v3_bin, 'orm_dynamic_malformed_where_update', "import db.sqlite

struct DynamicMalformedUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table DynamicMalformedUser
	}!

	user := DynamicMalformedUser{
		id: 1
		name: 'Alice'
	}
	sql db {
		insert user into DynamicMalformedUser
	}!

	update_expr := {
		name == 'Bob'
	}
	sql db {
		dynamic update DynamicMalformedUser set update_expr where {
			id + 1
		}
	} or {
		assert err.msg().contains('unsupported dynamic SQL predicate')
		return
	}
	panic('expected malformed dynamic WHERE to fail')
}
")
	assert malformed_dynamic_out == ''

	bad_batch_update := orm_join_sql_attr_compile(v3_bin, 'orm_batch_update_requires_equality', "import db.sqlite

struct BatchUpdateUser {
	id int @[primary]
	name string
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	updates := [
		BatchUpdateUser{
			id: 1
			name: 'Ada'
		},
	]
	sql db {
		update BatchUpdateUser set name = updates.name where id > updates.id
	}!
}
")
	assert bad_batch_update.exit_code != 0, bad_batch_update.output

	ambiguous_join_out := orm_join_sql_attr_run(v3_bin, 'orm_join_qualifies_default_fields', "import db.sqlite

struct JoinDepartment {
	id int @[primary]
	name string
}

struct JoinUser {
	id int @[primary]
	name string
	department_id int
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table JoinDepartment
		create table JoinUser
	}!

	department := JoinDepartment{
		id: 7
		name: 'Engineering'
	}
	sales := JoinDepartment{
		id: 8
		name: 'Sales'
	}
	user := JoinUser{
		id: 1
		name: 'Alice'
		department_id: 7
	}
	second_user := JoinUser{
		id: 2
		name: 'Bob'
		department_id: 8
	}

	sql db {
		insert department into JoinDepartment
		insert sales into JoinDepartment
		insert user into JoinUser
		insert second_user into JoinUser
	}!

	wanted := 2
	users := sql db {
		select from JoinUser
		join JoinDepartment on JoinUser.department_id == JoinDepartment.id
		where id == wanted
		order by id desc
	}!

	assert users.len == 1
	assert users[0].id == 2
	assert users[0].name == 'Bob'

	users_by_department := sql db {
		select from JoinUser
		join JoinDepartment on JoinUser.department_id == JoinDepartment.id
		order by JoinDepartment.name
	}!

	assert users_by_department.len == 2
	assert users_by_department[0].id == 1
	assert users_by_department[0].name == 'Alice'
	assert users_by_department[1].id == 2
	assert users_by_department[1].name == 'Bob'

	engineering_users := sql db {
		select from JoinUser
		join JoinDepartment on JoinUser.department_id == JoinDepartment.id
		where JoinDepartment.name == 'Engineering'
	}!

	assert engineering_users.len == 1
	assert engineering_users[0].id == 1
	assert engineering_users[0].name == 'Alice'

	dynamic_wanted := 1
	dynamic_users := sql db {
		dynamic select from JoinUser
		join JoinDepartment on JoinUser.department_id == JoinDepartment.id
		where {
			id == dynamic_wanted
		}
	}!

	assert dynamic_users.len == 1
	assert dynamic_users[0].id == 1
	assert dynamic_users[0].name == 'Alice'

	target_department := 'Sales'
	dynamic_department_users := sql db {
		dynamic select from JoinUser
		join JoinDepartment on JoinUser.department_id == JoinDepartment.id
		where {
			JoinDepartment.name == target_department
		}
	}!

	assert dynamic_department_users.len == 1
	assert dynamic_department_users[0].id == 2
	assert dynamic_department_users[0].name == 'Bob'
}
")
	assert ambiguous_join_out == ''

	qualified_attrs_out := orm_join_sql_attr_run_project(v3_bin, 'orm_qualified_table_attributes', {
		'foo/foo.v': "module foo

@[table: 'foo_users']
pub struct User {
pub mut:
	id int @[primary]
	bar_id int
	foo_name string
}
"
		'bar/bar.v': "module bar

@[table: 'bar_users']
pub struct User {
pub mut:
	id int @[primary]
	name string
}
"
		'main.v':    'module main

import db.sqlite
import foo
import bar

fn main() {
	mut db := sqlite.connect(\':memory:\') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table foo.User
		create table bar.User
	}!

	assert db.q_int("select count(*) from sqlite_master where type=\'table\' and name=\'foo_users\'")! == 1
	assert db.q_int("select count(*) from sqlite_master where type=\'table\' and name=\'bar_users\'")! == 1

	bar_user := bar.User{
		id: 7
		name: \'B\'
	}
	foo_user := foo.User{
		id: 1
		bar_id: 7
		foo_name: \'F\'
	}
	sql db {
		insert bar_user into bar.User
		insert foo_user into foo.User
	}!
	assert db.q_string(\'select name from bar_users\')! == \'B\'

	joined := sql db {
		select from foo.User
		join bar.User on bar.User.id == foo.User.bar_id
	}!
	assert joined.len == 1
}
'
	})
	assert qualified_attrs_out == ''

	import_alias_out := orm_join_sql_attr_run_project(v3_bin, 'orm_import_alias_table_resolution', {
		'models/models.v':           "module models

@[table: 'alias_users']
pub struct User {
pub mut:
	id            int @[primary]
	department_id int
	name          string
}
"
		'departments/departments.v': "module departments

@[table: 'alias_departments']
pub struct Department {
pub mut:
	id   int @[primary]
	name string
}
"
		'main.v':                    'module main

import db.sqlite
import departments as d
import models as m

fn main() {
	mut db := sqlite.connect(\':memory:\') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table d.Department
		create table m.User
	}!
	assert db.q_int("select count(*) from sqlite_master where type=\'table\' and name=\'alias_users\'")! == 1
	assert db.q_int("select count(*) from sqlite_master where type=\'table\' and name=\'alias_departments\'")! == 1

	department := d.Department{
		id: 9
		name: \'Core\'
	}
	user := m.User{
		id: 3
		department_id: 9
		name: \'Ada\'
	}
	sql db {
		insert department into d.Department
		insert user into m.User
	}!

	rows := sql db {
		select from m.User
	}!
	assert rows.len == 1
	assert db.q_string(\'select name from alias_users where id = 3\')! == \'Ada\'

	joined := sql db {
		select from m.User
		join d.Department on d.Department.id == m.User.department_id
	}!
	assert joined.len == 1
}
'
	})
	assert import_alias_out == ''

	join_out := orm_join_sql_attr_run(v3_bin, 'orm_join_sql_attr', "import db.sqlite

struct RenamedDepartment {
	dept_id int @[primary; sql: 'id']
	dept_name string
}

struct RenamedDepartmentUser {
	user_id int @[primary]
	user_name string
	department_id int
}

fn main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or {}
	}

	sql db {
		create table RenamedDepartment
		create table RenamedDepartmentUser
	}!

	department := RenamedDepartment{
		dept_id: 7
		dept_name: 'Engineering'
	}
	user := RenamedDepartmentUser{
		user_id: 1
		user_name: 'Alice'
		department_id: 7
	}

	sql db {
		insert department into RenamedDepartment
		insert user into RenamedDepartmentUser
	}!

	users := sql db {
		select from RenamedDepartmentUser
		join RenamedDepartment on RenamedDepartmentUser.department_id == RenamedDepartment.dept_id
	}!

	assert users.len == 1
	assert users[0].user_name == 'Alice'

	reversed_users := sql db {
		select from RenamedDepartmentUser
		join RenamedDepartment on RenamedDepartment.dept_id == RenamedDepartmentUser.department_id
	}!

	assert reversed_users.len == 1
	assert reversed_users[0].user_name == 'Alice'
}
")
	assert join_out == ''
}
