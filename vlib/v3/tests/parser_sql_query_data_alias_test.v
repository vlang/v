import os
import v3.flat
import v3.parser
import v3.pref

fn parse_sql_query_data_alias_source(name string, source string) &flat.FlatAst {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(src)
	return p.a
}

fn sql_expr_types(a &flat.FlatAst) []string {
	mut types := []string{}
	for node in a.nodes {
		if node.kind == .sql_expr {
			types << node.typ
		}
	}
	return types
}

fn test_non_orm_expr_suffix_braced_literal_is_not_query_data() {
	a := parse_sql_query_data_alias_source('non_orm_expr_suffix_braced_literal',
		"fn main() {\n\tfilter_expr := {'enabled': true}\n\t_ = filter_expr\n}\n")
	mut sql_expr_count := 0
	mut map_init_count := 0
	for node in a.nodes {
		if node.kind == .sql_expr {
			sql_expr_count++
		}
		if node.kind == .map_init {
			map_init_count++
		}
	}
	assert sql_expr_count == 0
	assert map_init_count == 1
}

fn test_where_expr_map_literal_is_not_query_data() {
	a := parse_sql_query_data_alias_source('where_expr_map_literal',
		"fn main() {\n\twhere_expr := {'enabled': true}\n\t_ = where_expr\n}\n")
	mut sql_expr_count := 0
	mut map_init_count := 0
	for node in a.nodes {
		if node.kind == .sql_expr {
			sql_expr_count++
		}
		if node.kind == .map_init {
			map_init_count++
		}
	}
	assert sql_expr_count == 0
	assert map_init_count == 1
}

fn test_dynamic_orm_query_data_aliases_parse_braced_literals() {
	a := parse_sql_query_data_alias_source('dynamic_orm_query_data_aliases',
		'struct User {\n\tid int\n\tname string\n}\n\nfn main() {\n\tdb := 0\n\twhere_expr := {\n\t\tid == 1\n\t}\n\tset_expr := {\n\t\tname == "v"\n\t}\n\tupdate_expr := {\n\t\tname == "vv"\n\t}\n\t_ = sql db {\n\t\tdynamic select from User where where_expr\n\t\tdynamic update User set set_expr where update_expr\n\t}\n}\n')
	mut sql_expr_count := 0
	for node in a.nodes {
		if node.kind == .sql_expr && node.typ == 'orm.QueryData' {
			sql_expr_count++
		}
	}
	assert sql_expr_count == 3
}

fn test_dynamic_orm_query_data_aliases_accept_arbitrary_names() {
	a := parse_sql_query_data_alias_source('dynamic_orm_query_data_arbitrary_aliases',
		'struct User {\n\tid int\n\tname string\n}\n\nfn main() {\n\tdb := 0\n\twhere_filter := {\n\t\tid == 1\n\t}\n\tupdate_filter := {\n\t\tname == "Ada"\n\t}\n\t_ = sql db {\n\t\tdynamic select from User where where_filter\n\t\tdynamic update User set update_filter where where_filter\n\t}\n}\n')
	mut sql_expr_count := 0
	for node in a.nodes {
		if node.kind == .sql_expr && node.typ == 'orm.QueryData' {
			sql_expr_count++
		}
	}
	assert sql_expr_count == 2
}

fn test_dynamic_orm_query_data_alias_map_literal_is_not_ambiguous() {
	a := parse_sql_query_data_alias_source('dynamic_orm_query_data_map_literal',
		"struct User {\n\tid int\n}\n\nfn main() {\n\tdb := 0\n\twhere_filter := {'enabled': true}\n\t_ = sql db {\n\t\tdynamic select from User where where_filter\n\t}\n}\n")
	mut sql_expr_count := 0
	mut map_init_count := 0
	for node in a.nodes {
		if node.kind == .sql_expr && node.typ == 'orm.QueryData' {
			sql_expr_count++
		}
		if node.kind == .map_init {
			map_init_count++
		}
	}
	assert sql_expr_count == 0
	assert map_init_count == 1
}

fn test_sql_expr_type_uses_final_statement() {
	a := parse_sql_query_data_alias_source('sql_expr_type_uses_final_statement', '
struct User {
	id int
}

fn main() {
	db := 0
	user := User{}
	rows := sql db {
		insert user into User
		select from User
	}
	_ = rows
}
')
	assert sql_expr_types(a) == ['![]User']
}

fn test_sql_expr_type_includes_upsert_create_and_drop() {
	a := parse_sql_query_data_alias_source('sql_expr_type_includes_upsert_create_and_drop', '
struct User {
	id int
}

fn main() {
	db := 0
	user := User{}
	upserted := sql db {
		upsert user into User
	}
	created := sql db {
		create table User
	}
	dropped := sql db {
		insert user into User
		drop table User
	}
	_ = upserted
	_ = created
	_ = dropped
}
')
	assert sql_expr_types(a) == ['!int', '!int', '!int']
}

fn test_sql_expr_type_preserves_generic_table_arguments() {
	a := parse_sql_query_data_alias_source('sql_expr_type_preserves_generic_table_arguments', '
fn main() {
	db := 0
	rows := sql db {
		select from Row[int]
	}
	imported := sql db {
		select from models.Row[string]
	}
	_ = rows
	_ = imported
}
')
	assert sql_expr_types(a) == ['![]Row[int]', '![]models.Row[string]']
}
