// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import strings

fn sql_params2params_gen(sql_params []string, sql_types []string, qprefix string) string {
	mut params_gen := ''
	for i, mparam in sql_params {
		param := mparam.trim_space()
		paramtype := sql_types[ i ]
		if param[0].is_digit() {
			params_gen += '${qprefix}params[$i] = int_str($param).str;\n'
		}else if param[0] == `\'` {
			sparam := param.trim('\'')
			params_gen += '${qprefix}params[$i] = "$sparam";\n'
		} else {
			// A variable like q.nr_orders
			if paramtype == 'int' {
				params_gen += '${qprefix}params[$i] = int_str( $param ).str;\n'
			}else if paramtype == 'string' {
				params_gen += '${qprefix}params[$i] = ${param}.str;\n'
			}else{
				verror('orm: only int and string variable types are supported in queries')
			}
		}
	}
	//println('>>>>>>>> params_gen')
	//println( params_gen )
	return params_gen
}

// `db.select from User where id == 1 && nr_bookings > 0`
fn (p mut Parser) select_query(fn_ph int) string {
	// NB: qprefix and { p.sql_i, p.sql_params, p.sql_types } SHOULD be reset for each query,
	// because we can have many queries in the _same_ scope.
	qprefix := p.get_tmp().replace('tmp','sql') + '_'
	p.sql_i = 0
	p.sql_params = []string
	p.sql_types = []string

	mut q := 'select '
	p.check(.key_select)
	n := p.check_name()
	if n == 'count' {
		q += 'count(*) from '
		p.check_name()
	}
	table_name := p.check_name()
	// Register this type's fields as variables so they can be used in where expressions
	typ := p.table.find_type(table_name)
	if typ.name == '' {
		p.error('unknown type `$table_name`')
	}
	//fields := typ.fields.filter(typ == 'string' || typ == 'int')
	// get only string and int fields
	mut fields := []Var
	for i, field in typ.fields {
		if field.typ != 'string' && field.typ != 'int' {
			continue
		}
		fields << field
	}
	if fields.len == 0 {
		p.error('V orm: select: empty fields in `$table_name`')
	}
	if fields[0].name != 'id' {
		p.error('V orm: `id int` must be the first field in `$table_name`')
	}
	// 'select id, name, age from...'
	if n == 'from' {
		for i, field in fields {
			q += field.name
			if i < fields.len - 1 {
				q += ', '
			}
		}
		q += ' from '
	}
	for field in fields {
		//println('registering sql field var $field.name')
		if field.typ != 'string' && field.typ != 'int' {
			continue
		}
		p.register_var({ field | is_used:true })
	}
	q += table_name
	// `where` statement
	if p.tok == .name && p.lit == 'where' {
		p.next()
		p.cgen.start_tmp()
		p.is_sql = true
		p.bool_expression()
		p.is_sql = false
		q += ' where ' + p.cgen.end_tmp()
	}
	// limit?
	mut query_one := false
	if p.tok == .name && p.lit == 'limit' {
		p.next()
		p.cgen.start_tmp()
		p.is_sql = true
		p.bool_expression()
		p.is_sql = false
		limit := p.cgen.end_tmp()
		q += ' limit ' + limit
		// `limit 1` means we are getting `?User`, not `[]User`
		if limit.trim_space() == '1' {
			query_one = true
		}
	}
	println('sql query="$q"')
	p.cgen.insert_before('// DEBUG_SQL prefix: $qprefix | fn_ph: $fn_ph | query: "$q" ')
	
	if n == 'count' {
		p.cgen.set_placeholder(fn_ph, 'pg__DB_q_int(')
		p.gen(', tos2("$q"))')
	} else {
		// Build an object, assign each field.
		tmp := p.get_tmp()
		mut obj_gen := strings.new_builder(300)
		for i, field in fields {
			mut cast := ''
			if field.typ == 'int' {
				cast = 'v_string_int'
			}
			obj_gen.writeln('${qprefix}$tmp . $field.name = $cast( *(string*)array__get(${qprefix}row.vals, $i) );')
		}
		// One object
		if query_one {
			mut params_gen := sql_params2params_gen( p.sql_params, p.sql_types, qprefix )
			p.cgen.insert_before('

char* ${qprefix}params[$p.sql_i];
$params_gen

Option_${table_name} opt_${qprefix}$tmp;
void* ${qprefix}res = PQexecParams(db.conn, "$q", $p.sql_i, 0, ${qprefix}params, 0, 0, 0)  ;
array_pg__Row ${qprefix}rows = pg__res_to_rows ( ${qprefix}res ) ;
Option_pg__Row opt_${qprefix}row = pg__rows_first_or_empty( ${qprefix}rows );
if (! opt_${qprefix}row . ok ) {
   opt_${qprefix}$tmp = v_error( opt_${qprefix}row . error );
}else{
   $table_name ${qprefix}$tmp;
   pg__Row ${qprefix}row = *(pg__Row*) opt_${qprefix}row . data;
${obj_gen.str()}
   opt_${qprefix}$tmp = opt_ok( & ${qprefix}$tmp, sizeof($table_name) );
}

')
			p.cgen.resetln('opt_${qprefix}$tmp')
		}
		// Array
		else {
			q += ' order by id'
			params_gen := sql_params2params_gen( p.sql_params, p.sql_types, qprefix )
			p.cgen.insert_before('char* ${qprefix}params[$p.sql_i];
$params_gen

void* ${qprefix}res = PQexecParams(db.conn, "$q", $p.sql_i, 0, ${qprefix}params, 0, 0, 0)  ;
array_pg__Row ${qprefix}rows = pg__res_to_rows(${qprefix}res);

// TODO preallocate
array ${qprefix}arr_$tmp = new_array(0, 0, sizeof($table_name));
for (int i = 0; i < ${qprefix}rows.len; i++) {
    pg__Row ${qprefix}row = *(pg__Row*)array__get(${qprefix}rows, i);
    $table_name ${qprefix}$tmp;
    ${obj_gen.str()}
    _PUSH(&${qprefix}arr_$tmp, ${qprefix}$tmp, ${tmp}2, $table_name);
}
')
			p.cgen.resetln('${qprefix}arr_$tmp')
}
		
	}
	if n == 'count' {
		return 'int'
	}	else if query_one {		
		opt_type := 'Option_$table_name'		
		p.cgen.typedefs << 'typedef Option $opt_type;'
		p.table.register_type( opt_type )
		return opt_type
	}  else {
		p.register_array('array_$table_name')
		return 'array_$table_name'
	}
}

// `db.insert(user)`
fn (p mut Parser) insert_query(fn_ph int) {
	p.check_name()
	p.check(.lpar)
	var_name := p.check_name()
	p.check(.rpar)
	var := p.find_var(var_name)  or { return }
	typ := p.table.find_type(var.typ)
	mut fields := []Var
	for i, field in typ.fields {
		if field.typ != 'string' && field.typ != 'int' {
			continue
		}
		fields << field
	}
	if fields.len == 0 {
		p.error('V orm: insert: empty fields in `$var.typ`')
	}
	if fields[0].name != 'id' {
		p.error('V orm: `id int` must be the first field in `$var.typ`')
	}
	table_name := var.typ
	mut sfields := ''  // 'name, city, country'
	mut params := '' // params[0] = 'bob'; params[1] = 'Vienna';
	mut vals := ''  // $1, $2, $3...
	mut nr_vals := 0
	for i, field in fields {
		if field.name == 'id' {
			continue
		}
		sfields += field.name
		vals += '$' + i.str()
		nr_vals++
		params += 'params[${i-1}] = '
		if field.typ == 'string' {
			params += '$var_name . $field.name .str;\n'
		}  else if field.typ == 'int' {
			params += 'int_str($var_name . $field.name).str;\n'
		} else {
			p.error('V ORM: unsupported type `$field.typ`')
		}
		if i < fields.len - 1 {
			sfields += ', '
			vals += ', '
		}
	}
	p.cgen.insert_before('char* params[$nr_vals];' + params)
	p.cgen.set_placeholder(fn_ph, 'PQexecParams( ')
	p.genln('.conn, "insert into $table_name ($sfields) values ($vals)", $nr_vals,
0, params, 0, 0, 0)')
}

