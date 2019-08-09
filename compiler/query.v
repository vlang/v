module main 

import strings 

// `db.select from User where id == 1 && nr_bookings > 0` 
fn (p mut Parser) select_query(fn_ph int) string {
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
	// 'select id, name, age from...' 
	if n == 'from' {
		for i, field in typ.fields {
			q += field.name  
			if i < typ.fields.len - 1 {
				q += ', ' 
			} 
		} 
		q += ' from ' 
	} 
	for field in typ.fields {
		//println('registering sql field var $field.name') 
		p.cur_fn.register_var({ field | is_used:true}) 
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
		// `limit 1` means we are getting `User`, not `[]User` 
		if limit.trim_space() == '1' { 
			query_one = true 
		} 
	} 
	//println('sql query="$q"') 
	if n == 'count' {
		p.cgen.set_placeholder(fn_ph, 'pg__DB_q_int(')
		p.gen(', tos2("$q"))') 
	} else { 
		// Build an object, assign each field. 
		tmp := p.get_tmp() 
		mut obj_gen := strings.new_builder(100) 
		for i, field in typ.fields {
			mut cast := '' 
			if field.typ == 'int' {
				cast = 'string_int' 
			} 
			obj_gen.writeln('$tmp . $field.name = $cast( *(string*)array__get(row.vals, $i) );')    
		} 
		// One object 
		if query_one { 
			p.cgen.insert_before('

pg__Row row = pg__DB_exec_one(db, tos2("$q"));
$table_name $tmp; 
${obj_gen.str()} 

')
		p.cgen.resetln(tmp) 
} 
		// Array 
		else {
			p.cgen.insert_before('

array_pg__Row rows = pg__DB_exec(db, tos2("$q"));
printf("ROWS LEN=%d\\n", rows.len); 
// TODO preallocate 
array arr_$tmp = new_array(0, 0, sizeof($table_name));  
for (int i = 0; i < rows.len; i++) { 
	pg__Row row = *(pg__Row*)array__get(rows, i); 
	$table_name $tmp; 
	${obj_gen.str()} 
	_PUSH(&arr_$tmp, $tmp, ${tmp}2, $table_name);  
} 
')
		p.cgen.resetln('arr_$tmp') 
} 
		 
	} 
	if n == 'count' {
		return 'int' 
	}	else if query_one {
		return table_name 
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
	var := p.cur_fn.find_var(var_name) 
	typ := p.table.find_type(var.typ) 
	table_name := var.typ 
	mut fields := ''  // 'name, city, country' 
	mut params := '' // params[0] = 'bob'; params[1] = 'Vienna'; 
	mut vals := ''  // $1, $2, $3... 
	mut nr_vals := 0 
	for i, field in typ.fields {
		if field.name == 'id' {
			continue 
		} 
		fields += field.name 
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
		if i < typ.fields.len - 1 { 
			fields += ', ' 
			vals += ', ' 
		} 
	} 
	p.cgen.insert_before('char* params[$nr_vals];' + params) 
	p.cgen.set_placeholder(fn_ph, 'PQexecParams( ')
	p.genln('.conn, "insert into $table_name ($fields) values ($vals)", $nr_vals,
0, params, 0, 0, 0)') 
} 

