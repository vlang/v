module mysql

pub struct Result {
	result &C.MYSQL_RES
}

pub struct Row {
pub mut:
	vals []string
}

pub struct Field {
	name             string
	org_name         string
	table            string
	org_table        string
	db               string
	catalog          string
	def              string
	length           int
	max_length       int
	name_length      u32
	org_name_length  u32
	table_length     u32
	org_table_length u32
	db_length        u32
	catalog_length   u32
	def_length       u32
	flags            u32
	decimals         u32
	charsetnr        u32
	type_            FieldType
}

// fetch_row - fetches the next row from a result.
pub fn (r Result) fetch_row() &byteptr {
	return C.mysql_fetch_row(r.result)
}

// n_rows - returns the number of rows from a result.
pub fn (r Result) n_rows() u64 {
	return C.mysql_num_rows(r.result)
}

// n_fields - returns the number of columns from a result.
pub fn (r Result) n_fields() int {
	return C.mysql_num_fields(r.result)
}

// rows - returns array of rows, each containing an array of values,
// one for each column.
pub fn (r Result) rows() []Row {
	mut rows := []Row{}
	nr_cols := r.n_fields()
	for rr := r.fetch_row(); rr; rr = r.fetch_row() {
		mut row := Row{}
		for i in 0 .. nr_cols {
			if unsafe { rr[i] == 0 } {
				row.vals << ''
			} else {
				row.vals << mystring(unsafe { byteptr(rr[i]) })
			}
		}
		rows << row
	}
	return rows
}

// maps - returns an array of maps, each containing a set of 
// field name: field value pairs.
pub fn (r Result) maps() []map[string]string {
	mut array_map := []map[string]string{}
	rows := r.rows()
	fields := r.fields()
	for i in 0 .. rows.len {
		mut map_val := map[string]string{}
		for j in 0 .. fields.len {
			map_val[fields[j].name] = rows[i].vals[j]
		}
		array_map << map_val
	}
	return array_map
}

// fields - returns an array of fields/columns.
// The definitions apply primarily for columns of results,
// such as those produced by `SELECT` statements.
pub fn (r Result) fields() []Field {
	mut fields := []Field{}
	nr_cols := r.n_fields()
	orig_fields := C.mysql_fetch_fields(r.result)
	for i in 0 .. nr_cols {
		unsafe {
			fields << Field{
				name: mystring(orig_fields[i].name)
				org_name: mystring(orig_fields[i].org_name)
				table: mystring(orig_fields[i].table)
				org_table: mystring(orig_fields[i].org_table)
				db: mystring(orig_fields[i].db)
				catalog: mystring(orig_fields[i].catalog)
				def: resolve_nil_str(orig_fields[i].def)
				length: orig_fields.length
				max_length: orig_fields.max_length
				name_length: orig_fields.name_length
				org_name_length: orig_fields.org_name_length
				table_length: orig_fields.table_length
				org_table_length: orig_fields.org_table_length
				db_length: orig_fields.db_length
				catalog_length: orig_fields.catalog_length
				def_length: orig_fields.def_length
				flags: orig_fields.flags
				decimals: orig_fields.decimals
				charsetnr: orig_fields.charsetnr
				type_: FieldType(orig_fields.@type)
			}
		}
	}
	return fields
}

// str - serializes the field
pub fn (f Field) str() string {
	return '
{
	name: "$f.name"
	org_name: "$f.org_name"
	table: "$f.table"
	org_table: "$f.org_table"
	db: "$f.db"
	catalog: "$f.catalog"
	def: "$f.def"
	length: $f.length
	max_length: $f.max_length
	name_length: $f.name_length
	org_name_length: $f.org_name_length
	table_length: $f.table_length
	org_table_length: $f.org_table_length
	db_length: $f.db_length
	catalog_length: $f.catalog_length
	def_length: $f.def_length
	flags: $f.flags
	decimals: $f.decimals
	charsetnr: $f.charsetnr
	type: $f.type_.str()
}
'
}

// free - frees the memory used by a result
[unsafe]
pub fn (r &Result) free() {
	C.mysql_free_result(r.result)
}
