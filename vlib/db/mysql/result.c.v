module mysql

pub struct Result {
pub:
	result &C.MYSQL_RES = unsafe { nil }
}

pub struct Row {
pub mut:
	vals []string
}

// NullableRow contains streamed values while preserving SQL NULL as `none`.
pub struct NullableRow {
pub mut:
	vals []?string
}

// val returns the value at `index`, flattening SQL NULL to an empty string.
pub fn (row NullableRow) val(index int) string {
	if value := row.vals[index] {
		return value
	}
	return ''
}

// values returns all row values, flattening SQL NULL to empty strings.
pub fn (row NullableRow) values() []string {
	mut values := []string{cap: row.vals.len}
	for value in row.vals {
		values << if unwrapped := value { unwrapped } else { '' }
	}
	return values
}

// val_opt returns the raw optional value at `index`.
pub fn (row NullableRow) val_opt(index int) ?string {
	return row.vals[index]
}

// RowSet contains materialized rows and their column names.
pub struct RowSet {
pub:
	names []string
	rows  []Row
}

pub struct Field {
pub:
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
	type             FieldType
}

fn fields_from_result(result &C.MYSQL_RES) []Field {
	if result == unsafe { nil } {
		return []Field{}
	}
	mut fields := []Field{}
	nr_cols := C.mysql_num_fields(result)
	orig_fields := C.mysql_fetch_fields(result)
	for i in 0 .. nr_cols {
		unsafe {
			fields << Field{
				name:             mystring(orig_fields[i].name)
				org_name:         mystring(orig_fields[i].org_name)
				table:            mystring(orig_fields[i].table)
				org_table:        mystring(orig_fields[i].org_table)
				db:               mystring(orig_fields[i].db)
				catalog:          mystring(orig_fields[i].catalog)
				def:              resolve_nil_str(orig_fields[i].def)
				length:           orig_fields[i].length
				max_length:       orig_fields[i].max_length
				name_length:      orig_fields[i].name_length
				org_name_length:  orig_fields[i].org_name_length
				table_length:     orig_fields[i].table_length
				org_table_length: orig_fields[i].org_table_length
				db_length:        orig_fields[i].db_length
				catalog_length:   orig_fields[i].catalog_length
				def_length:       orig_fields[i].def_length
				flags:            orig_fields[i].flags
				decimals:         orig_fields[i].decimals
				charsetnr:        orig_fields[i].charsetnr
				type:             FieldType(orig_fields[i].type)
			}
		}
	}
	return fields
}

// fetch_row fetches the next row from a result.
pub fn (r Result) fetch_row() &charptr {
	mut thread_guard := mysql_thread_guard() or { return unsafe { nil } }
	defer {
		thread_guard.release()
	}
	return C.mysql_fetch_row(r.result)
}

// n_rows returns the number of rows from a result.
pub fn (r Result) n_rows() u64 {
	mut thread_guard := mysql_thread_guard() or { return 0 }
	defer {
		thread_guard.release()
	}
	return C.mysql_num_rows(r.result)
}

// n_fields returns the number of columns from a result.
pub fn (r Result) n_fields() int {
	mut thread_guard := mysql_thread_guard() or { return 0 }
	defer {
		thread_guard.release()
	}
	return C.mysql_num_fields(r.result)
}

// rows returns array of rows, each containing an array of values,
// one for each column.
pub fn (r Result) rows() []Row {
	mut thread_guard := mysql_thread_guard() or { return []Row{} }
	defer {
		thread_guard.release()
	}
	mut rows := []Row{}
	nr_cols := C.mysql_num_fields(r.result)
	for rr := C.mysql_fetch_row(r.result); rr; rr = C.mysql_fetch_row(r.result) {
		mut row := Row{}
		for i in 0 .. nr_cols {
			if unsafe { rr[i] == 0 } {
				row.vals << ''
			} else {
				row.vals << mystring(unsafe { &u8(rr[i]) })
			}
		}
		rows << row
	}
	return rows
}

// maps returns an array of maps, each containing a set of
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

// fields returns an array of fields/columns.
// The definitions apply primarily for columns of results,
// such as those produced by `SELECT` statements.
pub fn (r Result) fields() []Field {
	mut thread_guard := mysql_thread_guard() or { return []Field{} }
	defer {
		thread_guard.release()
	}
	return fields_from_result(r.result)
}

// field_names returns the column names for this result set.
pub fn (r Result) field_names() []string {
	if r.result == unsafe { nil } {
		return []string{}
	}
	fields := r.fields()
	mut names := []string{cap: fields.len}
	for field in fields {
		names << field.name
	}
	return names
}

// str serializes the field.
pub fn (f Field) str() string {
	return '
{
	name: "${f.name}"
	org_name: "${f.org_name}"
	table: "${f.table}"
	org_table: "${f.org_table}"
	db: "${f.db}"
	catalog: "${f.catalog}"
	def: "${f.def}"
	length: ${f.length}
	max_length: ${f.max_length}
	name_length: ${f.name_length}
	org_name_length: ${f.org_name_length}
	table_length: ${f.table_length}
	org_table_length: ${f.org_table_length}
	db_length: ${f.db_length}
	catalog_length: ${f.catalog_length}
	def_length: ${f.def_length}
	flags: ${f.flags}
	decimals: ${f.decimals}
	charsetnr: ${f.charsetnr}
	type: ${f.type.str()}
}
'
}

// free frees the memory used by a result.
@[unsafe]
pub fn (r &Result) free() {
	mut thread_guard := mysql_thread_guard() or { return }
	defer {
		thread_guard.release()
	}
	C.mysql_free_result(r.result)
}
