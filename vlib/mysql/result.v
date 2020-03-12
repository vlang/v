module mysql

pub struct Result {
	result &MYSQL_RES
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

pub fn (r Result) fetch_row() &byteptr {
	return mysql_fetch_row(r.result)
}

pub fn (r Result) num_fields() int {
	return mysql_num_fields(r.result)
}

pub fn (r Result) rows() []Row {
	mut rows := []Row
	nr_cols := r.num_fields()
	for rr := r.fetch_row(); rr; rr = r.fetch_row() {
		mut row := Row{}
		for i in 0..nr_cols {
			if rr[i] == 0 {
				row.vals << ''
			} else {
				row.vals << string(rr[i])
			}
		}
		rows << row
	}
	return rows
}

pub fn (r Result) fetch_fields() []Field {
	mut fields := []Field
	nr_cols := r.num_fields()
	orig_fields := mysql_fetch_fields(r.result)
	for i in 0..nr_cols {
		fields << Field{
			name: string(orig_fields[i].name)
			org_name: string(orig_fields[i].org_name)
			table: string(orig_fields[i].table)
			org_table: string(orig_fields[i].org_table)
			db: string(orig_fields[i].db)
			catalog: string(orig_fields[i].catalog)
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
	return fields
}

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
	type: ${f.type_.str()}
}
'
}

pub fn (r Result) free() {
	mysql_free_result(r.result)
}
