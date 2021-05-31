module sqlite

import orm

/*pub fn (mut db DB) insert(table string, data orm.OrmQueryData) ?int {
	stmt := db.init_stmt()
}*/

pub fn (mut db DB) update(table string, data orm.OrmQueryData, where orm.OrmQueryData) ?int {
	query := orm.orm_stmt_gen(table, "'", .update, true, '?', data, where)
	stmt := db.init_stmt(query)
	mut c := 0
	for i, typ in data.types {
		mut err := 0
		if typ in orm.nums {
			err = stmt.bind_int(c, int(data.data[i]))
		} else if typ in orm.num64 {
			err = stmt.bind_i64(c, i64(data.data[i]))			
		} else if typ in orm.float {
			err = stmt.bind_f64(c, f64(data.data[i]))
		} else if typ == orm.string {
			err = stmt.bind_text(c, f64(data.data[i]))
		}
		if err != 0 {
			return IError(&SQLError{
				msg: unsafe { cstring_to_vstring(&char(C.sqlite3_errstr(code))) }
				code: code
			})
		}
		c++
	}
	return 0
}

/*pub fn (mut db DB) delete(table string, data orm.OrmQueryData, where orm.OrmQueryData) ?int {

}*/