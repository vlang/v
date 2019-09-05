module orm

struct Row {
pub mut:
	vals []string
}

pub fn rows_first_or_empty(rows []orm.Row) orm.Row? {
	if rows.len == 0 { return error('no rows') } 
	return rows[0]
}
