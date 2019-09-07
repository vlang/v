module orm

struct Row {
pub mut:
	vals []string
}

pub fn rows_first_or_empty(rows []orm.Row) orm.Row? {
	if rows.len == 0 { return error('no rows') } 
	return rows[0]
}

//////////////////////////////////////////////////////////////////////////////////////////
// The next functions are the public API that the ORM generator *needs* to be implemented
// in each DB driver that wants to be ORMable.
// NB: these will be called by the generated C code.
//////////////////////////////////////////////////////////////////////////////////////////

//Execute a parametrized query with N parameters. Return array of the result rows:
//pub fn (db &DB) pquery(cquery voidptr, nparams int, arr_params voidptr) []orm.Row 

//Execute a query that returns a single integer result, like for select count(*) 
//pub fn (db &DB) exec_to_row_to_int(query string) int

interface OrmDriver {
	pquery(cquery voidptr, nparams int, arr_params voidptr) []orm.Row 
	exec_to_row_to_int(query string) int
}
