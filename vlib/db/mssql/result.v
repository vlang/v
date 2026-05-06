module mssql

pub struct Row {
pub mut:
	vals []string
}

// val returns the value at `index`.
pub fn (row Row) val(index int) string {
	return row.vals[index]
}

// values returns all row values.
pub fn (row Row) values() []string {
	return row.vals.clone()
}

pub struct Result {
pub mut:
	rows []Row
	// the number of rows affected by sql statement
	num_rows_affected int
}
