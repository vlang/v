module mssql

pub struct Row {
pub mut:
	vals []string
}

pub struct Result {
pub mut:
	rows []Row
	// the number of rows affected by sql statement
	num_rows_affected int
}
