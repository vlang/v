module mssql

pub struct Row {
pub mut:
	vals []string
	// nulls is populated only for rows that contain a SQL NULL; is_null returns
	// false for indexes outside it.
	nulls []bool
}

// val returns the value at `index`.
pub fn (row Row) val(index int) string {
	return row.vals[index]
}

// values returns all row values.
pub fn (row Row) values() []string {
	return row.vals.clone()
}

// is_null reports whether the value at `index` came from SQL NULL.
pub fn (row Row) is_null(index int) bool {
	return index >= 0 && index < row.nulls.len && row.nulls[index]
}

pub struct Result {
pub mut:
	names []string
	rows  []Row
	// the number of rows affected by sql statement
	num_rows_affected int
}
