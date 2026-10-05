module rows

// Row has fields named like the builtin array fields `data` and `cap`.
pub struct Row {
pub mut:
	cap  u8
	data [64]u8
}

pub struct Log {
pub mut:
	rows []Row
}

pub fn (l &Log) shares_rows_with(other &Log) bool {
	return l.rows.data == other.rows.data
}

pub fn (l &Log) row_capacity() int {
	return l.rows.cap
}
