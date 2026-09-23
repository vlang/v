module rowreader

// Reader mirrors `encoding.csv.Reader`: a struct with the same short name as
// `streamreader.Reader`, whose `read` method has a different signature.
pub struct Reader {
mut:
	rows []string
	pos  int
}

// new_reader returns a `Reader` over `rows`.
pub fn new_reader(rows []string) &Reader {
	return &Reader{
		rows: rows
	}
}

// read returns the next row split by commas.
pub fn (mut r Reader) read() ![]string {
	if r.pos >= r.rows.len {
		return error('no more rows')
	}
	r.pos++
	return r.rows[r.pos - 1].split(',')
}

// read_line is a method that only this struct has.
pub fn (mut r Reader) read_line() !string {
	if r.pos >= r.rows.len {
		return error('no more rows')
	}
	r.pos++
	return r.rows[r.pos - 1]
}
