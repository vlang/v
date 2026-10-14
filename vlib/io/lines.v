module io

// LineIterator iterates over the lines of a BufferedReader, one `next` call
// per line (without the delimiter). Iteration ends with `none` at EOF; any
// read error likewise ends iteration.
pub struct LineIterator {
mut:
	r &BufferedReader
}

// lines returns an iterator over the lines of the buffered reader.
pub fn (mut r BufferedReader) lines() LineIterator {
	return LineIterator{
		r: r
	}
}

// next returns the next line, or `none` when the stream is exhausted.
pub fn (mut it LineIterator) next() ?string {
	return it.r.read_line() or { return none }
}
