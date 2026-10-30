module strings

// TabWriter collects tabular rows and renders them aligned into columns,
// similar to Go's text/tabwriter. Rows are added with write_row; render
// returns the aligned text.
pub struct TabWriter {
pub mut:
	min_width int
	tab_width int = 8
	padding   int = 1
mut:
	rows [][]string
}

// write_row appends a row of columns.
pub fn (mut w TabWriter) write_row(cols []string) {
	w.rows << cols.clone()
}

// reset clears all collected rows.
pub fn (mut w TabWriter) reset() {
	w.rows = [][]string{}
}

// render returns the rows formatted into aligned columns.
pub fn (w TabWriter) render() string {
	if w.rows.len == 0 {
		return ''
	}
	// number of columns = max row length
	mut ncols := 0
	for row in w.rows {
		if row.len > ncols {
			ncols = row.len
		}
	}
	// column widths
	mut widths := []int{len: ncols}
	for c in 0 .. ncols {
		for row in w.rows {
			if c < row.len {
				l := row[c].runes().len
				if l > widths[c] {
					widths[c] = l
				}
			}
		}
		if widths[c] < w.min_width {
			widths[c] = w.min_width
		}
	}
	mut out := []string{}
	for row in w.rows {
		mut parts := []string{}
		for c in 0 .. ncols {
			cell := if c < row.len { row[c] } else { '' }
			if c == ncols - 1 {
				parts << cell
			} else {
				pad := widths[c] - cell.runes().len + w.padding
				mut p := cell
				for _ in 0 .. pad {
					p += ' '
				}
				parts << p
			}
		}
		out << parts.join('')
	}
	return out.join('\n')
}
