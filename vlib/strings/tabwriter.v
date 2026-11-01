module strings

// TabWriter collects tabular rows and aligns their columns with spaces.
// Widths count runes, not terminal display columns. Rows are added with
// write_row; render returns the aligned text. Negative sizes are treated as zero.
pub struct TabWriter {
pub mut:
	min_width int // Minimum content width before padding.
	padding   int = 1 // Extra spacing after each column except the last.
mut:
	rows [][]string
}

// write_row appends a copy of the row of columns. Cell contents are preserved literally.
pub fn (mut w TabWriter) write_row(cols []string) {
	w.rows << cols.clone()
}

// reset clears all collected rows while keeping the configuration.
pub fn (mut w TabWriter) reset() {
	w.rows = [][]string{}
}

// render returns the rows formatted into aligned columns without clearing them.
// Missing cells are empty, and the final column is not padded.
// It panics if the requested column width overflows int.
pub fn (w TabWriter) render() string {
	if w.rows.len == 0 {
		return ''
	}
	padding := if w.padding > 0 { w.padding } else { 0 }
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
		if c < ncols - 1 && padding > max_int - widths[c] {
			panic('strings.TabWriter: column width overflow')
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
				pad := widths[c] - cell.runes().len + padding
				parts << cell + repeat(` `, pad)
			}
		}
		out << parts.join('')
	}
	return out.join('\n')
}
