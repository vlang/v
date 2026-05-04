module main

import db.sqlite
import term

pub enum OutputMode {
	table
	csv
	line
	box
	markdown
	json
	html
	insert
	quote
}

// FormatOptions controls how query results are rendered.
pub struct FormatOptions {
pub:
	mode       OutputMode = .table
	headers    bool       = true
	nullvalue  string     = 'NULL'
	separator  string     = ','
	col_widths map[int]int // key = column index (0-based); value = fixed width; 0 = auto
	table_name string = 'tbl' // used by insert mode
}

// format renders rows using mode and headers with default options.
pub fn format(rows []sqlite.Row, mode OutputMode, headers bool) string {
	return format_opts(rows, FormatOptions{
		mode:    mode
		headers: headers
	})
}

// format_opts renders rows using the full FormatOptions.
pub fn format_opts(rows []sqlite.Row, opts FormatOptions) string {
	return match opts.mode {
		.table { format_table_ex(rows, opts) }
		.csv { format_csv_ex(rows, opts) }
		.line { format_line_ex(rows, opts) }
		.box { format_box_ex(rows, opts) }
		.markdown { format_markdown_ex(rows, opts) }
		.json { format_json_ex(rows, opts) }
		.html { format_html_ex(rows, opts) }
		.insert { format_insert_ex(rows, opts) }
		.quote { format_quote_ex(rows, opts) }
	}
}

// ---------- internal helpers ----------

fn eff_width(opts FormatOptions, i int, auto_w int) int {
	w := opts.col_widths[i]
	if w > 0 {
		return w
	}
	return auto_w
}

fn null_disp(s string, nullvalue string) string {
	return if s == '' { nullvalue } else { s }
}

fn resolve_names(names []string, ncols int) []string {
	if names.len == ncols {
		return names
	}
	mut generated := []string{cap: ncols}
	for i in 0 .. ncols {
		generated << 'col${i}'
	}
	return generated
}

fn str_pad(s string, w int) string {
	if s.len >= w {
		return s
	}
	return s + ' '.repeat(w - s.len)
}

fn str_pad_left(s string, w int) string {
	if s.len >= w {
		return s
	}
	return ' '.repeat(w - s.len) + s
}

fn compute_widths(rows []sqlite.Row, names []string, opts FormatOptions) []int {
	ncols := names.len
	mut auto_w := []int{len: ncols, init: names[index].len}
	for row in rows {
		for i, val in row.vals {
			if i < ncols {
				v := null_disp(val, opts.nullvalue)
				if v.len > auto_w[i] {
					auto_w[i] = v.len
				}
			}
		}
	}
	mut widths := []int{len: ncols}
	for i in 0 .. ncols {
		widths[i] = eff_width(opts, i, auto_w[i])
	}
	return widths
}

// ---------- table ----------

fn format_table_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)
	widths := compute_widths(rows, names, opts)

	mut sep := '+'
	for w in widths {
		sep += '-'.repeat(w + 2) + '+'
	}

	mut out := ''
	if opts.headers {
		out += sep + '\n'
		mut header := '|'
		for i, col in names {
			header += ' ' + term.bold(str_pad(col.limit(widths[i]), widths[i])) + ' |'
		}
		out += header + '\n'
	}
	out += sep + '\n'
	for row in rows {
		mut line := '|'
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			v := null_disp(val, opts.nullvalue)
			line += ' ' + str_pad(v.limit(widths[i]), widths[i]) + ' |'
		}
		out += line + '\n'
	}
	out += sep
	return out
}

// ---------- csv ----------

fn format_csv_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)
	sep := opts.separator

	mut lines := []string{}
	if opts.headers {
		lines << names.map(csv_escape_sep(it, sep)).join(sep)
	}
	for row in rows {
		lines << row.vals.map(csv_escape_sep(null_disp(it, opts.nullvalue), sep)).join(sep)
	}
	return lines.join('\n')
}

// ---------- line ----------

fn format_line_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)

	mut max_len := 0
	for col in names {
		if col.len > max_len {
			max_len = col.len
		}
	}

	mut blocks := []string{}
	for row in rows {
		mut lines := []string{}
		for j, val in row.vals {
			if j >= ncols {
				break
			}
			lines << '${str_pad_left(names[j], max_len)}: ${null_disp(val, opts.nullvalue)}'
		}
		blocks << lines.join('\n')
	}
	return blocks.join('\n\n')
}

// ---------- box (Unicode box-drawing) ----------

fn format_box_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)
	widths := compute_widths(rows, names, opts)

	mut top := '┌'
	mut mid := '├'
	mut bot := '└'
	for j, w in widths {
		top += '─'.repeat(w + 2)
		mid += '─'.repeat(w + 2)
		bot += '─'.repeat(w + 2)
		if j < ncols - 1 {
			top += '┬'
			mid += '┼'
			bot += '┴'
		}
	}
	top += '┐'
	mid += '┤'
	bot += '┘'

	mut out := top + '\n'
	if opts.headers {
		mut hdr := '│'
		for i, col in names {
			hdr += ' ' + term.bold(str_pad(col.limit(widths[i]), widths[i])) + ' │'
		}
		out += hdr + '\n' + mid + '\n'
	}
	for row in rows {
		mut line := '│'
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			v := null_disp(val, opts.nullvalue)
			line += ' ' + str_pad(v.limit(widths[i]), widths[i]) + ' │'
		}
		out += line + '\n'
	}
	out += bot
	return out
}

// ---------- markdown ----------

fn format_markdown_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)
	raw_w := compute_widths(rows, names, opts)
	mut widths := []int{len: ncols}
	for i, w in raw_w {
		widths[i] = if w < 3 { 3 } else { w }
	}

	mut lines := []string{}
	if opts.headers {
		mut hdr := '|'
		mut sep_row := '|'
		for i, col in names {
			hdr += ' ' + str_pad(col.limit(widths[i]), widths[i]) + ' |'
			sep_row += ' ' + '-'.repeat(widths[i]) + ' |'
		}
		lines << hdr
		lines << sep_row
	}
	for row in rows {
		mut line := '|'
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			v := null_disp(val, opts.nullvalue)
			line += ' ' + str_pad(v.limit(widths[i]), widths[i]) + ' |'
		}
		lines << line
	}
	return lines.join('\n')
}

// ---------- json ----------

fn format_json_ex(rows []sqlite.Row, _opts FormatOptions) string {
	if rows.len == 0 {
		return '[]'
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)

	mut items := []string{}
	for row in rows {
		mut fields := []string{}
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			key := '"${json_escape(names[i])}"'
			if val == '' {
				fields << '${key}:null'
			} else {
				fields << '${key}:"${json_escape(val)}"'
			}
		}
		items << '{${fields.join(',')}}'
	}
	return '[${items.join(',\n ')}]'
}

fn json_escape(s string) string {
	return s
		.replace('\\', '\\\\')
		.replace('"', '\\"')
		.replace('\n', '\\n')
		.replace('\r', '\\r')
		.replace('\t', '\\t')
}

// ---------- html ----------

fn format_html_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)

	mut out := '<table>\n'
	if opts.headers {
		out += '<tr>'
		for col in names {
			out += '<th>${html_escape(col)}</th>'
		}
		out += '</tr>\n'
	}
	for row in rows {
		out += '<tr>'
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			v := null_disp(val, opts.nullvalue)
			out += '<td>${html_escape(v)}</td>'
		}
		out += '</tr>\n'
	}
	out += '</table>'
	return out
}

fn html_escape(s string) string {
	return s
		.replace('&', '&amp;')
		.replace('<', '&lt;')
		.replace('>', '&gt;')
		.replace('"', '&quot;')
}

// ---------- insert ----------

fn format_insert_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)
	tbl := if opts.table_name != '' { opts.table_name } else { 'tbl' }
	col_list := names.join(',')

	mut lines := []string{}
	for row in rows {
		mut vals := []string{}
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			if val == '' {
				vals << 'NULL'
			} else {
				vals << "'" + val.replace("'", "''") + "'"
			}
		}
		lines << 'INSERT INTO ${tbl}(${col_list}) VALUES(${vals.join(',')});'
	}
	return lines.join('\n')
}

// ---------- quote ----------

fn format_quote_ex(rows []sqlite.Row, opts FormatOptions) string {
	if rows.len == 0 {
		return ''
	}
	ncols := rows[0].vals.len
	names := resolve_names(rows[0].names, ncols)
	sep := opts.separator

	mut lines := []string{}
	if opts.headers {
		lines << names.join(sep)
	}
	for row in rows {
		mut vals := []string{}
		for i, val in row.vals {
			if i >= ncols {
				break
			}
			if val == '' {
				vals << 'NULL'
			} else {
				vals << "'" + val.replace("'", "''") + "'"
			}
		}
		lines << vals.join(sep)
	}
	return lines.join('\n')
}
