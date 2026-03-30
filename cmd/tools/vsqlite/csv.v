module main

import db.sqlite
import os

// read_csv reads a CSV file (comma-separated) and returns (headers, data_rows).
pub fn read_csv(path string) !([]string, [][]string) {
	return read_csv_sep(path, `,`)
}

// read_tsv reads a TSV (tab-separated) file and returns (headers, data_rows).
pub fn read_tsv(path string) !([]string, [][]string) {
	return read_csv_sep(path, `\t`)
}

// read_csv_sep reads a delimited file with sep as the field separator.
pub fn read_csv_sep(path string, sep u8) !([]string, [][]string) {
	content := os.read_file(path)!
	if content.len == 0 {
		return error('file is empty')
	}
	records := parse_csv_records(content, sep)
	if records.len == 0 {
		return error('CSV file is empty')
	}
	return records[0], records[1..]
}

// write_csv writes rows to a CSV file.
pub fn write_csv(path string, rows []sqlite.Row, headers bool) ! {
	mut lines := []string{}
	if headers && rows.len > 0 {
		lines << rows[0].names.map(csv_escape(it)).join(',')
	}
	for row in rows {
		lines << row.vals.map(csv_escape(it)).join(',')
	}
	os.write_file(path, lines.join('\n') + '\n')!
}

// csv_escape quotes a field for RFC 4180 CSV (comma separator).
pub fn csv_escape(s string) string {
	return csv_escape_sep(s, ',')
}

// csv_escape_sep quotes a field if it contains the separator, a double-quote, or a newline.
fn csv_escape_sep(s string, sep string) string {
	if s.contains(sep) || s.contains('"') || s.contains('\n') {
		return '"' + s.replace('"', '""') + '"'
	}
	return s
}

// parse_csv_line parses a single CSV line (comma separator).
pub fn parse_csv_line(line string) []string {
	return parse_csv_line_sep(line, `,`)
}

// parse_csv_line_sep parses a single delimited line with a custom separator byte.
pub fn parse_csv_line_sep(line string, sep u8) []string {
	mut fields := []string{}
	mut field := ''
	mut in_quotes := false
	mut i := 0
	bytes := line.bytes()
	for i < bytes.len {
		c := bytes[i]
		if in_quotes {
			if c == `"` {
				if i + 1 < bytes.len && bytes[i + 1] == `"` {
					field += '"'
					i += 2
					continue
				} else {
					in_quotes = false
				}
			} else {
				field += c.ascii_str()
			}
		} else {
			if c == `"` {
				in_quotes = true
			} else if c == sep {
				fields << field
				field = ''
			} else {
				field += c.ascii_str()
			}
		}
		i++
	}
	fields << field
	return fields
}

// parse_csv_records parses a full CSV/TSV content string into a slice of records.
pub fn parse_csv_records(content string, sep u8) [][]string {
	mut records := [][]string{}
	mut record := []string{}
	mut field := ''
	mut in_quotes := false
	mut i := 0
	bytes := content.bytes()
	for i < bytes.len {
		c := bytes[i]
		if in_quotes {
			if c == `"` {
				if i + 1 < bytes.len && bytes[i + 1] == `"` {
					field += '"'
					i += 2
					continue
				}
				in_quotes = false
			} else {
				field += c.ascii_str()
			}
		} else {
			if c == `"` {
				in_quotes = true
			} else if c == sep {
				record << field
				field = ''
			} else if c == `\r` {
				if i + 1 < bytes.len && bytes[i + 1] == `\n` {
					i++
				}
				record << field
				field = ''
				if record.len > 1 || (record.len == 1 && record[0] != '') {
					records << record
				}
				record = []string{}
			} else if c == `\n` {
				record << field
				field = ''
				if record.len > 1 || (record.len == 1 && record[0] != '') {
					records << record
				}
				record = []string{}
			} else {
				field += c.ascii_str()
			}
		}
		i++
	}
	if field.len > 0 || record.len > 0 {
		record << field
		if record.len > 1 || (record.len == 1 && record[0] != '') {
			records << record
		}
	}
	return records
}
