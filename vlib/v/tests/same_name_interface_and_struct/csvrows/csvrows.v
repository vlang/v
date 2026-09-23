module csvrows

import encoding.csv

// first_row returns the first row of the CSV `text`, loading `csv.Reader`.
pub fn first_row(text string) []string {
	mut r := csv.new_reader(text)
	return r.read() or { []string{} }
}
