module dataframe

import encoding.csv
import math
import strconv

// CsvConfig configures CSV loading for a DataFrame.
@[params]
pub struct CsvConfig {
pub:
	has_header   bool = true
	separator    u8   = `,`
	comment      u8   = `#`
	default_cell string
	empty_cell   string
	end_line_len int = csv.endline_cr_len
	quote        u8  = `"`
	quote_remove bool
}

// SortOrder controls the direction used by sorting helpers.
pub enum SortOrder {
	asc
	desc
}

// Summary contains basic numeric statistics for a Series.
pub struct Summary {
pub:
	count  int
	sum    f64
	mean   f64
	min    f64
	max    f64
	median f64
	stddev f64
}

// Series is a single named string column with numeric helpers.
pub struct Series {
pub:
	name   string
	values []string
}

// Row is a named view of a DataFrame row.
pub struct Row {
pub:
	values map[string]string
}

// DataFrame stores rectangular tabular data as string cells.
pub struct DataFrame {
	index map[string]int
pub:
	columns []string
	rows    [][]string
}

// new creates a DataFrame from column names and rows.
pub fn new(columns []string, rows [][]string) !DataFrame {
	index := build_index(columns)!
	mut copied_rows := [][]string{cap: rows.len}
	for row_index, row in rows {
		if row.len != columns.len {
			return error('row ${row_index} has ${row.len} values, expected ${columns.len}')
		}
		copied_rows << row.clone()
	}
	return DataFrame{
		index:   index
		columns: columns.clone()
		rows:    copied_rows
	}
}

// empty creates an empty DataFrame with the given columns.
pub fn empty(columns []string) !DataFrame {
	return new(columns, [][]string{})!
}

// from_series creates a DataFrame from named columns.
pub fn from_series(series []Series) !DataFrame {
	if series.len == 0 {
		return error('at least one series is required')
	}
	row_count := series[0].values.len
	mut columns := []string{cap: series.len}
	for s in series {
		if s.values.len != row_count {
			return error('series `${s.name}` has ${s.values.len} values, expected ${row_count}')
		}
		columns << s.name
	}
	mut rows := [][]string{cap: row_count}
	for row_index in 0 .. row_count {
		mut row := []string{cap: series.len}
		for s in series {
			row << s.values[row_index]
		}
		rows << row
	}
	return new(columns, rows)!
}

// from_columns creates a DataFrame from a map of columns.
pub fn from_columns(columns map[string][]string) !DataFrame {
	if columns.len == 0 {
		return error('at least one column is required')
	}
	mut names := columns.keys()
	names.sort()
	mut series := []Series{cap: names.len}
	for name in names {
		series << Series{
			name:   name
			values: columns[name].clone()
		}
	}
	return from_series(series)!
}

// from_csv creates a DataFrame from CSV text.
pub fn from_csv(text string, cfg CsvConfig) !DataFrame {
	if text.len == 0 {
		return error('csv input is empty')
	}
	mut reader := csv.csv_reader(
		scr_buf:      text.str
		scr_buf_len:  text.len
		separator:    cfg.separator
		comment:      cfg.comment
		default_cell: cfg.default_cell
		empty_cell:   cfg.empty_cell
		end_line_len: cfg.end_line_len
		quote:        cfg.quote
		quote_remove: cfg.quote_remove
	)!
	defer {
		reader.dispose_csv_reader()
	}
	return from_csv_reader(mut reader, cfg)!
}

// read_csv creates a DataFrame from a CSV file.
pub fn read_csv(path string, cfg CsvConfig) !DataFrame {
	mut reader := csv.csv_reader(
		file_path:    path
		separator:    cfg.separator
		comment:      cfg.comment
		default_cell: cfg.default_cell
		empty_cell:   cfg.empty_cell
		end_line_len: cfg.end_line_len
		quote:        cfg.quote
		quote_remove: cfg.quote_remove
	)!
	defer {
		reader.dispose_csv_reader()
	}
	return from_csv_reader(mut reader, cfg)!
}

fn from_csv_reader(mut reader csv.RandomAccessReader, cfg CsvConfig) !DataFrame {
	if reader.csv_map.len == 0 {
		return error('csv input has no rows')
	}
	first_row := reader.get_row(0)!
	if first_row.len == 0 {
		return error('csv input has no columns')
	}
	columns := if cfg.has_header {
		normalize_columns(first_row)
	} else {
		default_columns(first_row.len)
	}
	start_row := if cfg.has_header { 1 } else { 0 }
	mut rows := [][]string{cap: math.max(0, reader.csv_map.len - start_row)}
	for row_index in start_row .. reader.csv_map.len {
		row := reader.get_row(row_index)!
		rows << normalize_row(row, columns.len, cfg.empty_cell)
	}
	return new(columns, rows)!
}

// height returns the number of rows.
pub fn (df DataFrame) height() int {
	return df.rows.len
}

// width returns the number of columns.
pub fn (df DataFrame) width() int {
	return df.columns.len
}

// shape returns the row and column count.
pub fn (df DataFrame) shape() (int, int) {
	return df.rows.len, df.columns.len
}

// cell returns a single cell by row index and column name.
pub fn (df DataFrame) cell(row_index int, column string) !string {
	if row_index < 0 || row_index >= df.rows.len {
		return error('row index ${row_index} is out of range')
	}
	column_index := df.column_index(column)!
	return df.rows[row_index][column_index]
}

// row returns a named row by index.
pub fn (df DataFrame) row(row_index int) !Row {
	if row_index < 0 || row_index >= df.rows.len {
		return error('row index ${row_index} is out of range')
	}
	mut values := map[string]string{}
	row := df.rows[row_index]
	for column_index, column in df.columns {
		values[column] = row[column_index]
	}
	return Row{
		values: values
	}
}

// column returns a Series by column name.
pub fn (df DataFrame) column(name string) !Series {
	column_index := df.column_index(name)!
	mut values := []string{cap: df.rows.len}
	for row in df.rows {
		values << row[column_index]
	}
	return Series{
		name:   name
		values: values
	}
}

// select returns a DataFrame with only the requested columns.
pub fn (df DataFrame) select(names []string) !DataFrame {
	mut column_indices := []int{cap: names.len}
	for name in names {
		column_indices << df.column_index(name)!
	}
	mut rows := [][]string{cap: df.rows.len}
	for row in df.rows {
		mut selected := []string{cap: names.len}
		for column_index in column_indices {
			selected << row[column_index]
		}
		rows << selected
	}
	return new(names, rows)!
}

// head returns the first n rows.
pub fn (df DataFrame) head(n int) DataFrame {
	if n <= 0 {
		return df.with_rows([][]string{})
	}
	end := math.min(n, df.rows.len)
	return df.with_rows(df.rows[..end])
}

// tail returns the last n rows.
pub fn (df DataFrame) tail(n int) DataFrame {
	if n <= 0 {
		return df.with_rows([][]string{})
	}
	start := math.max(0, df.rows.len - n)
	return df.with_rows(df.rows[start..])
}

// filter returns rows accepted by the predicate.
pub fn (df DataFrame) filter(predicate fn (Row) bool) DataFrame {
	mut rows := [][]string{cap: df.rows.len}
	for row_index in 0 .. df.rows.len {
		row := df.row(row_index) or { continue }
		if predicate(row) {
			rows << df.rows[row_index].clone()
		}
	}
	return df.with_rows(rows)
}

// sort_by returns rows sorted lexicographically by column.
pub fn (df DataFrame) sort_by(name string, order SortOrder) !DataFrame {
	column_index := df.column_index(name)!
	return df.sorted(column_index, order, false)!
}

// sort_by_f64 returns rows sorted numerically by column.
pub fn (df DataFrame) sort_by_f64(name string, order SortOrder) !DataFrame {
	column_index := df.column_index(name)!
	return df.sorted(column_index, order, true)!
}

// value_counts counts unique values in a column.
pub fn (df DataFrame) value_counts(name string) !map[string]int {
	column_index := df.column_index(name)!
	mut counts := map[string]int{}
	for row in df.rows {
		value := row[column_index]
		counts[value]++
	}
	return counts
}

// describe returns numeric statistics for a named column.
pub fn (df DataFrame) describe(name string) !Summary {
	series := df.column(name)!
	return series.describe()!
}

fn (df DataFrame) sorted(column_index int, order SortOrder, numeric bool) !DataFrame {
	mut rows := clone_rows(df.rows)
	for i in 1 .. rows.len {
		mut j := i
		for j > 0 {
			if !should_swap(rows[j - 1][column_index], rows[j][column_index], order, numeric)! {
				break
			}
			rows[j - 1], rows[j] = rows[j], rows[j - 1]
			j--
		}
	}
	return df.with_rows(rows)
}

fn (df DataFrame) with_rows(rows [][]string) DataFrame {
	return DataFrame{
		index:   df.index.clone()
		columns: df.columns.clone()
		rows:    clone_rows(rows)
	}
}

fn (df DataFrame) column_index(name string) !int {
	if name !in df.index {
		return error('unknown column `${name}`')
	}
	return df.index[name]
}

// get returns a value from the Row by column name.
pub fn (row Row) get(name string) !string {
	if name !in row.values {
		return error('unknown column `${name}`')
	}
	return row.values[name]
}

// len returns the number of values in the Series.
pub fn (s Series) len() int {
	return s.values.len
}

// get returns a value from the Series by row index.
pub fn (s Series) get(index int) !string {
	if index < 0 || index >= s.values.len {
		return error('series index ${index} is out of range')
	}
	return s.values[index]
}

// f64s converts every value in the Series to f64.
pub fn (s Series) f64s() ![]f64 {
	mut values := []f64{cap: s.values.len}
	for index, raw_value in s.values {
		values << parse_f64(raw_value, '${s.name}[${index}]')!
	}
	return values
}

// sum returns the numeric sum of the Series.
pub fn (s Series) sum() !f64 {
	values := s.f64s()!
	mut total := 0.0
	for value in values {
		total += value
	}
	return total
}

// mean returns the numeric mean of the Series.
pub fn (s Series) mean() !f64 {
	values := s.f64s()!
	if values.len == 0 {
		return error('series `${s.name}` is empty')
	}
	return sum_f64(values) / f64(values.len)
}

// min returns the smallest numeric value in the Series.
pub fn (s Series) min() !f64 {
	values := s.f64s()!
	if values.len == 0 {
		return error('series `${s.name}` is empty')
	}
	mut min_value := values[0]
	for value in values[1..] {
		if value < min_value {
			min_value = value
		}
	}
	return min_value
}

// max returns the largest numeric value in the Series.
pub fn (s Series) max() !f64 {
	values := s.f64s()!
	if values.len == 0 {
		return error('series `${s.name}` is empty')
	}
	mut max_value := values[0]
	for value in values[1..] {
		if value > max_value {
			max_value = value
		}
	}
	return max_value
}

// median returns the numeric median of the Series.
pub fn (s Series) median() !f64 {
	mut values := s.f64s()!
	if values.len == 0 {
		return error('series `${s.name}` is empty')
	}
	values.sort()
	return median_f64(values)
}

// stddev returns the population standard deviation of the Series.
pub fn (s Series) stddev() !f64 {
	values := s.f64s()!
	if values.len == 0 {
		return error('series `${s.name}` is empty')
	}
	mean := sum_f64(values) / f64(values.len)
	mut variance_sum := 0.0
	for value in values {
		diff := value - mean
		variance_sum += diff * diff
	}
	return math.sqrt(variance_sum / f64(values.len))
}

// describe returns basic numeric statistics for the Series.
pub fn (s Series) describe() !Summary {
	mut values := s.f64s()!
	if values.len == 0 {
		return error('series `${s.name}` is empty')
	}
	values.sort()
	sum := sum_f64(values)
	mean := sum / f64(values.len)
	mut variance_sum := 0.0
	for value in values {
		diff := value - mean
		variance_sum += diff * diff
	}
	return Summary{
		count:  values.len
		sum:    sum
		mean:   mean
		min:    values.first()
		max:    values.last()
		median: median_f64(values)
		stddev: math.sqrt(variance_sum / f64(values.len))
	}
}

fn build_index(columns []string) !map[string]int {
	if columns.len == 0 {
		return error('at least one column is required')
	}
	mut index := map[string]int{}
	for column_index, column in columns {
		name := column.trim_space()
		if name.len == 0 {
			return error('column ${column_index} is empty')
		}
		if name in index {
			return error('duplicate column `${name}`')
		}
		index[name] = column_index
	}
	return index
}

fn clone_rows(rows [][]string) [][]string {
	mut copied_rows := [][]string{cap: rows.len}
	for row in rows {
		copied_rows << row.clone()
	}
	return copied_rows
}

fn default_columns(count int) []string {
	mut columns := []string{cap: count}
	for index in 0 .. count {
		columns << 'column_${index}'
	}
	return columns
}

fn normalize_columns(columns []string) []string {
	mut normalized := []string{cap: columns.len}
	for column in columns {
		normalized << column.trim_space()
	}
	return normalized
}

fn normalize_row(row []string, column_count int, fill string) []string {
	mut normalized := []string{cap: column_count}
	for column_index in 0 .. column_count {
		if column_index < row.len {
			normalized << row[column_index]
		} else {
			normalized << fill
		}
	}
	return normalized
}

fn should_swap(left string, right string, order SortOrder, numeric bool) !bool {
	comparison := if numeric {
		compare_f64(left, right)!
	} else {
		compare_string(left, right)
	}
	return match order {
		.asc { comparison > 0 }
		.desc { comparison < 0 }
	}
}

fn compare_string(left string, right string) int {
	if left < right {
		return -1
	}
	if left > right {
		return 1
	}
	return 0
}

fn compare_f64(left string, right string) !int {
	left_number := parse_f64(left, 'left value')!
	right_number := parse_f64(right, 'right value')!
	if left_number < right_number {
		return -1
	}
	if left_number > right_number {
		return 1
	}
	return 0
}

fn parse_f64(value string, label string) !f64 {
	trimmed_value := value.trim_space()
	$if js {
		return strconv.atof64(trimmed_value) or { error('${label} is not a number: `${value}`') }
	} $else {
		return strconv.atof64(trimmed_value, strconv.AtoF64Param{}) or {
			error('${label} is not a number: `${value}`')
		}
	}
}

fn sum_f64(values []f64) f64 {
	mut total := 0.0
	for value in values {
		total += value
	}
	return total
}

fn median_f64(sorted_values []f64) f64 {
	mid := sorted_values.len / 2
	if sorted_values.len % 2 == 1 {
		return sorted_values[mid]
	}
	return (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
}
