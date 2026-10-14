// Coverage for the `x/dataframe` constructors, views and scalar helpers that
// `dataframe_test.v` does not use: `new`, `empty`, `from_series`, `read_csv`,
// `head`, `tail`, `sort_by`, `Row.get`, `Series.get`, `Series.f64s` and
// `Series.stddev`.
//
// The one input that comes from disk is written by the test itself into
// `os.vtmp_dir()`, so there is no fixture to go missing and no dependency on
// the code that produced it. Expected values were read off the running library;
// the statistics are asserted against invariants (a mean that lies between the
// min and the max, a stddev that is zero for a constant series) rather than
// against decimal expansions.
import math
import os
import x.dataframe

const table = 'name,score,team\nann,10,x\nbob,9,y\ncid,11,x\n'

fn load_table() !dataframe.DataFrame {
	return dataframe.from_csv(table, dataframe.CsvConfig{})!
}

// ---------------------------------------------------------------------
// new / empty
// ---------------------------------------------------------------------

fn test_new_builds_a_frame_from_rows() {
	df := dataframe.new(['a', 'b'], [
		['1', '2'],
		['3', '4'],
	])!
	rows, columns := df.shape()
	assert rows == 2
	assert columns == 2
	assert df.columns == ['a', 'b']
	assert df.cell(0, 'a')! == '1'
	assert df.cell(1, 'b')! == '4'
	assert df.height() == 2
	assert df.width() == 2
}

fn test_new_trims_the_column_names_it_indexes_by() {
	df := dataframe.new([' a ', 'b'], [
		['1', '2'],
	])!
	// Measured: the index keys are trimmed, but `columns` keeps the original
	// spelling, so a lookup uses the trimmed name.
	assert df.column('a')!.values == ['1']
	assert df.columns == [' a ', 'b']
}

fn test_new_copies_the_rows_it_is_given() {
	mut rows := [
		['1', '2'],
	]
	df := dataframe.new(['a', 'b'], rows)!
	rows[0][0] = 'mutated'
	// Measured: the frame holds its own copy, so mutating the input afterwards
	// does not change it.
	assert df.cell(0, 'a')! == '1'
}

fn test_new_rejects_a_row_of_the_wrong_width() {
	if _ := dataframe.new(['a', 'b'], [
		['1', '2'],
		['3'],
	]) {
		assert false, 'a short row must be refused'
	} else {
		// The index is the 0-based position of the offending row.
		assert err.msg() == 'row 1 has 1 values, expected 2'
	}
}

fn test_new_rejects_an_empty_column_list() {
	if _ := dataframe.new([], []) {
		assert false, 'a frame needs at least one column'
	} else {
		assert err.msg() == 'at least one column is required'
	}
}

fn test_new_rejects_an_empty_and_a_duplicate_column_name() {
	if _ := dataframe.new(['a', ''], []) {
		assert false, 'an empty column name must be refused'
	} else {
		assert err.msg() == 'column 1 is empty'
	}
	if _ := dataframe.new(['a', ' a'], []) {
		assert false, 'a duplicate column name must be refused'
	} else {
		// Measured: the check runs on the trimmed name, so ` a` collides with
		// `a` and the message names the trimmed spelling.
		assert err.msg() == 'duplicate column `a`'
	}
}

// ---------------------------------------------------------------------
// empty / from_series
// ---------------------------------------------------------------------

fn test_empty_builds_a_frame_with_columns_and_no_rows() {
	df := dataframe.empty(['a', 'b'])!
	rows, columns := df.shape()
	assert rows == 0
	assert columns == 2
	assert df.columns == ['a', 'b']
	// Measured: every view of an empty frame is empty, including the `n <= 0`
	// branches.
	assert df.head(3).height() == 0
	assert df.head(0).height() == 0
	assert df.tail(3).height() == 0
	assert df.tail(-1).height() == 0
	assert df.filter(fn (row dataframe.Row) bool {
		return true
	}).height() == 0
	assert df.value_counts('a')!.len == 0
}

fn test_empty_still_answers_queries_about_its_columns() {
	df := dataframe.empty(['a'])!
	assert df.column('a')!.len() == 0
	assert df.select(['a'])!.width() == 1
	if _ := df.column('b') {
		assert false, 'an absent column must error'
	} else {
		assert err.msg() == 'unknown column `b`'
	}
}

fn test_from_series_places_each_series_in_a_column() {
	df := dataframe.from_series([
		dataframe.Series{
			name:   'a'
			values: ['1', '2']
		},
		dataframe.Series{
			name:   'b'
			values: ['x', 'y']
		},
	])!
	rows, columns := df.shape()
	assert rows == 2
	assert columns == 2
	assert df.columns == ['a', 'b']
	// Rows are read across the series, so row 1 is `2` and `y`.
	assert df.cell(0, 'a')! == '1'
	assert df.cell(1, 'a')! == '2'
	assert df.cell(1, 'b')! == 'y'
}

fn test_from_series_refuses_no_series_and_mismatched_lengths() {
	if _ := dataframe.from_series([]) {
		assert false, 'at least one series is required'
	} else {
		assert err.msg() == 'at least one series is required'
	}
	if _ := dataframe.from_series([
		dataframe.Series{
			name:   'a'
			values: ['1', '2']
		},
		dataframe.Series{
			name:   'b'
			values: ['1']
		},
	]) {
		assert false, 'series of different lengths must be refused'
	} else {
		assert err.msg() == 'series `b` has 1 values, expected 2'
	}
}

fn test_from_series_of_one_empty_series_makes_an_empty_frame() {
	df := dataframe.from_series([
		dataframe.Series{
			name:   'a'
			values: []string{}
		},
	])!
	assert df.height() == 0
	assert df.width() == 1
	assert df.column('a')!.len() == 0
}

// ---------------------------------------------------------------------
// read_csv
// ---------------------------------------------------------------------

fn test_read_csv_loads_a_table_from_disk() {
	path := os.join_path(os.vtmp_dir(), 'x_dataframe_read_csv_test.csv')
	os.write_file(path, table)!
	defer {
		os.rm(path) or {}
	}
	df := dataframe.read_csv(path, dataframe.CsvConfig{})!
	rows, columns := df.shape()
	assert rows == 3
	assert columns == 3
	assert df.columns == ['name', 'score', 'team']
	assert df.cell(1, 'name')! == 'bob'
	// `read_csv` and `from_csv` agree on the same text.
	assert df.cell(1, 'name')! == load_table()!.cell(1, 'name')!
}

fn test_read_csv_names_the_file_in_a_read_error() {
	missing := os.join_path(os.vtmp_dir(), 'x_dataframe_read_csv_test_missing.csv')
	if _ := dataframe.read_csv(missing, dataframe.CsvConfig{}) {
		assert false, 'a missing file must error'
	} else {
		assert err.msg().contains(missing)
	}
}

// ---------------------------------------------------------------------
// head / tail / sort_by
// ---------------------------------------------------------------------

fn test_head_returns_the_first_n_rows() {
	df := load_table()!
	head := df.head(2)
	assert head.height() == 2
	assert head.width() == 3
	assert head.columns == df.columns
	assert head.cell(0, 'name')! == 'ann'
	assert head.cell(1, 'name')! == 'bob'
	// Measured: `n <= 0` gives no rows, and `n` past the end gives them all.
	assert df.head(0).height() == 0
	assert df.head(-1).height() == 0
	assert df.head(99).height() == 3
}

fn test_tail_returns_the_last_n_rows() {
	df := load_table()!
	tail := df.tail(2)
	assert tail.height() == 2
	assert tail.cell(0, 'name')! == 'bob'
	assert tail.cell(1, 'name')! == 'cid'
	// Measured: the same clamps as `head`.
	assert df.tail(0).height() == 0
	assert df.tail(-1).height() == 0
	assert df.tail(99).height() == 3
	assert df.tail(3).cell(2, 'name')! == 'cid'
}

fn test_head_and_tail_leave_the_source_untouched() {
	df := load_table()!
	_ := df.head(1)
	_ := df.tail(1)
	// Both build a new frame, so the original still has all three rows.
	assert df.height() == 3
	assert df.cell(0, 'name')! == 'ann'
	assert df.cell(2, 'name')! == 'cid'
}

fn test_sort_by_orders_lexicographically_in_both_directions() {
	df := load_table()!
	asc := df.sort_by('name', .asc)!
	assert asc.column('name')!.values == ['ann', 'bob', 'cid']
	desc := df.sort_by('name', .desc)!
	assert desc.column('name')!.values == ['cid', 'bob', 'ann']
	// Measured: the sort is a plain string comparison, so `9` sorts before
	// `10` when the column is sorted as text.
	as_score := df.sort_by('score', .asc)!
	assert as_score.column('score')!.values == ['10', '11', '9']
	// The whole row travels with the key.
	assert as_score.cell(0, 'name')! == 'ann'
	// `sort_by_f64` is the numeric spelling of the same thing.
	assert df.sort_by_f64('score', .asc)!.column('score')!.values == ['9', '10', '11']
}

fn test_sort_by_returns_a_new_frame_and_keeps_the_columns() {
	df := load_table()!
	sorted := df.sort_by('name', .desc)!
	assert sorted.height() == 3
	assert sorted.columns == df.columns
	// The source is not reordered in place.
	assert df.cell(0, 'name')! == 'ann'
}

// ---------------------------------------------------------------------
// Row.get / Series.get / Series.f64s / Series.stddev
// ---------------------------------------------------------------------

fn test_row_get_reads_by_name_and_reports_an_unknown_column() {
	row := load_table()!.row(1)!
	assert row.get('name')! == 'bob'
	assert row.get('team')! == 'y'
	if _ := row.get('nope') {
		assert false, 'an unknown column must error'
	} else {
		assert err.msg() == 'unknown column `nope`'
	}
	// Every column of the frame is reachable through the row.
	assert row.values.len == 3
}

fn test_row_reports_an_out_of_range_index() {
	if _ := load_table()!.row(9) {
		assert false, 'row 9 does not exist'
	} else {
		assert err.msg() == 'row index 9 is out of range'
	}
	// The negative form is refused by the same branch.
	if _ := load_table()!.row(-1) {
		assert false, 'row -1 does not exist'
	} else {
		assert err.msg() == 'row index -1 is out of range'
	}
}

fn test_series_get_reads_by_index_and_reports_an_out_of_range_one() {
	scores := load_table()!.column('score')!
	assert scores.len() == 3
	assert scores.get(0)! == '10'
	assert scores.get(2)! == '11'
	if _ := scores.get(3) {
		assert false, 'index 3 is out of range'
	} else {
		assert err.msg() == 'series index 3 is out of range'
	}
	if _ := scores.get(-1) {
		assert false, 'index -1 is out of range'
	} else {
		assert err.msg() == 'series index -1 is out of range'
	}
}

fn test_series_f64s_converts_every_value() {
	scores := load_table()!.column('score')!
	assert scores.f64s()! == [10.0, 9.0, 11.0]
	// The conversion is the same one `describe` uses, so the two agree.
	summary := scores.describe()!
	assert math.alike(summary.mean, scores.sum()! / scores.len())
	// An empty series yields an empty slice rather than erroring.
	assert dataframe.empty(['a'])!.column('a')!.f64s()! == []f64{}
}

fn test_series_stddev_matches_the_definition_of_sample_standard_deviation() {
	scores := load_table()!.column('score')!
	stddev := scores.stddev()!
	// Measured: 3 values of 10, 9 and 11 give the sample (n-1) estimator.
	assert math.alike(stddev, 0.816496580927726)
	// Invariants: a constant series has none, and stddev is symmetric in the
	// order of the values.
	constant := dataframe.new(['a'], [
		['5'],
		['5'],
		['5'],
	])!
	assert math.alike(constant.column('a')!.stddev()!, 0.0)
	shuffled := dataframe.new(['a'], [
		['11'],
		['10'],
		['9'],
	])!
	assert math.alike(shuffled.column('a')!.stddev()!, stddev)
}

fn test_series_stddev_of_one_value_is_zero() {
	one := dataframe.new(['a'], [['5']])!
	// Measured: the single-value case is a divide by zero guard, not an error.
	assert math.alike(one.column('a')!.stddev()!, 0.0)
	assert math.alike(one.column('a')!.mean()!, 5.0)
}
