import math
import x.dataframe

const prices_csv = 'symbol,price,qty
AAPL,189.5,2
MSFT,420.25,3
AAPL,191.0,5
'

fn test_from_csv_and_numeric_series() {
	df := dataframe.from_csv(prices_csv, dataframe.CsvConfig{})!
	rows, columns := df.shape()
	assert rows == 3
	assert columns == 3
	assert df.columns == ['symbol', 'price', 'qty']
	assert df.cell(1, 'symbol')! == 'MSFT'

	prices := df.column('price')!
	assert prices.len() == 3
	assert math.alike(prices.mean()!, 266.9166666666667)
	assert math.alike(prices.min()!, 189.5)
	assert math.alike(prices.max()!, 420.25)
	assert math.alike(prices.median()!, 191.0)

	summary := df.describe('qty')!
	assert summary.count == 3
	assert math.alike(summary.sum, 10.0)
	assert math.alike(summary.mean, 3.3333333333333335)
}

fn test_select_filter_sort_and_value_counts() {
	df := dataframe.from_csv(prices_csv, dataframe.CsvConfig{})!
	aapl := df.filter(fn (row dataframe.Row) bool {
		return row.values['symbol'] == 'AAPL'
	})
	assert aapl.height() == 2

	selected := aapl.select(['symbol', 'qty'])!
	assert selected.width() == 2
	assert selected.cell(1, 'qty')! == '5'

	sorted := df.sort_by_f64('price', .asc)!
	assert sorted.cell(0, 'symbol')! == 'AAPL'
	assert sorted.cell(2, 'symbol')! == 'MSFT'

	counts := df.value_counts('symbol')!
	assert counts['AAPL'] == 2
	assert counts['MSFT'] == 1
}

fn test_from_columns_and_csv_without_header() {
	df := dataframe.from_columns({
		'b': ['2', '1']
		'a': ['x', 'y']
	})!
	assert df.columns == ['a', 'b']
	assert df.cell(0, 'a')! == 'x'
	assert math.alike(df.column('b')!.median()!, 1.5)

	no_header := dataframe.from_csv('AAPL,189.5
MSFT,420.25
', dataframe.CsvConfig{
		has_header: false
	})!
	assert no_header.columns == ['column_0', 'column_1']
	assert no_header.cell(1, 'column_1')! == '420.25'
}
