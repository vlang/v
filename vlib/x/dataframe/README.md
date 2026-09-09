# DataFrame

`x.dataframe` provides a small experimental tabular data API. It is intended as
a foundation for data analysis workflows that need DataFrame-style operations
without leaving V.

The module stores cells as strings and provides numeric helpers on `Series` for
common analysis tasks.

```v
import x.dataframe

const prices = 'symbol,price,qty
AAPL,189.5,2
MSFT,420.25,3
'

fn main() {
	df := dataframe.from_csv(prices, dataframe.CsvConfig{})!
	println(df.shape())

	price := df.column('price')!
	println(price.mean()!)

	liquid := df.filter(fn (row dataframe.Row) bool {
		return row.values['qty'].int() >= 3
	})
	println(liquid.rows)
}
```

## Features

- Load tabular data from CSV strings or files.
- Access cells, rows, and columns by name.
- Select columns and filter rows.
- Sort by string or numeric column values.
- Count distinct column values.
- Calculate `sum`, `mean`, `min`, `max`, `median`, `stddev`, and `describe`
  summaries for numeric columns.

The API is experimental and may change while the module is under `x`.
