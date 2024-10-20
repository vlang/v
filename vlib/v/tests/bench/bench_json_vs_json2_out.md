```sh
benchmark_measure_json_vs_json2_on_complex_struct
SPENT    1.886 ms in json2.decode
SPENT    0.344 ms in json.decode

SPENT    0.463 ms in json2.encode Person
SPENT    0.433 ms in  json.encode Person


benchmark_measure_decode_by_type
SPENT    0.488 ms in json2.decode StructType[string]
SPENT    0.079 ms in  json.decode StructType[string]

SPENT    0.461 ms in json2.decode StructType[bool]
SPENT    0.067 ms in  json.decode StructType[bool]

SPENT    0.498 ms in json2.decode StructType[int]
SPENT    0.105 ms in  json.decode StructType[int]

SPENT    1.486 ms in json2.decode StructType[time.Time]
SPENT    0.117 ms in  json.decode StructType[time.Time]


benchmark_measure_encode_by_type
✔な🐈	 = 847
SPENT    3.419 ms in json2.encode StructType[string]
SPENT    1.386 ms in  json.encode StructType[string]

🐈🐟🐧🐀💀 = 500
SPENT    0.514 ms in json2.encode StructType[string]
SPENT    0.709 ms in  json.encode StructType[string]

ひらがな = 372
SPENT    1.430 ms in json2.encode StructType[string]
SPENT    0.423 ms in  json.encode StructType[string]

big string length = 599
SPENT    1.487 ms in json2.encode StructType[string]
SPENT    0.577 ms in  json.encode StructType[string]

empty string
SPENT    0.051 ms in json2.encode StructType[string]
SPENT    0.170 ms in  json.encode StructType[string]

empty time.Time
SPENT    0.146 ms in json2.encode StructType[time.Time]
SPENT    0.188 ms in  json.encode StructType[time.Time]

time.utc()
SPENT    0.143 ms in json2.encode StructType[time.Time]
SPENT    0.210 ms in  json.encode StructType[time.Time]

time.now()
SPENT    1.362 ms in json2.encode StructType[time.Time]
SPENT    0.180 ms in  json.encode StructType[time.Time]

SPENT    0.074 ms in json2.encode StructType[int]
SPENT    0.181 ms in  json.encode StructType[int]

SPENT    0.120 ms in json2.encode StructType[u64]
SPENT    0.865 ms in  json.encode StructType[u64]

SPENT    0.091 ms in json2.encode StructType[f64]
SPENT    0.182 ms in  json.encode StructType[f64]

SPENT    0.044 ms in json2.encode StructType[bool]
SPENT    0.130 ms in  json.encode StructType[bool]

empty array
SPENT    0.157 ms in json2.encode StructType[[]int]
SPENT    0.139 ms in  json.encode StructType[[]int]

array with 10 elements [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
SPENT    0.623 ms in json2.encode StructType[[]int]
SPENT    0.624 ms in  json.encode StructType[[]int]

SPENT    0.290 ms in json2.encode StructType[StructType[int]]
SPENT    0.235 ms in  json.encode StructType[StructType[int]]

SPENT    0.051 ms in json2.encode StructType[Enum]
SPENT    0.149 ms in  json.encode StructType[Enum]

SPENT    0.291 ms in json2.encode StructType[SumTypes]
SPENT    0.182 ms in  json.encode StructType[SumTypes]


benchmark_measure_encode_by_alias_type
SPENT    0.065 ms in json2.encode StructType[StringAlias]
SPENT    0.211 ms in  json.encode StructType[StringAlias]

SPENT    0.587 ms in json2.encode StructType[TimeAlias]
SPENT    0.933 ms in  json.encode StructType[TimeAlias]

SPENT    0.049 ms in json2.encode StructType[IntAlias]
SPENT    0.165 ms in  json.encode StructType[IntAlias]

SPENT    0.080 ms in json2.encode StructType[BoolAlias]
SPENT    0.146 ms in  json.encode StructType[BoolAlias]

SPENT    0.488 ms in json2.encode StructType[StructAlias]
SPENT    0.239 ms in  json.encode StructType[StructAlias]

```
