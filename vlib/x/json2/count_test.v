module json2

import time

const fixed_time = time.new(
	year:   2022
	month:  3
	day:    11
	hour:   13
	minute: 54
	second: 25
)

type StringAlias = string
type BoolAlias = bool
type IntAlias = int
type TimeAlias = time.Time
type StructAlias = StructType[int]
type EnumAlias = Enumerates

type SumTypes = StructType[string] | []SumTypes | []string | bool | int | string | time.Time

enum Enumerates {
	a
	b
	c
	d
	e = 99
	f
}

struct StructType[T] {
mut:
	val T
}

struct StructTypeOption[T] {
mut:
	val ?T
}

struct StructTypePointer[T] {
mut:
	val &T
}

fn count_test[T](value T) {
	mut count := Count{0}

	count.count_chars(value)
	assert encode(value).len == count.get_total()
}

fn test_empty() {
	count_test(map[string]string{})

	count_test([]string{})

	count_test(StructType[bool]{})

	count_test(map[string]string{})
}

fn test_types() {
	count_test(StructType[string]{})

	count_test(StructType[string]{ val: '' })

	count_test(StructType[string]{ val: 'abcd' })

	count_test(StructType[bool]{ val: false })

	count_test(StructType[bool]{ val: true })

	count_test(StructType[int]{ val: 26 })

	count_test(StructType[int]{ val: 1 })

	count_test(StructType[int]{ val: -125 })

	count_test(StructType[u64]{ val: u64(-1) })

	count_test(StructType[time.Time]{})

	count_test(StructType[time.Time]{ val: fixed_time })

	count_test(StructType[StructType[int]]{
		val: StructType[int]{
			val: 1
		}
	})

	count_test(StructType[Enumerates]{})
	count_test(StructType[Enumerates]{})
	count_test(StructType[Enumerates]{ val: Enumerates.f })
	count_test(StructType[[]int]{})
	count_test(StructType[[]int]{ val: [0] })
	count_test(StructType[[]int]{ val: [0, 1, 0, 2, 3, 2, 5, 1] })
}
