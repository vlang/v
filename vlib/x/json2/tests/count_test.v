import x.json2 as json
import time

const fixed_time = time.Time{
	year: 2022
	month: 3
	day: 11
	hour: 13
	minute: 54
	second: 25
	unix: 1647006865
}

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

fn test_empty() {
	mut count := json.Count{0}

	count.count_chars(map[string]string{})
	assert json.encode(map[string]string{}).len == count.get_total()

	count.reset_total()
	count.count_chars([]string{})
	assert json.encode([]string{}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[bool]{})
	assert json.encode(StructType[bool]{}).len == count.get_total()
}

fn test_types() {
	mut count := json.Count{0}

	count.reset_total()
	count.count_chars(StructType[string]{})
	assert json.encode(StructType[string]{}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[string]{})
	assert json.encode(StructType[string]{ val: '' }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[string]{ val: 'abcd' })
	assert json.encode(StructType[string]{ val: 'abcd' }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[bool]{ val: false })
	assert json.encode(StructType[bool]{ val: false }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[bool]{ val: true })
	assert json.encode(StructType[bool]{ val: true }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[int]{ val: 26 })
	assert json.encode(StructType[int]{ val: 26 }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[int]{ val: 1 })
	assert json.encode(StructType[int]{ val: 1 }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[int]{ val: -125 })
	assert json.encode(StructType[int]{ val: -125 }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[u64]{ val: u64(-1) })
	assert json.encode(StructType[u64]{ val: u64(-1) }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[time.Time]{})
	assert json.encode(StructType[time.Time]{}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[time.Time]{ val: fixed_time })
	assert json.encode(StructType[time.Time]{ val: fixed_time }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[StructType[int]]{
		val: StructType[int]{
			val: 1
		}
	})
	assert json.encode(StructType[StructType[int]]{
		val: StructType[int]{
			val: 1
		}
	}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[Enumerates]{})
	assert json.encode(StructType[Enumerates]{}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[Enumerates]{})
	assert json.encode(StructType[Enumerates]{}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[Enumerates]{ val: Enumerates.f })
	assert json.encode(StructType[Enumerates]{ val: Enumerates.f }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[[]int]{})
	assert json.encode(StructType[[]int]{}).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[[]int]{ val: [0] })
	assert json.encode(StructType[[]int]{ val: [0] }).len == count.get_total()

	count.reset_total()
	count.count_chars(StructType[[]int]{ val: [0, 1, 0, 2, 3, 2, 5, 1] })
	assert json.encode(StructType[[]int]{ val: [0, 1, 0, 2, 3, 2, 5, 1] }).len == count.get_total()
}
