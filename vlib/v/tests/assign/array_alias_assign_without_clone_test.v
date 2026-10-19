// Like V1, the `array2 = array1.clone()` diagnostic is limited to plain array
// types; values of an array type alias may be assigned directly.
struct SumItem {
	value f64
}

type SumItems = []SumItem

struct SumHolder {
mut:
	items SumItems
}

fn doubled(rounds int) SumItems {
	mut values := SumItems{
		len: 1
	}
	values[0] = SumItem{1}
	for _ in 0 .. rounds {
		mut temp := SumItems{}
		for item in values {
			temp << SumItem{item.value * 2}
		}
		values = temp
	}
	return values
}

fn test_array_alias_assign_without_clone() {
	assert doubled(3)[0].value == 8
	holder := SumHolder{
		items: [SumItem{3}]
	}
	mut items := holder.items
	items << SumItem{4}
	assert items.len == 2
	assert holder.items.len == 1
}
