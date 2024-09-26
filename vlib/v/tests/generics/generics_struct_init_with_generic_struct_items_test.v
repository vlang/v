struct Item[T] {
	val T
}

struct Items[A, B] {
	item1 Item[A]
	item2 Item[B]
}

fn combine[A, B](a A, b B) Items[A, B] {
	return Items[A, B]{
		item1: Item[A]{
			val: a
		}
		item2: Item[B]{
			val: b
		}
	}
}

fn test_generic_struct_with_generic_struct_items() {
	_ := Items[int, string]{Item{42}, Item{'bye'}}
	_ := Items[int, string]{
		item1: Item{42}
		item2: Item{'bye'}
	}
	_ := Items[int, string]{
		item1: Item{
			val: 42
		}
		item2: Item{
			val: 'bye'
		}
	}
	_ := Items[int, string]{
		item1: Item[int]{
			val: 42
		}
		item2: Item[string]{
			val: 'bye'
		}
	}
	_ := Items[string, int]{
		item1: Item[string]{
			val: 'hallo'
		}
		item2: Item[int]{
			val: 42
		}
	}
	_ := Items[string, string]{
		item1: Item[string]{
			val: 'hallo'
		}
		item2: Item[string]{
			val: 'bye'
		}
	}

	//  calling function all combination OK
	r1 := combine('Hallo', 42)
	println(r1)
	assert r1.item1.val == 'Hallo'
	assert r1.item2.val == 42

	r2 := combine(42, 'Hallo')
	println(r2)
	assert r2.item1.val == 42
	assert r2.item2.val == 'Hallo'

	r3 := combine('Hallo', 'Bye')
	println(r3)
	assert r3.item1.val == 'Hallo'
	assert r3.item2.val == 'Bye'

	r4 := combine(42, 44)
	println(r4)
	assert r4.item1.val == 42
	assert r4.item2.val == 44

	r5 := combine(`🥸`, `🤡`)
	println(r5)
	assert r5.item1.val == `🥸`
	assert r5.item2.val == `🤡`
}
