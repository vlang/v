type SumFieldValue = int | string
type SumFieldOne = SumFieldValue
type SumFieldTwo = SumFieldOne
type SumFieldThree = SumFieldTwo

type SumFieldUnrelated = bool | f64

struct SumFieldBox {
mut:
	a SumFieldOne
}

fn (b &SumFieldBox) sum_field_is_int() bool {
	return b.a is int
}

struct SumFieldBox2 {
	a SumFieldTwo
}

struct SumFieldBox3 {
	a SumFieldThree
}

struct SumFieldPlain {
	a SumFieldValue
}

struct SumFieldGeneric[T] {
	a T
}

fn sum_field_next_index(mut count []int) int {
	count[0]++
	return 0
}

fn test_sumtype_alias_field_is_checks_one_level() {
	b := SumFieldBox{ a: SumFieldOne(SumFieldValue('text')) }
	mut actual := ''
	if b.a is int {
		actual = 'int'
	} else {
		actual = 'not int'
	}
	assert actual == 'not int'
	mut matched_1 := false
	if b.a is string {
		matched_1 = true
	}
	assert matched_1 == true
	mut matched_2 := false
	if b.a !is int {
		matched_2 = true
	}
	assert matched_2 == true

	c := SumFieldBox{ a: SumFieldOne(SumFieldValue(7)) }
	if c.a is int {
		actual = 'int'
	} else {
		actual = 'not int'
	}
	assert actual == 'int'
}

fn test_sumtype_alias_field_is_checks_chained_aliases() {
	b := SumFieldBox2{ a: SumFieldTwo(SumFieldOne(SumFieldValue(5))) }
	mut actual := 0
	if b.a is int {
		actual = 5
	} else {
		actual = -5
	}
	assert actual == 5

	c := SumFieldBox2{ a: SumFieldTwo(SumFieldOne(SumFieldValue('text'))) }
	if c.a is int {
		actual = 10
	} else {
		actual = -10
	}
	assert actual == -10

	d := SumFieldBox3{ a: SumFieldThree(SumFieldTwo(SumFieldOne(SumFieldValue(3)))) }
	mut matched_3 := false
	if d.a is int {
		matched_3 = true
	}
	assert matched_3 == true
	mut matched_4 := false
	if d.a !is string {
		matched_4 = true
	}
	assert matched_4 == true
}

fn test_sumtype_alias_field_is_preserves_plain_sum_and_alias_locals() {
	plain := SumFieldPlain{ a: SumFieldValue('text') }
	mut matched_5 := false
	if plain.a is string {
		matched_5 = true
	}
	assert matched_5 == true
	mut matched_6 := false
	if plain.a !is int {
		matched_6 = true
	}
	assert matched_6 == true
	local_one := SumFieldOne(SumFieldValue('text'))
	local_two := SumFieldTwo(SumFieldOne(SumFieldValue(7)))
	mut matched_7 := false
	if local_one !is int {
		matched_7 = true
	}
	assert matched_7 == true
	mut matched_8 := false
	if local_two is int {
		matched_8 = true
	}
	assert matched_8 == true
	mut matched_9 := false
	if local_two !is string {
		matched_9 = true
	}
	assert matched_9 == true
	local_three := SumFieldThree(SumFieldTwo(SumFieldOne(SumFieldValue('text'))))
	assert local_three !is int
	other := SumFieldUnrelated(true)
	mut matched_10 := false
	if other is bool {
		matched_10 = true
	}
	assert matched_10 == true
}

fn test_sumtype_alias_field_is_mutable_storage() {
	mut b := SumFieldBox{ a: SumFieldOne(SumFieldValue('text')) }
	mut matched_11 := false
	if b.a !is int {
		matched_11 = true
	}
	assert matched_11 == true
	b.a = SumFieldOne(SumFieldValue(12))
	mut matched_12 := false
	if b.a is int {
		matched_12 = true
	}
	assert matched_12 == true
	b.a = SumFieldOne(SumFieldValue('again'))
	mut matched_13 := false
	if b.a !is int {
		matched_13 = true
	}
	assert matched_13 == true
}

fn test_sumtype_alias_field_is_indexed_and_pointer_receivers() {
	bs := [SumFieldBox{ a: SumFieldOne(SumFieldValue('text')) }]
	mut calls := [0]
	mut actual := 0
	if bs[sum_field_next_index(mut calls)].a is int {
		actual = 1
	} else {
		actual = 2
	}
	assert actual == 2
	assert calls[0] == 1

	b := &SumFieldBox{ a: SumFieldOne(SumFieldValue(9)) }
	mut matched_14 := false
	if b.a is int {
		matched_14 = true
	}
	assert matched_14 == true
	assert b.sum_field_is_int()
	b_string := &SumFieldBox{ a: SumFieldOne(SumFieldValue('text')) }
	assert !b_string.sum_field_is_int()
}

fn test_sumtype_alias_field_is_generic_field() {
	b := SumFieldGeneric[SumFieldTwo]{ a: SumFieldTwo(SumFieldOne(SumFieldValue('text'))) }
	mut matched_15 := false
	if b.a !is int {
		matched_15 = true
	}
	assert matched_15 == true
	c := SumFieldGeneric[SumFieldTwo]{ a: SumFieldTwo(SumFieldOne(SumFieldValue(9))) }
	mut matched_16 := false
	if c.a is int {
		matched_16 = true
	}
	assert matched_16 == true
}

fn sum_field_match_two(b SumFieldBox2) string {
	return match b.a {
		int { 'int' }
		string { 'string' }
	}
}

fn sum_field_match_three(b SumFieldBox3) string {
	return match b.a {
		int { 'int' }
		string { 'string' }
	}
}

fn test_sumtype_alias_field_match_chained_aliases() {
	assert sum_field_match_two(SumFieldBox2{ a: SumFieldTwo(SumFieldOne(SumFieldValue('text'))) }) == 'string'
	assert sum_field_match_two(SumFieldBox2{ a: SumFieldTwo(SumFieldOne(SumFieldValue(7))) }) == 'int'
	assert sum_field_match_three(SumFieldBox3{
		a: SumFieldThree(SumFieldTwo(SumFieldOne(SumFieldValue('text'))))
	}) == 'string'
	assert sum_field_match_three(SumFieldBox3{
		a: SumFieldThree(SumFieldTwo(SumFieldOne(SumFieldValue(7))))
	}) == 'int'
}
