// vtest vflags: -new-compiler

struct ConditionalStack {
mut:
	count int
}

struct ConditionalArmy {
	stacks   [2]ConditionalStack
	town     []int
	exchange []int
}

enum ConditionalSource {
	hero
	town
	exchange
}

fn conditional_stacks(count int) [2]ConditionalStack {
	return [ConditionalStack{count}, ConditionalStack{count + 1}]!
}

fn conditional_army(army &ConditionalArmy, source ConditionalSource, side int) [2]ConditionalStack {
	mut selected := match source {
		.hero { army.stacks }
		.town { conditional_stacks(if side == 0 { army.town[0] } else { army.exchange[0] }) }
		.exchange { conditional_stacks(30) }
	}
	conditional_increment(mut selected)
	return selected
}

fn conditional_increment(mut stacks [2]ConditionalStack) {
	stacks[0].count++
}

fn test_fixed_array_match_copies_fields_and_returned_arrays() {
	army := ConditionalArmy{conditional_stacks(10), [20], [40]}
	for index, source in [ConditionalSource.hero, .town, .exchange] {
		count := (index + 1) * 10
		mut selected := conditional_army(&army, source, 0)
		assert selected[0].count == count + 1
		assert selected[1].count == count + 1
		selected[0].count = 99
		assert army.stacks[0].count == 10
	}
	assert conditional_army(&army, .town, 1)[0].count == 41
}

fn test_fixed_array_if_blocks_copy_the_selected_branch_once() {
	for choice in 0 .. 3 {
		mut calls := 0
		selected := if choice == 0 {
			calls++
			conditional_stacks(10)
		} else if choice == 1 {
			calls++
			conditional_stacks(20)
		} else {
			calls++
			literal := [ConditionalStack{30}, ConditionalStack{31}]!
			literal
		}
		assert selected[0].count == (choice + 1) * 10
		assert selected[1].count == (choice + 1) * 10 + 1
		assert calls == 1
	}
}

struct ConditionalLiteralStack {
mut:
	count u16
}

fn increment_conditional_literal(mut stacks [3]ConditionalLiteralStack) {
	stacks[0].count++
}

fn test_fixed_array_match_without_a_function_return_wrapper() {
	for side in 0 .. 2 {
		mut selected := match side {
			0 {
				[ConditionalLiteralStack{u16(if side == 0 { 10 } else { 20 })},
					ConditionalLiteralStack{11}, ConditionalLiteralStack{12}]!
			}
			else {
				local := [ConditionalLiteralStack{20}, ConditionalLiteralStack{21},
					ConditionalLiteralStack{22}]!
				local
			}
		}
		increment_conditional_literal(mut selected)
		assert selected[0].count == (side + 1) * 10 + 1
		assert selected[2].count == (side + 1) * 10 + 2
	}
}
