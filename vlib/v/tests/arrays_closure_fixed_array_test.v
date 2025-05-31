import arrays

fn test_main() {
	mut rules := [][2]int{}
	rules << [1, 2]!
	rules << [2, 3]!

	common_rule := [1, 2]!
	assert arrays.index_of_first(rules, fn [common_rule] (idx int, rule [2]int) bool {
		return rule == common_rule
	}) == 0

	common_rule2 := [2, 3]!
	assert arrays.index_of_first(rules, fn [common_rule2] (idx int, rule [2]int) bool {
		return rule == common_rule2
	}) == 1
}
