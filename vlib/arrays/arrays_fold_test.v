import arrays

type MyInt = int

interface FoldCyclable {
	id() int
}

struct FoldWheel {
	v int
}

fn (w FoldWheel) id() int {
	return w.v
}

fn test_main() {
	assert arrays.fold[int, []int]([1, 2, 3, 4], []int{}, fn (r []int, t int) []int {
		return arrays.merge(r, [t])
	}) == [1, 2, 3, 4]
	assert arrays.fold[string, []string](['a', 'b', 'c', 'd'], []string{}, fn (r []string, t string) []string {
		return arrays.merge(r, [t])
	}) == ['a', 'b', 'c', 'd']
	assert arrays.fold[bool, []bool]([true, false], []bool{}, fn (r []bool, t bool) []bool {
		return arrays.merge(r, [t])
	}) == [false, true]
	assert arrays.fold[MyInt, []MyInt]([MyInt(0), 1], []MyInt{}, fn (r []MyInt, t MyInt) []MyInt {
		return arrays.merge(r, [t])
	}) == [MyInt(0), 1]
	assert arrays.fold([1, 2, 3, 4], [5, 6, 7], fn (r []int, t int) []int {
		return r.map(it * t)
	}) == [120, 144, 168]
}

fn test_fold_with_interface_accumulator() {
	items := [FoldWheel{1}, FoldWheel{2}]
	result := arrays.fold[FoldWheel, FoldCyclable](items, FoldCyclable(FoldWheel{}), fn (_ FoldCyclable, elem FoldWheel) FoldCyclable {
		return elem
	})
	assert result.id() == 2
}
