// An option or a result stored as one whose payload is stored differently (a `?[2]int` or a
// `?[]Item` as a `?[]int` or `?[]Named`, a `?Item` as a `?Named`, a `?u8` as a `?u64`) has
// its payload converted when it is present, as a plain value would be, and keeps `none` or
// its error otherwise.
interface Named {
	name() string
}

struct Item {
	n string
}

fn (i Item) name() string {
	return i.n
}

type Sum = Item | int
type Inner = bool | int
type Outer = Inner | string
type Ints = []int

fn fixed(ok bool) ?[2]int {
	if ok {
		return [1, 2]!
	}
	return none
}

fn fixed_result(ok bool) ![2]int {
	if ok {
		return [1, 2]!
	}
	return error('no pair')
}

fn items(ok bool) ?[]Item {
	if ok {
		return [Item{'a'}, Item{'b'}]
	}
	return none
}

fn fixed_items(ok bool) ?[2]Item {
	if ok {
		return [Item{'c'}, Item{'d'}]!
	}
	return none
}

fn item(ok bool) ?Item {
	if ok {
		return Item{'e'}
	}
	return none
}

fn small(ok bool) ?u8 {
	if ok {
		return 200
	}
	return none
}

fn whole(ok bool) ?int {
	if ok {
		return 3
	}
	return none
}

fn inner(ok bool) ?Inner {
	if ok {
		return Inner(7)
	}
	return none
}

fn names(a ?[]Named) string {
	if v := a {
		return v.map(it.name()).join(',')
	}
	return 'none'
}

fn name(a ?Named) string {
	if v := a {
		return v.name()
	}
	return 'none'
}

fn ints(a ?[]int) string {
	if v := a {
		return '${v} ${v.len}'
	}
	return 'none'
}

fn return_fixed_ident(ok bool) ?[]int {
	x := fixed(ok)
	return x
}

fn return_fixed_call(ok bool) ?[]int {
	return fixed(ok)
}

fn return_fixed_alias(ok bool) ?Ints {
	x := fixed(ok)
	return x
}

fn return_fixed_result(ok bool) ![]int {
	return fixed_result(ok)
}

fn return_items_ident(ok bool) ?[]Named {
	x := items(ok)
	return x
}

fn return_items_call(ok bool) ?[]Named {
	return items(ok)
}

fn return_fixed_items(ok bool) ?[]Named {
	return fixed_items(ok)
}

fn return_item(ok bool) ?Named {
	x := item(ok)
	return x
}

fn return_small(ok bool) ?u64 {
	x := small(ok)
	return x
}

fn return_whole(ok bool) ?f64 {
	x := whole(ok)
	return x
}

fn return_sum(ok bool) ?Sum {
	x := whole(ok)
	return x
}

fn return_inner(ok bool) ?Outer {
	x := inner(ok)
	return x
}

struct Holder {
	pair ?[2]int
}

fn (h Holder) pair() ?[]int {
	return h.pair
}

fn return_branch(ok bool) ?[]int {
	x := fixed(true)
	y := fixed(false)
	return if ok { x } else { y }
}

fn return_match(n int) ?[]Named {
	x := items(true)
	return match n {
		0 { x }
		else { items(false) }
	}
}

fn forward[T](x ?T) ?T {
	return x
}

fn return_generic() ?[]int {
	return forward[[2]int]([5, 6]!)
}

fn convert_generic[T](x ?T) ?[]int {
	return x
}

fn fixed_slot(ok bool) (int, ?[2]int) {
	return 1, fixed(ok)
}

fn forwarded_slot(ok bool) (int, ?[]int) {
	return fixed_slot(ok)
}

fn whole_slot() (int, ?int) {
	return 1, 3
}

fn forwarded_real_slot() (int, ?f64) {
	return whole_slot()
}

fn test_returned_option_payloads_are_converted() {
	assert ints(return_fixed_ident(true)) == '[1, 2] 2'
	assert ints(return_fixed_ident(false)) == 'none'
	assert ints(return_fixed_call(true)) == '[1, 2] 2'
	assert ints(return_fixed_call(false)) == 'none'
	alias := return_fixed_alias(true) or { Ints([]int{}) }
	assert alias.len == 2 && alias[1] == 2
	assert names(return_items_ident(true)) == 'a,b'
	assert names(return_items_ident(false)) == 'none'
	assert names(return_items_call(true)) == 'a,b'
	assert names(return_fixed_items(true)) == 'c,d'
	assert names(return_fixed_items(false)) == 'none'
	assert name(return_item(true)) == 'e'
	assert name(return_item(false)) == 'none'
	assert (return_small(true) or { 0 }) == 200
	assert return_small(false) == none
	assert (return_whole(true) or { 0.0 }) == 3.0
	assert (return_sum(true) or { Sum(Item{}) }) == Sum(3)
	assert return_sum(false) == none
	assert (return_inner(true) or { Outer('') }) == Outer(Inner(7))
	assert ints(Holder{
		pair: [3, 4]!
	}.pair()) == '[3, 4] 2'
	assert ints(Holder{}.pair()) == 'none'
	assert ints(return_branch(true)) == '[1, 2] 2'
	assert ints(return_branch(false)) == 'none'
	assert names(return_match(0)) == 'a,b'
	assert names(return_match(1)) == 'none'
	assert ints(return_generic()) == '[5, 6] 2'
	assert ints(convert_generic[[2]int]([7, 8]!)) == '[7, 8] 2'
	assert ints(convert_generic[[2]int](none)) == 'none'
	n, slot := forwarded_slot(true)
	assert n == 1 && ints(slot) == '[1, 2] 2'
	_, none_slot := forwarded_slot(false)
	assert ints(none_slot) == 'none'
	_, real_slot := forwarded_real_slot()
	assert (real_slot or { 0.0 }) == 3.0
}

fn test_returned_result_payloads_are_converted() {
	pair := return_fixed_result(true) or { [0] }
	assert pair == [1, 2]
	if _ := return_fixed_result(false) {
		assert false
	} else {
		assert err.msg() == 'no pair'
	}
}

struct Target {
mut:
	pair  ?[]int
	named ?[]Named
	item  ?Named
	wide  ?u64
	real  ?f64
}

fn pass_ints(a ?[]int) string {
	return ints(a)
}

fn pass_named(a ?Named) string {
	return name(a)
}

fn pass_wide(a ?u64) u64 {
	return a or { 0 }
}

fn pass_real(a ?f64) f64 {
	return a or { -1.0 }
}

fn test_assigned_and_passed_option_payloads_are_converted() {
	x := fixed(true)
	mut pair := ?[]int(none)
	pair = x
	assert ints(pair) == '[1, 2] 2'
	pair = fixed(false)
	assert ints(pair) == 'none'
	mut named := ?Named(none)
	named = item(true)
	assert name(named) == 'e'
	mut wide := ?u64(none)
	wide = small(true)
	assert (wide or { 0 }) == 200
	assert pass_ints(x) == '[1, 2] 2'
	assert pass_ints(fixed(false)) == 'none'
	assert pass_named(item(true)) == 'e'
	assert pass_named(item(false)) == 'none'
	assert pass_wide(small(true)) == 200
	assert pass_real(whole(true)) == 3.0
	mut target := Target{
		pair:  x
		named: fixed_items(true)
		item:  item(true)
		wide:  small(true)
		real:  whole(true)
	}
	assert ints(target.pair) == '[1, 2] 2'
	assert names(target.named) == 'c,d'
	assert name(target.item) == 'e'
	assert (target.wide or { 0 }) == 200
	assert (target.real or { 0.0 }) == 3.0
	target.item = item(false)
	assert name(target.item) == 'none'
	target.named = items(true)
	assert names(target.named) == 'a,b'
	mut m := 0
	m, pair = fixed_slot(true)
	assert m == 1 && ints(pair) == '[1, 2] 2'
	mut real := ?f64(none)
	m, real = whole_slot()
	assert (real or { 0.0 }) == 3.0
}
