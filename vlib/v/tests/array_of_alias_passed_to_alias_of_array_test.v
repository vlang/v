// Regression test for https://github.com/vlang/v/issues/27006:
// `[]Alias` should be assignable to `Tokens` (and vice versa) when
// `type Alias = T` and `type Tokens = []T`, since both describe the
// same in-memory payload.

type Token = string
type Tokens = []string

type Idx = int
type Idxs = []int

struct Pair {
	a int
	b int
}

type APair = Pair
type APairs = []Pair

fn take_tokens(data Tokens) string {
	return data.join(',')
}

fn take_idxs(data Idxs) int {
	mut sum := 0
	for v in data {
		sum += v
	}
	return sum
}

fn take_apairs(data APairs) int {
	mut sum := 0
	for p in data {
		sum += p.a + p.b
	}
	return sum
}

fn make_tokens_return() Tokens {
	mut tokens := []Token{}
	tokens << Token('hello')
	tokens << Token('world')
	return tokens
}

fn make_idxs_return() Idxs {
	mut idxs := []Idx{}
	idxs << Idx(1)
	idxs << Idx(2)
	return idxs
}

fn test_array_of_alias_to_alias_of_array_arg() {
	mut tokens := []Token{}
	tokens << Token('hello')
	tokens << Token('world')
	assert take_tokens(tokens) == 'hello,world'
}

fn test_array_of_alias_to_alias_of_array_int_arg() {
	mut idxs := []Idx{}
	idxs << Idx(10)
	idxs << Idx(20)
	assert take_idxs(idxs) == 30
}

fn test_array_of_alias_to_alias_of_array_struct_arg() {
	mut pairs := []APair{}
	pairs << APair(Pair{1, 2})
	pairs << APair(Pair{3, 4})
	assert take_apairs(pairs) == 10
}

fn test_array_of_alias_to_alias_of_array_return() {
	t := make_tokens_return()
	assert t.len == 2
	assert t[0] == 'hello'
}

fn test_array_of_alias_to_alias_of_array_int_return() {
	idxs := make_idxs_return()
	assert idxs.len == 2
	assert idxs[0] == 1
}

fn test_array_of_alias_to_alias_of_array_assign() {
	mut tokens := []Token{}
	tokens << Token('a')
	mut t2 := Tokens([])
	t2 = tokens
	assert t2.len == 1
}

fn test_alias_of_array_to_array_of_alias_arg() {
	tokens := Tokens(['x', 'y'])
	assert take_array_of_token(tokens) == 'x|y'
}

fn take_array_of_token(data []Token) string {
	return data.join('|')
}

fn test_fixed_array_of_alias() {
	arr := [Token('a'), Token('b')]!
	got := take_fixed_string(arr)
	assert got == 'a-b'
}

fn take_fixed_string(data [2]string) string {
	return '${data[0]}-${data[1]}'
}

type StrMap = map[string]string

fn take_strmap(m StrMap) int {
	return m.len
}

fn test_map_with_alias_values() {
	m := map[string]Token{}
	assert take_strmap(m) == 0
}
