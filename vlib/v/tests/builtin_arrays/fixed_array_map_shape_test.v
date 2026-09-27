struct Source {
	value int
}

struct Target {
	value int
}

struct Event {
	values [2]Target
}

fn test_fixed_array_map_function_value() {
	source := [Source{1}, Source{2}]!
	event := Event{ values: source.map(fn (v Source) Target { return Target{v.value} }) }
	assert event.values[0].value == 1
	assert event.values[1].value == 2
}

type Sources = [2]Source

fn mapped_sources(source Sources) [2]string {
	return source.map(it.value.str()).map('value:' + it)
}

fn test_fixed_array_map_expression_alias_and_chain() {
	source := Sources([Source{3}, Source{4}]!)
	assert mapped_sources(source) == ['value:3', 'value:4']!
	assert source.map(it.value).filter(it > 3) == [4]
}

struct SourceFactory {
mut:
	calls int
}

fn (mut f SourceFactory) source() [2]Source {
	f.calls++
	return [Source{5}, Source{6}]!
}

fn test_fixed_array_map_evaluates_source_once() {
	mut factory := SourceFactory{}
	offset := 10
	values := factory.source().map(fn [offset] (value Source) int {
		return value.value + offset
	})
	assert values == [15, 16]!
	assert factory.calls == 1
}
