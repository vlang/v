// vtest build: !msvc // error: cannot support compound statement expression ({expr; expr; expr;})
type Numbers = int | string

struct Values {
	value Numbers
}

struct Wrapper {
	element []Values
mut:
	index int
}

fn (mut instance Wrapper) get() Values {
	instance.index++
	return instance.element[0]
}

fn test_sumtype_as_cast() {
	mut wrapper := Wrapper{
		element: [
			Values{
				value: 1
			},
			Values{
				value: '2'
			},
		]
	}
	println(wrapper.get().value as int)
	assert wrapper.index == 1
}
