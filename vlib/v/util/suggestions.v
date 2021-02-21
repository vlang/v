module util

import strings

struct Possibility {
	value  string
	svalue string
mut:
	similarity f32
}

struct Suggestion {
mut:
	known   []Possibility
	wanted  string
	swanted string
}

pub fn new_suggestion(wanted string, possibilities []string) Suggestion {
	mut s := Suggestion{
		wanted: wanted
		swanted: short_module_name(wanted)
	}
	s.add_many(possibilities)
	s.sort()
	return s
}

pub fn (mut s Suggestion) add(val string) {
	if val in [s.wanted, s.swanted] {
		return
	}
	sval := short_module_name(val)
	if sval in [s.wanted, s.swanted] {
		return
	}
	// round to 3 decimal places to avoid float comparison issues
	similarity := f32(int(strings.dice_coefficient(s.swanted, sval) * 1000)) / 1000
	s.known << Possibility{
		value: val
		svalue: sval
		similarity: similarity
	}
}

pub fn (mut s Suggestion) add_many(many []string) {
	for x in many {
		s.add(x)
	}
}

pub fn (mut s Suggestion) sort() {
	s.known.sort(a.similarity < b.similarity)
}

pub fn (s Suggestion) say(msg string) string {
	mut res := msg
	mut found := false
	if s.known.len > 0 {
		top_posibility := s.known.last()
		if top_posibility.similarity > 0.5 {
			val := top_posibility.value
			if !val.starts_with('[]') {
				res += '.\nDid you mean `$val`?'
				found = true
			}
		}
	}
	if !found {
		if s.known.len > 0 {
			mut values := s.known.map('`$it.svalue`')
			values.sort()
			if values.len == 1 {
				res += '.\n1 possibility: ${values[0]}.'
			} else if values.len < 25 {
				// it is hard to read/use too many suggestions
				res += '.\n$values.len possibilities: ' + values.join(', ') + '.'
			}
		}
	}
	return res
}

pub fn short_module_name(name string) string {
	if !name.contains('.') {
		return name
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	mname := vals[vals.len - 2]
	symname := vals[vals.len - 1]
	return '${mname}.$symname'
}
