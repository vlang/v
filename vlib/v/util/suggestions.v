module util

import term
import strings

// Possibility is a simple pair of a string, with a similarity coefficient
// determined by the editing distance to a wanted value.
struct Possibility {
	value  string
	svalue string
mut:
	similarity f32 // Note: 0.0 for *equal* strings.
}

// Suggestion is set of known possibilities and a wanted string.
// It has helper methods for making educated guesses based on the possibilities,
// on which of them match best the wanted string.
struct Suggestion {
mut:
	known   []Possibility
	wanted  string
	swanted string
}

// new_suggestion creates a new Suggestion, given a wanted value and a list of possibilities.
pub fn new_suggestion(wanted string, possibilities []string) Suggestion {
	mut s := Suggestion{
		wanted: wanted
		swanted: short_module_name(wanted)
	}
	s.add_many(possibilities)
	s.sort()
	return s
}

// add adds the `val` to the list of known possibilities of the suggestion.
// It calculates the similarity metric towards the wanted value.
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

// add adds all of the `many` to the list of known possibilities of the suggestion
pub fn (mut s Suggestion) add_many(many []string) {
	for x in many {
		s.add(x)
	}
}

// sort sorts the list of known possibilities, based on their similarity metric.
// Equal strings will be first, followed by less similar ones, very distinct ones will be last.
pub fn (mut s Suggestion) sort() {
	s.known.sort(a.similarity < b.similarity)
}

// say produces a final suggestion message, based on the preset `wanted` and
// `possibilities` fields, accumulated in the Suggestion.
pub fn (s Suggestion) say(msg string) string {
	mut res := msg
	mut found := false
	if s.known.len > 0 {
		top_posibility := s.known.last()
		if top_posibility.similarity > 0.5 {
			val := top_posibility.value
			if !val.starts_with('[]') {
				res += '.\nDid you mean `${highlight_suggestion(val)}`?'
				found = true
			}
		}
	}
	if !found {
		if s.known.len > 0 {
			mut values := s.known.map('`${highlight_suggestion(it.svalue)}`')
			values.sort()
			if values.len == 1 {
				res += '.\n1 possibility: ${values[0]}.'
			} else if values.len < 25 {
				// it is hard to read/use too many suggestions
				res += '.\n${values.len} possibilities: ' + values.join(', ') + '.'
			}
		}
	}
	return res
}

// short_module_name returns a shortened version of the fully qualified `name`,
// i.e. `xyz.def.abc.symname` -> `abc.symname`
pub fn short_module_name(name string) string {
	if !name.contains('.') {
		return name
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	mname := vals[vals.len - 2]
	symname := vals.last()
	return '${mname}.${symname}'
}

// highlight_suggestion returns a colorfull/highlighted version of `message`,
// but only if the standart error output allows for color messages, otherwise
// the plain message will be returned.
pub fn highlight_suggestion(message string) string {
	return term.ecolorize(term.bright_blue, message)
}
