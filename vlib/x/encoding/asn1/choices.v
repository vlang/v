// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

const max_choices_size = 255
const default_choices_size = 64

// ASN.1 CHOICE
//
// Choice is an ASN.1 Element
pub struct Choice {
mut:
	size    int = default_choices_size
	choosen Element
	choices []Element
}

// new creates a new choice element. It accepts el as a choosen element
// and choices as a source of possible choices list.
pub fn Choice.new(el Element, choices []Element) !Choice {
	c := Choice{
		choosen: el
		choices: choices
	}
	c.check()!
	return c
}

// set_size sets the maximal this choices size
pub fn (mut c Choice) set_size(size int) ! {
	if size > max_choices_size {
		return error('Your size exceed max_choices_size')
	}
	c.size = size
	c.check()!
}

// choose chooses some element for as a choosen element for the choice c.
pub fn (mut c Choice) choose(some Element) ! {
	mut within := false
	for el in c.choices {
		if el.equal(some) {
			within = true
		}
	}
	if within {
		c.choosen = some
		return
	}
	return error('element not within choices')
}

fn (c Choice) check() ! {
	if c.choices.len == 0 {
		return error('bad choices list')
	}
	if c.choices.len > c.size {
		return error('Your choices list exceed size')
	}
	// check the choosen element is within choices
	filtered := c.choices.filter(it.equal(c.choosen))
	// nothing found, so..its bad choice
	if filtered.len == 0 {
		return error('choosen element not within choices list')
	}
	if filtered.len > 1 {
		return error('multiples element within choices matching with choosen element')
	}
}

// tag returns the tag of choice element.
pub fn (c Choice) tag() Tag {
	return c.choosen.tag()
}

// payload returns the payload of choice element.
pub fn (c Choice) payload() ![]u8 {
	return c.choosen.payload()!
}
