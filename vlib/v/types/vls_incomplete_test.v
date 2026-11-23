module types

// Code being written is completed where it does not parse yet, as V1 parsed it
// in its VLS mode, and only there: each line keeps what it had before what the
// completion writes, so that the questions about it keep their positions.
fn test_a_dot_without_a_name_gets_one() {
	source := 'module main

import os
import strings

fn main() {
	p := Point{}
	println(p.)
	q := os.
	k := 1
	p.
	mut s := 0
	os.
	strings.new_builder(0)
	x := p.
}
'
	assert vls_complete_source(source) == 'module main

import os
import strings

fn main() {
	p := Point{}
	println(p.vlsmember)
	q := os.vlsmember
	k := 1
	p.vlsmember
	mut s := 0
	os.vlsmember
	strings.new_builder(0)
	x := p.vlsmember
}
'
}

fn test_a_call_left_open_is_closed_before_the_next_statement() {
	source := 'module main

fn main() {
	x := s.all_before_last(
	y := s.all_after(os.
	z := 1
	f(a,
}
'
	assert vls_complete_source(source) == 'module main

fn main() {
	x := s.all_before_last()
	y := s.all_after(os.vlsmember)
	z := 1
	f(a,)
}
'
}

fn test_code_that_parses_is_left_as_it_is() {
	source := 'module main

import math as m

fn main() {
	a := m.
		pi
	b := [1
		2]
	c := f(1,
		2)
	d := strings.
		repeat(`x`, 2)
	e := xs.filter(it > 0).
		map(it * 2)
	g := h(
		mut x
		y: 1
	)
	println(if v := m[k] { v } else { 0 })
	asm amd64 {
		mov rax, [rbx
	}
}
'
	assert vls_complete_source(source) == source
}
