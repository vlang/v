// usage test: v ast path_to_v/cmd/tools/vast/test/demo.v
// will generate demo.json

// comment for module
module main

// import module
import os
import math
import time { Time, now }

// const decl
const a = 1
const b = 3
const c = 'c'

// struct decl
struct Point {
	x int
mut:
	y int
pub:
	z int
pub mut:
	name string
}

// method of Point
pub fn (p Point) get_x() int {
	return p.x
}

// embed struct
struct MyPoint {
	Point
	title string
}

// enum type
enum Color {
	red
	green
	blue
}

// type alias
type Myint = int

// sum type
type MySumType = bool | int | string

// function type
type Myfn = fn (int) int

// interface type
interface Myinterfacer {
	add(int, int) int
	sub(int, int) int
}

// main function
fn main() {
	add(1, 3)
	println(add(1, 2))
	println('ok') // comment println
	arr := [1, 3, 5, 7]
	for a in arr {
		println(a)
		add(1, 3)
	}
	color := Color.red
	println(color)
	println(os.args)
	m := math.max(1, 3)
	println(m)
	println(now())
	t := Time{}
	println(t)
	p := Point{
		x: 1
		y: 2
		z: 3
	}
	println(p)
	my_point := MyPoint{
		// x: 1
		// y: 3
		// z: 5
	}
	println(my_point.get_x())
}

// normal function
fn add(x int, y int) int {
	return x + y
}

// function with defer stmt
fn defer_fn() {
	mut x := 1
	println('start fn')
	defer {
		println('in defer block')
		println(x)
	}
	println('end fn')
}

// generic function
fn g_fn[T](p T) T {
	return p
}

// generic struct
struct GenericStruct[T] {
	point Point
mut:
	model T
}

// generic interface
interface Gettable[T] {
	get() T
}

// generic sumtype
struct None {}

type MyOption[T] = Error | None | T
