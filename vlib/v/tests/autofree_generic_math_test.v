// vtest vflags: -autofree

// Test for memory leak fix when using generics with math.min/max and autofree
module main

import math
import time

fn test_math_min_with_time_and_int() {
	// Test with time.Time
	t1 := time.Time{
		year: 2022
		day:  1
	}
	t2 := time.Time{
		year: 2021
		day:  1
	}
	t3 := math.min(t1, t2)
	assert t3.year == 2021

	// Test with int
	a := 10
	b := 5
	c := math.min(a, b)
	assert c == 5
}

fn test_math_max_with_time_and_int() {
	// Test with time.Time
	t1 := time.Time{
		year: 2022
		day:  1
	}
	t2 := time.Time{
		year: 2021
		day:  1
	}
	t3 := math.max(t1, t2)
	assert t3.year == 2022

	// Test with int
	a := 10
	b := 5
	c := math.max(a, b)
	assert c == 10
}
