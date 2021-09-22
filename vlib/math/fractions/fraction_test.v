// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import math.fractions

// (Old) results are verified using https://www.calculatorsoup.com/calculators/math/fractions.php
// Newer ones are contrived for corner cases or prepared by hand.
fn test_4_by_8_f64_and_str() {
	f := fractions.fraction(4, 8)
	assert f.f64() == 0.5
	assert f.str() == '4/8'
}

fn test_10_by_5_f64_and_str() {
	f := fractions.fraction(10, 5)
	assert f.f64() == 2.0
	assert f.str() == '10/5'
}

fn test_9_by_3_f64_and_str() {
	f := fractions.fraction(9, 3)
	assert f.f64() == 3.0
	assert f.str() == '9/3'
}

fn test_4_by_minus_5_f64_and_str() {
	f := fractions.fraction(4, -5)
	assert f.f64() == -0.8
	assert f.str() == '-4/5'
}

fn test_minus_7_by_minus_92_str() {
	f := fractions.fraction(-7, -5)
	assert f.str() == '7/5'
}

fn test_4_by_8_plus_5_by_10() {
	f1 := fractions.fraction(4, 8)
	f2 := fractions.fraction(5, 10)
	sum := f1 + f2
	assert sum.f64() == 1.0
	assert sum.str() == '1/1'
	assert sum.equals(fractions.fraction(1, 1))
}

fn test_5_by_5_plus_8_by_8() {
	f1 := fractions.fraction(5, 5)
	f2 := fractions.fraction(8, 8)
	sum := f1 + f2
	assert sum.f64() == 2.0
	assert sum.str() == '2/1'
	assert sum.equals(fractions.fraction(2, 1))
}

fn test_9_by_3_plus_1_by_3() {
	f1 := fractions.fraction(9, 3)
	f2 := fractions.fraction(1, 3)
	sum := f1 + f2
	assert sum.str() == '10/3'
	assert sum.equals(fractions.fraction(10, 3))
}

fn test_3_by_7_plus_1_by_4() {
	f1 := fractions.fraction(3, 7)
	f2 := fractions.fraction(1, 4)
	sum := f1 + f2
	assert sum.str() == '19/28'
	assert sum.equals(fractions.fraction(19, 28))
}

fn test_36529_by_12409100000_plus_418754901_by_9174901000() {
	f1 := fractions.fraction(i64(36529), i64(12409100000))
	f2 := fractions.fraction(i64(418754901), i64(9174901000))
	sum := f1 + f2
	assert sum.str() == '5196706591957729/113852263999100000'
}

fn test_4_by_8_plus_minus_5_by_10() {
	f1 := fractions.fraction(4, 8)
	f2 := fractions.fraction(-5, 10)
	diff := f2 + f1
	assert diff.f64() == 0
	assert diff.str() == '0/1'
}

fn test_4_by_8_minus_5_by_10() {
	f1 := fractions.fraction(4, 8)
	f2 := fractions.fraction(5, 10)
	diff := f2 - f1
	assert diff.f64() == 0
	assert diff.str() == '0/1'
}

fn test_5_by_5_minus_8_by_8() {
	f1 := fractions.fraction(5, 5)
	f2 := fractions.fraction(8, 8)
	diff := f2 - f1
	assert diff.f64() == 0
	assert diff.str() == '0/1'
}

fn test_9_by_3_minus_1_by_3() {
	f1 := fractions.fraction(9, 3)
	f2 := fractions.fraction(1, 3)
	diff := f1 - f2
	assert diff.str() == '8/3'
}

fn test_3_by_7_minus_1_by_4() {
	f1 := fractions.fraction(3, 7)
	f2 := fractions.fraction(1, 4)
	diff := f1 - f2
	assert diff.str() == '5/28'
}

fn test_36529_by_12409100000_minus_418754901_by_9174901000() {
	f1 := fractions.fraction(i64(36529), i64(12409100000))
	f2 := fractions.fraction(i64(418754901), i64(9174901000))
	sum := f1 - f2
	assert sum.str() == '-5196036292040471/113852263999100000'
}

fn test_4_by_8_times_5_by_10() {
	f1 := fractions.fraction(4, 8)
	f2 := fractions.fraction(5, 10)
	product := f1 * f2
	assert product.f64() == 0.25
	assert product.str() == '1/4'
}

fn test_5_by_5_times_8_by_8() {
	f1 := fractions.fraction(5, 5)
	f2 := fractions.fraction(8, 8)
	product := f1 * f2
	assert product.f64() == 1.0
	assert product.str() == '1/1'
}

fn test_9_by_3_times_1_by_3() {
	f1 := fractions.fraction(9, 3)
	f2 := fractions.fraction(1, 3)
	product := f1 * f2
	assert product.f64() == 1.0
	assert product.str() == '1/1'
}

fn test_3_by_7_times_1_by_4() {
	f1 := fractions.fraction(3, 7)
	f2 := fractions.fraction(1, 4)
	product := f2 * f1
	assert product.f64() == (3.0 / 28.0)
	assert product.str() == '3/28'
}

fn test_4_by_8_over_5_by_10() {
	f1 := fractions.fraction(4, 8)
	f2 := fractions.fraction(5, 10)
	q := f1 / f2
	assert q.f64() == 1.0
	assert q.str() == '1/1'
}

fn test_5_by_5_over_8_by_8() {
	f1 := fractions.fraction(5, 5)
	f2 := fractions.fraction(8, 8)
	q := f1 / f2
	assert q.f64() == 1.0
	assert q.str() == '1/1'
}

fn test_9_by_3_over_1_by_3() {
	f1 := fractions.fraction(9, 3)
	f2 := fractions.fraction(1, 3)
	q := f1 / f2
	assert q.f64() == 9.0
	assert q.str() == '9/1'
}

fn test_3_by_7_over_1_by_4() {
	f1 := fractions.fraction(3, 7)
	f2 := fractions.fraction(1, 4)
	q := f1 / f2
	assert q.str() == '12/7'
}

fn test_reciprocal_4_by_8() {
	f := fractions.fraction(4, 8)
	assert f.reciprocal().str() == '8/4'
}

fn test_reciprocal_5_by_10() {
	f := fractions.fraction(5, 10)
	assert f.reciprocal().str() == '10/5'
}

fn test_reciprocal_5_by_5() {
	f := fractions.fraction(5, 5)
	assert f.reciprocal().str() == '5/5'
}

fn test_reciprocal_8_by_8() {
	f := fractions.fraction(8, 8)
	assert f.reciprocal().str() == '8/8'
}

fn test_reciprocal_9_by_3() {
	f := fractions.fraction(9, 3)
	assert f.reciprocal().str() == '3/9'
}

fn test_reciprocal_1_by_3() {
	f := fractions.fraction(1, 3)
	assert f.reciprocal().str() == '3/1'
}

fn test_reciprocal_7_by_3() {
	f := fractions.fraction(7, 3)
	assert f.reciprocal().str() == '3/7'
}

fn test_reciprocal_1_by_4() {
	f := fractions.fraction(1, 4)
	assert f.reciprocal().str() == '4/1'
}

fn test_4_by_8_equals_5_by_10() {
	f1 := fractions.fraction(4, 8)
	f2 := fractions.fraction(5, 10)
	assert f1.equals(f2)
}

fn test_1_by_2_does_not_equal_3_by_4() {
	f1 := fractions.fraction(1, 2)
	f2 := fractions.fraction(3, 4)
	assert !f1.equals(f2)
}

fn test_reduce_3_by_9() {
	f := fractions.fraction(3, 9)
	assert f.reduce().equals(fractions.fraction(1, 3))
}

fn test_1_by_3_less_than_2_by_4() {
	f1 := fractions.fraction(1, 3)
	f2 := fractions.fraction(2, 4)
	assert f1.lt(f2)
	assert f1.le(f2)
}

fn test_2_by_3_greater_than_2_by_4() {
	f1 := fractions.fraction(2, 3)
	f2 := fractions.fraction(2, 4)
	assert f1.gt(f2)
	assert f1.ge(f2)
}

fn test_5_by_7_not_less_than_2_by_4() {
	f1 := fractions.fraction(5, 7)
	f2 := fractions.fraction(2, 4)
	assert !f1.lt(f2)
	assert !f1.le(f2)
}

fn test_49_by_75_not_greater_than_2_by_3() {
	f1 := fractions.fraction(49, 75)
	f2 := fractions.fraction(2, 3)
	assert !f1.gt(f2)
	assert !f1.ge(f2)
}
