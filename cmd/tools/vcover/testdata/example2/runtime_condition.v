module example2

import os

pub fn runtime_condition() int {
	mut res := 0
	branch := os.getenv('CONDITION')
	if branch == '' {
		return res
	}
	if branch.contains('1') {
		res += 1
	}
	if branch.contains('2') {
		res += 2
	}
	if branch.contains('3') {
		res += 4
	}
	if branch.contains('4') {
		res += 8
	}
	if branch.contains('5') {
		res += 16
	}
	if branch.contains('6') {
		res += 32
	}
	if branch.contains('7') {
		res += 64
	}
	if branch.contains('8') {
		res += 128
	}
	return res
}
