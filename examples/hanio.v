// hanio tower

module main

import (
	//os
)

const (
	Num = 7
)

fn main() {
	hanio(Num, 'A','B','C')
}

fn move(n int, a,b string) int {
	println('Disc $n from $a to $b\...')
	return 0
}

fn hanio(n int, a, b, c string) int {
	if n==1 {
		move(1,a,c)
	} else {
		hanio(n-1, a, c, b)
		move(n,a,c)
		hanio(n-1, b, a, c)
	}
	return 0
}
