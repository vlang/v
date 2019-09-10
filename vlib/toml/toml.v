// (c) 2019 keito940 All Rights Reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module toml

import ebnf

#include "toml.tab.h"

fn compile(){
	// flex compile for C
	system('flex -d toml.l')
	// bison compile for C
	system('bison toml.y')
}

struct KeyVal{
	key string
	val string
}

struct Array{
	key string
	kind int
	value_type int
}

struct TimeStamp{
	year   		int
	month  		int
	date   		int
	hour   		int
	minute 		int
	second 		int
	millsecond 	int	
}