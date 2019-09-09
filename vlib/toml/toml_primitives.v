// (c) 2019 keito940 All Rights Reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module toml

import scanner
import os
import strings

struct Toml{

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