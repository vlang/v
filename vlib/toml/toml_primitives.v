// Sorry! This Libary Compliant is v0.4.0.
// (c) 2019 keito940

module toml

#flag -I @VROOT/thirdparty/tomlc99
#flag @VROOT/thirdparty/tomlc99/toml.o
#include "toml.h"

struct &C.toml{
	intger		&C.toml.toml_rtoi
	boolean 	&C.toml.toml_rtob
	double		&C.toml.toml_rtod
	dbl_str 	&C.toml.toml_rtod_ex
	time_stamp	&C.toml.toml_rtots
	table		&C.toml.toml_table_t
	array		&C.toml.toml_array_t
}

fn tomldecode_int (root &C.toml) int{
	err := root.intger(root,rtn)
	if err = -1 {
		return 0
	}

	return rtn
}

fn tomldecode_bool (root &C.toml) bool{
	err := root.boolean(root,rtn)
	if err = -1 {
		return false
	}

	if rtn = 1 {
		return true
	}
	else {
		return false
	}
}

fn tomldecode_i8 (root &C.toml) i8{
	err := root.intger(root,rtn)
	if err = -1 {
		return 0
	}
	return rtn
}