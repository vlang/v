module eventbus

/*
NOTE: All these non-generic methods are temporary until
V has a properly functioning generic system
*/

pub struct Params {
	mut:
	params []Param
}

struct Param{
	typ string
	name string
	value voidptr
}

pub fn (p Params) get_string(name string) string {
	param, is_type := p.get_param(name, "string")
	return if is_type {string(byteptr(param.value))}else{""}
}

pub fn (p Params) get_int(name string) int {
	param, is_type := p.get_param(name, "num")
	return if is_type {int(param.value)}else{0}
}

pub fn (p Params) get_bool(name string) bool {
	param, is_type := p.get_param(name, "bool")
	return if is_type {int(param.value) == 1}else{false}
}

pub fn (p Params) get_array<T>(name string, def T) []T {
	param, is_type := p.get_param(name, "array")
	if is_type {
		val := param.value
		return unmarshall_array(def, val)
	} else {
		return []
	}
}

pub fn (p Params) get_map<T>(name string, def T) map[string]T {
	param, is_type := p.get_param(name, "map")
	if is_type {
		val := param.value
		return unmarshall_map(def, val)
	} else {
		return map[string]T
	}
}

pub fn (p Params) get_raw(name string) voidptr {
	param, _ := p.get_param(name, "")
	return param.value
}

pub fn (p mut Params) put_map(name string, value voidptr) {
	p.put_custom(name, "map", value)
}

pub fn (p mut Params) put_array(name string, arr voidptr) {
	p.put_custom(name, "array", arr)
}

pub fn (p mut Params) put_int(name string, num int) {
	p.put_custom(name, "num", num)
}

pub fn (p mut Params) put_string(name string, s string) {
	p.put_custom(name, "string", s.str)
}

pub fn (p mut Params) put_bool(name string, val bool) {
	p.put_custom(name, "bool", if val { 1 } else { 0 })
}

pub fn (p mut Params) put_custom(name, typ string, data voidptr) {
	p.params << Param {typ, name, data}
}

//HELPERS
fn (p Params) get_param(name string, typ string) (Param, bool) {
	for param in p.params {
		if param.name == name {
			return param, param.typ == typ
		}
	}
	return Param{value: voidptr(0)}, false
}

fn unmarshall_array<T> (s T, m voidptr) array_T {
	return *(*array_T(m))
}

fn unmarshall_map<T> (s T, m voidptr) map_T {
	return *(*map_T(m))
}