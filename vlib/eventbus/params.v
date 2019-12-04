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
	keys voidptr
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

pub fn get_array<T>(p Params, name string, def T) []T {
	param, _ := p.get_param(name, "")
	if param.typ.contains("[") {
		len := parse_len(param.typ, "[", "]")
		mut b := []T
		b = C.new_array_from_c_array_no_alloc(len, len, sizeof(T), param.value)
		return b
	}
	return []
}

fn C.map_set() // TODO remove hack

// TODO: make this a method after generics are fixed.
pub fn get_map<T>(p Params, name string, valueTyp T) map[string]T {
	param, _ := p.get_param(name, "")
	ret := map[string]T
	if param.typ.contains("map(") {
		len := parse_len(param.typ, "(", ")")
		mut keys := []string
		// the best way (that I could find) to convert voidptr into array without alloc
		// since we know that the voidptr we are getting is an array
		keys = C.new_array_from_c_array_no_alloc(len, len, sizeof(T), param.keys)
		for i, key in keys {
			//the most simple way to set map value without knowing the typ
			// TODO remove
			C.map_set(&ret, key, param.value + i * sizeof(T))
		}
	}
	return ret
}

pub fn (p Params) get_string_map(name string) map[string]string {
	return get_map(p, name, "")
}

pub fn (p Params) get_int_map(name string) map[string]int {
	return get_map(p, name, 0)
}

pub fn (p Params) get_bool_map(name string) map[string]bool {
	return get_map(p, name, false)
}

// TODO: make this a method after generics are fixed.
pub fn put_map<T>(p mut Params, name string, valueTyp T, value map[string]T) {
	keys := value.keys()
	mut vals := []T
	for key in keys {
		vals << value[key]
 	}
	p.params << Param {
		typ: "map($value.size)"
		name: name
		keys: keys.data
		value: vals.data
	}
}

// TODO: make this a method after generic methods are working.
pub fn put_array<T>(p mut Params, name string, arr []T) {
	p.put_custom(name, "[$arr.len]", arr.data)
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

pub fn (p mut Params) put_custom(name string, typ string, data voidptr) {
	p.params << Param {typ, name, data, voidptr(0)}
}

//HELPERS

fn parse_len(typ, s_tok, e_tok string) int {
	start_index := typ.index(s_tok) or { return 0 }
	end_index := typ.index(e_tok) or { return 0 }
	len := typ[start_index+1..end_index].int()
	//t := typ.substr(typ.index(e_tok) + 1, typ.len)
	return len
}

fn (p Params) get_param(name string, typ string) (Param, bool) {
	for param in p.params {
		if param.name == name {
			return param, param.typ == typ
		}
	}
	return Param{value: voidptr(0), keys: voidptr(0)}, false
}
