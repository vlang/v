module main

import json2
import hiddeniface as _
import otherrec as orec
import plainrec

// https://github.com/vlang/v/issues/28952
// `plainrec.MyData` and `otherrec.MyData` are structs, while the private
// `hiddeniface.MyData` is an interface. Arrays, options and pointers of the
// structs must not be taken for that interface just because of the short name.
struct Records {
mut:
	list   []plainrec.MyData
	one    plainrec.MyData
	by_key map[string]plainrec.MyData
	maybe  ?plainrec.MyData
	fixed  [1]plainrec.MyData
	nested [][]plainrec.MyData
	others []orec.MyData
}

struct Refs {
	ptr &plainrec.MyData = unsafe { nil }
}

fn is_interface[T]() bool {
	$if T is $interface {
		return true
	}
	return false
}

fn sample() Records {
	d := plainrec.MyData{'hello'}
	return Records{
		list:   [d]
		one:    d
		by_key: {
			'k': d
		}
		maybe:  d
		fixed:  [d]!
		nested: [[d]]
		others: [orec.MyData{
			num: 7
		}]
	}
}

const sample_json = '{"list":[{"id":"hello"}],"one":{"id":"hello"},"by_key":{"k":{"id":"hello"}},"maybe":{"id":"hello"},"fixed":[{"id":"hello"}],"nested":[[{"id":"hello"}]],"others":[{"num":7}]}'

fn test_types_named_like_an_interface_of_another_module_are_not_interfaces() {
	assert !is_interface[plainrec.MyData]()
	assert !is_interface[[]plainrec.MyData]()
	assert !is_interface[&plainrec.MyData]()
	assert !is_interface[[]orec.MyData]()
	mut interface_fields := []string{}
	$for field in Records.fields {
		$if field.typ is $interface {
			interface_fields << field.name
		}
	}
	$for field in Refs.fields {
		$if field.typ is $interface {
			interface_fields << field.name
		}
	}
	assert interface_fields == []
}

fn test_json2_encode_of_types_named_like_an_interface_of_another_module() {
	assert json2.encode(sample()) == sample_json
	d := plainrec.MyData{'hi'}
	assert json2.encode(Refs{ ptr: &d }) == '{"ptr":{"id":"hi"}}'
}

fn test_json2_decode_of_types_named_like_an_interface_of_another_module() {
	decoded := json2.decode[Records](sample_json)!
	assert decoded.list == [plainrec.MyData{'hello'}]
	assert decoded.one == plainrec.MyData{'hello'}
	assert decoded.by_key == {
		'k': plainrec.MyData{'hello'}
	}
	assert decoded.maybe? == plainrec.MyData{'hello'}
	assert decoded.fixed == [plainrec.MyData{'hello'}]!
	assert decoded.nested == [[plainrec.MyData{'hello'}]]
	assert decoded.others == [orec.MyData{
		num: 7
	}]
}

fn test_str_of_types_named_like_an_interface_of_another_module() {
	r := sample()
	assert r.list.str() == "[plainrec.MyData{\n    id: 'hello'\n}]"
	assert r.others.str() == '[otherrec.MyData{\n    num: 7\n}]'
	assert '${r.maybe}' == "Option(plainrec.MyData{\n    id: 'hello'\n})"
}
