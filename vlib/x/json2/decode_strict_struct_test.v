import x.json2 as json

struct StructType[T] {
mut:
	val T
}

struct StructTypeAndOptionType[T] {
mut:
	val        T
	option_val ?T
}

fn test_duplicate() {
	json.strict_decode[StructTypeAndOptionType[string]]('{"val": "","val": ""}') or {
		assert err.msg() == '[x.json2] `val` is duplicate'
		assert err.superfluous_fields() == []
		return
	}
	assert false
}

fn test_superfluous() {
	json.strict_decode[StructTypeAndOptionType[string]]('{"val": "","val2": ""}') or {
		assert err.msg() == '[x.json2] useless data detected. Try prune superfluous fields showed in err.superfluous_fields()'
		assert err.superfluous_fields() == ['val2']
		return
	}
	assert false
}

fn test_nested_superfluous() {
	json.strict_decode[StructType[StructTypeAndOptionType[string]]]('{"val": {"val": "","val2": ""}}') or {
		assert err.msg() == '[x.json2] useless data detected. Try prune superfluous fields showed in err.superfluous_fields()'
		assert err.superfluous_fields() == ['val.val2']
		return
	}
	assert false
}
