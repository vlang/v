module jsdom

pub struct JS.DOMString {
}

pub struct JS.DOMTokenList {
	length JS.Number
	value  JS.DOMString
}

pub struct DOMTokenList {
	list JS.DOMTokenList [noinit]
}

pub fn (x DOMTokenList) len() int {
	res := 0
	#res.val = x.list.length;

	return res
}

pub fn (x DOMTokenList) item(idx int) ?string {
	res := ''
	#let tmp = x.list.item(idx.list.val)
	#if (tmp === undefined) return new Option({state: new byte(2),err: none__});
	#res.val = tmp

	return res
}

pub fn (x DOMTokenList) contains(token string) bool {
	res := false
	#res.val = x.list.contains(token.str);

	return res
}

pub fn (x DOMTokenList) add(tokens ...string) {
	for token in tokens {
		#x.list.add(token.str);

		_ := token
	}
}

pub fn (x DOMTokenList) remove(tokens ...string) {
	for token in tokens {
		#x.list.remove(token.str);

		_ := token
	}
}

pub fn (x DOMTokenList) replace(old_token string, new_token string) bool {
	is_replaced := false
	#is_replaced.val = x.list.replace(old_token.str,new_token.str);

	return is_replaced
}

// supports returns true if the given `token` is in the associated attibute's supported tokens.
pub fn (x DOMTokenList) supports(token string) bool {
	supports := false
	#supports.val = x.list.supports(token.str)

	return supports
}

// toggle removes a given token from the list and returns `false`. If token does not exist
// it is added and function returns `true`.
pub fn (x DOMTokenList) toggle(token string, force bool) bool {
	res := false
	#res.val = x.list.toggle(token.str, force.val);

	return res
}

// entries returns array of all tokens in this token list
pub fn (x DOMTokenList) values() []string {
	mut res := []string{}
	#for (let [_,value] of x.list.entries()) array_push(res, new string(value));

	return res
}

pub fn (x DOMTokenList) str() string {
	mut fmt := 'DOMTokenList['
	values := x.values()
	for i, val in values {
		fmt += '"' + val + '"'
		if i != values.len - 1 {
			fmt += ','
		}
	}
	fmt += ']'
	return fmt
}
