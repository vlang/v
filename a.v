module main

import x.json2 as json

struct Foo{
	confirm_url string [json: 'confirmUrl'; required]
}

fn main(){
	mut j := '{"confirm_url":"http://foo.com"}'
	mut f := json.decode[Foo]( j) or { panic(err) }
	dump(f)
	
	j = '{"confirmUrl":"http://foo.com"}'
	f = json.decode[Foo]( j) or { panic(err) }
	dump(f)
}