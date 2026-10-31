// vtest vflags: -w
import json2

enum Lang {
	en = 1
}

struct Request {
	lang ?Lang // ?string, ?int are ok
}

fn test_main() {
	assert dump(json2.decode[Request]('{}')!) == Request{
		lang: ?Lang(none)
	}
	assert dump(json2.decode[Request]('{"lang": "en"}')!) == Request{
		lang: .en
	}
}
