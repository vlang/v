import json

enum Lang {
	en = 1
}

struct Request {
	lang ?Lang // ?string, ?int are ok
}

fn test_main() {
	assert dump(json.decode(Request, '{}')!) == Request{
		lang: ?Lang(none)
	}
	assert dump(json.decode(Request, '{"lang": "en"}')!) == Request{
		lang: .en
	}
}
