// vtest vflags: -w
import json2

struct PostTag {
	id         string
	parent     ?&PostTag
	visibility string
	created_at string @[json: 'createdAt']
	metadata   string @[raw]
}

fn test_main() {
	new_post_tag := &PostTag{}
	assert json2.encode(new_post_tag, escape_unicode: true) == '{"id":"","visibility":"","createdAt":"","metadata":""}'

	new_post_tag2 := PostTag{
		parent: new_post_tag
	}
	assert json2.encode(new_post_tag2, escape_unicode: true) == '{"id":"","parent":{"id":"","visibility":"","createdAt":"","metadata":""},"visibility":"","createdAt":"","metadata":""}'
}
