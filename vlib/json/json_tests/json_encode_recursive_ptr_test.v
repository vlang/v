import json

struct PostTag {
	id         string
	parent     ?&PostTag
	visibility string
	created_at string @[json: 'createdAt']
	metadata   string @[raw]
}

fn test_main() {
	new_post_tag := &PostTag{}
	assert json.encode(new_post_tag) == '{"id":"","visibility":"","createdAt":"","metadata":""}'

	new_post_tag2 := PostTag{
		parent: new_post_tag
	}
	assert json.encode(new_post_tag2) == '{"id":"","parent":{"id":"","visibility":"","createdAt":"","metadata":""},"visibility":"","createdAt":"","metadata":""}'
}
