module taglib

pub struct Box {
pub mut:
	tags []string
}

// add_tag appends a tag to the box stored under key.
pub fn (mut boxes map[string]Box) add_tag(key string, tag string) {
	boxes[key].tags << tag
}

// tags_for returns the tags stored under key.
pub fn (boxes map[string]Box) tags_for(key string) []string {
	return boxes[key].tags
}
