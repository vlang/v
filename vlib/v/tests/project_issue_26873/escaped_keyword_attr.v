module main

pub enum ContentType {
	none
	block
	inline
}

pub struct Content {
pub:
	@type ContentType
	data  string
}

@[params]
struct Options {
	path string @[required]
}

fn new_content(data string) Content {
	return Content{
		@type: .block
		data:  data
	}
}

fn make_options(opts Options) Options {
	return Options{
		...opts
	}
}
