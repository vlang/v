module websocket

struct Uri {
mut:
	url         string
	hostname    string
	port        string
	resource    string
	querystring string
}

pub fn (u Uri) str() string {
	return u.url
}
