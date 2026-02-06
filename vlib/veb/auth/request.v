module auth

pub struct Request {
pub:
	client_id     string
	client_secret string
	code          string
	state         string
}
