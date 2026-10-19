module attrs

pub enum Method {
	get
	post
}

@[attribute]
pub struct Route {
	method Method
}
