import json

pub struct NotificationMessage<T> {
pub:
	method string
	params T
}

struct Abc {}

pub fn (x &Abc) notification_at<T>() ?NotificationMessage<T> {
	return json.decode(NotificationMessage<T>, '{}')
}

pub fn (x &Abc) generic_method<T>(method_name string) ?NotificationMessage<T> {
	return x.notification_at<T>()
}

struct Res {}

pub fn (mut x Abc) diagnostics() ?Res {
	got := x.generic_method<Res>('xyz')?
	return got.params
}

fn test_generic_method_returning_optional() ? {
	mut a := Abc{}
	a.diagnostics()?
	assert true
}
