module foo

pub fn bar(params struct { name string }) string {
	return params.name
}
