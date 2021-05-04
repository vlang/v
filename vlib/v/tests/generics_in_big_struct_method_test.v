struct Product {
pub mut:
	id    int    = 2
	sku   string = 'ABCD'
	ean13 string = 'EFGH'

	collections    string
	ptype          string
	featured_image string
	featured_media string
	handle         string
	variant        int
}

pub fn (p Product) save() string {
	return do_something(p)
}

fn do_something<T>(p T) string {
	return 'whatever'
}

fn test_generics_in_big_struct_method() {
	mut p := Product{}
	println(p.save())
	assert p.save() == 'whatever'
}
