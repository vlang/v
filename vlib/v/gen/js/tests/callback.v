struct FooParams {
	name   string
	update fn (name string) @[required]
}

fn foo(params FooParams) {
	params.update(params.name)
}

fn main() {
	// anonymous function callback
	foo(
		name:   'item 1'
		update: fn (name string) {
			println('update ${name}')
		}
	)

	// lambda function callback
	update := fn (name string) {
		println('update ${name}')
	}
	foo(name: 'item 2', update: update)

	// anonymous function field
	item_3 := FooParams{
		update: fn (name string) {
			println('update ${name}')
		}
	}
	item_3.update('item 3')

	// lambda function field
	item_4 := FooParams{
		update: update
	}
	item_4.update('item 4')
}
