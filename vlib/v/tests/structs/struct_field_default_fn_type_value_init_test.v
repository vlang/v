struct Flip {
	name    string  = 'NULL'
	execute fn () ! = unsafe { nil }
}

fn (flip Flip) exec() ! {
	if isnil(flip.execute) {
		return
	}

	println('Executing ${flip.name}')
	flip.execute()!
}

fn test_struct_field_default_fn_type_value() {
	fl := Flip{
		name:    'a function'
		execute: fn () ! {
			println('Hello, World!')
		}
	}

	fl.exec()!
	assert true
}
