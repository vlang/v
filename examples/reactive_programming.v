module main

import os

struct ReactiveVar[T] {
mut:
	value     T
	observers []fn (old_value T, new_value T)
}

fn (mut r ReactiveVar[T]) set_value(new_value T) {
	old_value := r.value
	r.value = new_value
	for observer in r.observers {
		observer(old_value, new_value)
	}
}

fn (mut r ReactiveVar[T]) on_change(observer fn (old_value T, new_value T)) {
	r.observers << observer
}

fn main() {
	// creating a reactive variable
	mut reactive_var := ReactiveVar{
		value: '0'
	}

	// New observer (callback) for variable
	reactive_var.on_change(fn (old_value string, new_value string) {
		println('\nValue changed')
		println('Old value: ${old_value}')
		println('New value: ${new_value}')
	})

	// Changing the value
	reactive_var.set_value('42')
	reactive_var.set_value('53')
	reactive_var.set_value(os.input('Type something to send a event:'))
}
