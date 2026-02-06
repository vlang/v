// CycleMenuItem is a menu item that can be cycled through by pressing a button.
pub struct CycleMenuItem {
mut:
	label  string
	values []string
	on     CycleMenuItemEvents
}

// CycleMenuItemEvents is a set of events that can be invoked by a
// `CycleMenuItem`.
@[params]
pub struct CycleMenuItemEvents {
__global:
	click ?fn (string) bool
	cycle ?fn (string) bool
}

// CycleMenuItem.new creates a new `CycleMenuItem` with the given label, values,
// and function to invoke when the item is selected.
@[inline]
pub fn CycleMenuItem.new(label string, values []string, events CycleMenuItemEvents) CycleMenuItem {
	return CycleMenuItem{
		label:  label
		values: values
		on:     events
	}
}

fn test_struct_init_with_multi_option_fn_type() {
	item := CycleMenuItem.new('FPS', ['30', '60', '90', '120', '144', '165', 'unlimited'],
		click: fn (value string) bool {
			if value == 'unlimited' {
				return true
			} else {
				return false
			}
		}
	)
	func := item.on.click?
	ret := func('unlimited')
	println(ret)
	assert ret
}
