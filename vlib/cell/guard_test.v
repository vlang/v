module cell

struct GuardTestValue {
mut:
	text string
}

fn test_refcell_shared_guards_hold_the_borrow_through_a_move() {
	$if ownership ? {
		payload := new_ref_cell(41)
		{
			first := payload.borrow()
			moved := first
			{
				second := payload.try_borrow_ref() or { panic('expected another shared guard') }
				assert *second.get() == 41
				assert payload.try_borrow_mut() == none
			}
			assert *moved.get() == 41
			assert payload.try_borrow_mut() == none
		}
		write := payload.try_borrow_mut() or { panic('expected the shared guards to be dropped') }
		assert *write.get() == 41
	}
}

fn test_refcell_exclusive_guard_updates_in_place_and_releases_the_borrow() {
	$if ownership ? {
		payload := new_ref_cell(GuardTestValue{ text: 'before'.clone() })
		{
			mut guard := payload.borrow_mut()
			assert payload.try_borrow_ref() == none
			assert payload.try_borrow_mut() == none
			{
				mut value := guard.get_mut()
				value.text = 'after'.clone()
			}
			assert guard.get().text == 'after'
		}
		guard := payload.borrow()
		assert guard.get().text == 'after'
	}
}
