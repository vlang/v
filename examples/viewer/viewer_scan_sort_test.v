module main

// Regression test for the sort-mode reset on a replacement scan.
//
// Reproduction of the reported bug: sort a list by name, drop a new set of
// paths (which aborts the old scan and rebuilds the list in "as loaded"
// order), then press `1` again. Before the fix, reset() kept the stale
// .name mode, so App.set_sort(.name) early-returned and the freshly dropped
// list stayed unsorted. reset() must therefore restore .loaded.
fn test_reset_restores_loaded_sort_mode() ! {
	mut il := Item_list{}
	il.lst << Item{
		name:     'b.jpg'
		i_type:   .jpg
		drawable: true
	}
	il.lst << Item{
		name:     'a.jpg'
		i_type:   .jpg
		drawable: true
	}
	il.n_item = 2
	il.loaded = true
	// Simulate a resort to name order (App.set_sort(.name) -> resort).
	il.resort(.name)
	assert il.sort_mode == .name
	// A drag & drop replaces the contents: scan_abort() calls reset().
	il.scan_abort()
	assert il.sort_mode == .loaded
	assert il.lst.len == 0
	assert il.n_item == 0
}
