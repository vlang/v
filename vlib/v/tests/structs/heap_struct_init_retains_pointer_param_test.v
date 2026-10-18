struct RetainedTiming {
	start int
}

@[heap]
struct RetainingExtensions {
	timing &RetainedTiming = unsafe { nil }
}

fn RetainingExtensions.new(timing &RetainedTiming) &RetainingExtensions {
	return &RetainingExtensions{
		timing: timing
	}
}

fn retaining_value(timing &RetainedTiming) RetainingExtensions {
	return RetainingExtensions{
		timing: timing
	}
}

fn test_heap_struct_init_retains_a_pointer_parameter() {
	timing := &RetainedTiming{
		start: 7
	}
	extensions := RetainingExtensions.new(timing)
	assert voidptr(extensions.timing) == voidptr(timing)
	assert retaining_value(timing).timing.start == 7
}
