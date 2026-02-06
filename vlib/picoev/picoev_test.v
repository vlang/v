module picoev

fn test_if_all_file_descriptors_are_properly_initialized() {
	mut pv := &Picoev{}
	pv.init()

	for i in 0 .. max_fds {
		assert unsafe { pv.file_descriptors[i] } != unsafe { nil }
		assert unsafe { pv.file_descriptors[i].loop_id } == -1
		assert unsafe { pv.file_descriptors[i].fd } == 0
	}
}
