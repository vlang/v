module picoev

import picohttpparser
import time

fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	if req.method == 'GET' {
		res.http_ok()
		res.header_server()
		res.header_date()
		res.html()
		res.body('Hello Picoev!\n')
	} else {
		res.http_405()
	}
	res.end()
}

fn test_if_all_file_descriptors_are_properly_initialized() {
	mut pv := &Picoev{}
	assert pv.file_descriptors.len == max_fds

	// test not inited
	for i in 0 .. max_fds {
		assert unsafe { pv.file_descriptors[i] } == unsafe { nil } // inited after pv.init()
	}
	assert pv.loop == unsafe { nil } // loop not inited

	fd := 5
	assert pv.add(fd, picoev_read, 10, unsafe { nil }) == -1
	assert pv.add(fd, picoev_add, 10, unsafe { nil }) == -1

	// test init
	pv.init()

	assert pv.add(fd, picoev_read, 10, unsafe { nil }) == -1
	assert pv.add(fd, picoev_add, 10, unsafe { nil }) == -1

	for i in 0 .. max_fds {
		assert unsafe { pv.file_descriptors[i] } != unsafe { nil }
		assert unsafe { pv.file_descriptors[i].fd } == 0
		assert unsafe { pv.file_descriptors[i].loop_id } == -1
		assert unsafe { pv.file_descriptors[i].events } == 0
		assert unsafe { pv.file_descriptors[i].cb } == unsafe { nil }
		assert unsafe { pv.file_descriptors[i].backend } == 0

		assert pv.loop == unsafe { nil } // loop not inited

		// assert pv.del(pv.file_descriptors[i].fd) == -1
	}

	for i in 0 .. max_fds {
		unsafe { free(pv.file_descriptors[i]) }
	}

	unsafe { free(pv) }

	mut new_pv := new(port: 8080, cb: callback)
	new_pv.init() // FIXME/REVIEW - should not need init. It is inited at new()

	assert new_pv.add(fd, picoev_add, 10, unsafe { nil }) == 0
	// assert new_pv.add(fd, picoev_read, 10, unsafe { nil }) == 0 // TODO

	for i in 0 .. max_fds {
		assert unsafe { pv.file_descriptors[i] } != unsafe { nil }
		assert unsafe { pv.file_descriptors[i].fd } == 0
		assert unsafe { pv.file_descriptors[i].loop_id } == -1
		assert unsafe { pv.file_descriptors[i].events } == 0
		assert unsafe { pv.file_descriptors[i].cb } == unsafe { nil } // FIXME/REVIEW should not be nil
		assert unsafe { pv.file_descriptors[i].backend } == 0

		assert new_pv.loop != unsafe { nil } // loop inited

		assert new_pv.del(pv.file_descriptors[i].fd) == -1
		new_pv.close_conn(pv.file_descriptors[i].fd) // REVIEW and TEST
	}

	for i in 0 .. max_fds {
		unsafe { free(new_pv.file_descriptors[i]) }
	}

	unsafe { free(new_pv) }
}

fn test_if_all_file_descriptors_are_properly_initialized() {
	mut pv := &Picoev{}
	pv.init()

	for i in 0 .. max_fds {
		assert unsafe { pv.file_descriptors[i] } != unsafe { nil }
		assert unsafe { pv.file_descriptors[i].loop_id } == -1
		assert unsafe { pv.file_descriptors[i].fd } == 0
	}
}
