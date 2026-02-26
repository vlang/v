module http

import io
import net
import strings

fn receive_all_data_timeout_cb(_ voidptr, _ &u8, _ int) !int {
	return error_with_code('read timed out', net.err_timed_out_code)
}

fn receive_all_data_eof_cb(_ voidptr, _ &u8, _ int) !int {
	return io.Eof{}
}

fn test_receive_all_data_from_cb_in_builder_propagates_non_eof_errors() {
	mut req := Request{}
	mut content := strings.new_builder(64)
	req.receive_all_data_from_cb_in_builder(mut content, unsafe { nil }, receive_all_data_timeout_cb) or {
		assert err.code() == net.err_timed_out_code
		return
	}
	panic('expected a timeout error')
}

fn test_receive_all_data_from_cb_in_builder_stops_on_eof() {
	mut req := Request{}
	mut content := strings.new_builder(64)
	req.receive_all_data_from_cb_in_builder(mut content, unsafe { nil }, receive_all_data_eof_cb) or {
		panic('unexpected error: ${err}')
	}
	assert content.str() == ''
}
