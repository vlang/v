// vtest build: windows

import net as _

fn accept_wsa_data(_ &C.WSAData) {}

fn accept_wsadata(_ &C.WSADATA) {}

fn test_wsa_data_names_remain_available() {
	legacy := C.WSAData{}
	typedef := C.WSADATA{}
	accept_wsa_data(&legacy)
	accept_wsadata(&typedef)
}
