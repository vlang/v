// vtest build: !sanitize-address-gcc && !sanitize-address-clang
// vtest vflags: -autofree -cg
module main

import net

fn test_autofree_net_addr_len_from_resolve_addrs() {
	addrs := net.resolve_addrs(':8008', .ip, .tcp)!
	addr := addrs[0]
	assert addr.family() == .ip
	assert addr.len() > 0
}
