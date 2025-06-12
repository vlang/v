enum Traffic {
	rx_bytes
	tx_bytes
}

fn (t Traffic) str() string {
	return match t {
		.rx_bytes { 'rx-bytes' }
		.tx_bytes { 'tx-bytes' }
	}
}

fn Traffic.from_string(s string) ?Traffic {
	return match s {
		'rx-bytes' { .rx_bytes }
		'tx-bytes' { .tx_bytes }
		else { none }
	}
}

fn test_main() {
	traffic := Traffic.from_string('rx-bytes') or { return }
	assert Traffic.rx_bytes.str() == 'rx-bytes'
	assert '${traffic}' == 'rx-bytes'
	assert '${Traffic.rx_bytes}' == 'rx-bytes'
}
