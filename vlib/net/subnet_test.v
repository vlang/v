import net

fn test_cidr_rfc_vectors() {
	n := net.parse_cidr('192.168.1.0/24')!
	assert n.network() == '192.168.1.0'
	assert n.broadcast() == '192.168.1.255'
	assert n.size() == 256
	assert n.contains('192.168.1.77')
	assert n.contains('192.168.1.0')
	assert n.contains('192.168.1.255')
	assert !n.contains('192.168.2.1')
	assert !n.contains('10.0.0.1')
	assert !n.contains('not-an-ip'), 'malformed input is not contained'
	ten := net.parse_cidr('10.0.0.0/8')!
	assert ten.contains('10.244.3.9')
	assert !ten.contains('11.0.0.0')
	assert ten.size() == 16777216
}

fn test_cidr_edges() {
	host := net.parse_cidr('192.168.1.77/32')!
	assert host.size() == 1
	assert host.contains('192.168.1.77')
	assert !host.contains('192.168.1.78')
	assert host.network() == '192.168.1.77'
	all := net.parse_cidr('0.0.0.0/0')!
	assert all.size() == 4294967296
	assert all.contains('1.2.3.4')
	assert all.contains('255.255.255.255')
	pair := net.parse_cidr('192.168.1.0/31')!
	assert pair.size() == 2
	assert net.format_ip(net.parse_ip('1.2.3.4')!) == '1.2.3.4'
}

fn test_cidr_malformed() {
	net.parse_cidr('192.168.1.0') or {
		assert err.msg().contains('missing a prefix')
		return
	}
	assert false, 'missing prefix must error'
	net.parse_cidr('192.168.1.0/33') or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'prefix > 32 must error'
	net.parse_cidr('192.168.1.0/abc') or {
		assert err.msg().contains('bad prefix')
		return
	}
	assert false, 'non-numeric prefix must error'
	net.parse_cidr('999.1.1.0/24') or {
		assert err.msg().contains('bad IPv4')
		return
	}
	assert false, 'octet > 255 must error'
	net.parse_cidr('1.2.3/24') or {
		assert err.msg().contains('bad IPv4')
		return
	}
	assert false, 'short address must error'
	net.parse_cidr('01.2.3.4/24') or {
		assert err.msg().contains('bad IPv4')
		return
	}
	assert false, 'leading zeros must error'
}
