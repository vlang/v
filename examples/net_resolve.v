import net

for addr in [
	'vlang.io:80',
	'google.com:80',
	'steampowered.com:80',
	'api.steampowered.com:80',
] {
	println('${addr}')

	for @type in [net.SocketType.tcp, .udp] {
		family := net.AddrFamily.unspec

		addrs := net.resolve_addrs(addr, family, @type) or {
			println('> None')
			continue
		}

		for a in addrs {
			f := a.family()
			println('> ${a} ${f} ${@type}')
		}
	}
}
