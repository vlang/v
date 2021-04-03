
import net

for addr in [
	'vlang.io:80',
	'google.com:80',
	'steampowered.com:80',
	'api.steampowered.com:80'
] {
	println('$addr')

	addrs := net.resolve_addrs(addr, .unspec, .tcp) or {
		println('> None')
		continue
	}


	for a in addrs {
		println('> $a ${a.family} ${a.@type}')
	}
}
