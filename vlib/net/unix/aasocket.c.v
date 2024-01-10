module unix

import net

const use_net = net.no_timeout

const max_sun_path = $if windows {
	256
} $else {
	// 104 for macos, 108 for linux => use the minimum
	104
}

// Select represents a select operation
enum Select {
	read
	write
	except
}
