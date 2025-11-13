module unix

import net as _

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
