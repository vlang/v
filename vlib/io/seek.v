module io

import os

// Seeker is implemented by streams that support random-access repositioning,
// e.g. os.File (see os.SeekMode for the origin values).
pub interface Seeker {
mut:
	seek(pos i64, mode os.SeekMode) !
}
