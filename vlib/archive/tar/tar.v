module tar

// ustart header block octets
// Field    | Offset | Length
// --------------------------
// name     |   0    | 100
// mode     | 100    |   8
// uid      | 108    |   8
// gid      | 116    |   8
// size     | 124    |  12
// mtime    | 136    |  12
// chksum   | 148    |   8
// typeflag | 156    |   1
// linkname | 157    | 100
// magic    | 257    |   6
// version  | 263    |   2
// uname    | 265    |  32
// gname    | 297    |  32
// devmajor | 329    |   8
// devminor | 337    |   8
// prefix   | 345    | 155

pub enum BlockHeader as u8 {
	file      = u8(`0`) // 0x30
	hard_link = u8(`1`) // 0x31
	sym_link  = u8(`2`) // 0x32
	char_dev  = u8(`3`) // 0x33
	block_dev = u8(`4`) // 0x34
	dir       = u8(`5`) // 0x35
	fifo      = u8(`6`) // 0x36
	long_name = u8(`L`) // 0x4c = 76 dec
	global    = u8(`g`) // 0x67 pax
}

pub enum BlockSpecial {
	no        // for headers `0`,`5` or data blocks
	blank_1   // first blank block: continue
	blank_2   // second blank block: end of archiv
	ignore    // for headers `1`, `2`, `3`, `4`, `6`
	long_name // for header `L`
	global    // for header `g`
	unknown   // for not header defined
}
