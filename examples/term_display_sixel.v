module main

import os
import term

fn main() {
	println('If your terminal supports the sixel graphics format,')
	println('you should see a small green "HI" and a V logo below this text.\n')
	if term.supports_sixel() {
		// Prints the original "HI" sixel image from the spec:
		// https://www.digiater.nl/openvms/decus/vax90b1/krypton-nasa/all-about-sixels.text (See "V. EXAMPLE SIXEL IMAGE" section)
		println('\ePq
#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0
#1~~@@vv@@~~@@~~$
#2??}}GG}}??}}??-
#1!14@
\e\\')
		// Prints a 128x128 pixels V logo
		bytes := os.read_bytes(os.resource_abs_path('assets/v.six'))!
		println(bytes.bytestr())
		dump(term.supports_sixel())
		dump(term.graphics_num_colors())
	}
}
