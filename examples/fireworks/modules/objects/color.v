module objects

import gg
import rand

pub fn random_color() gg.Color {
	return gg.Color{
		r: byte(rand.int_in_range(0, 256))
		g: byte(rand.int_in_range(0, 256))
		b: byte(rand.int_in_range(0, 256))
	}
}
