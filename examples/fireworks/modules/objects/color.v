module objects

import gg
import rand

pub fn random_color() gg.Color {
	return gg.Color{
		r: rand.u8()
		g: rand.u8()
		b: rand.u8()
	}
}
