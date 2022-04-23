module objects

import gx
import rand

pub fn random_color() gx.Color {
	return gx.Color{
		r: rand.u8()
		g: rand.u8()
		b: rand.u8()
	}
}
