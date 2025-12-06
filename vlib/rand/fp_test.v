import rand

struct Histo {
mut:
	lo f64
	ct int
}

// The sample size to be used; keep cpu time less than 5 seconds
const count = 9100100
// Two sets of seeds
const seeds = [[u32(2742798260), 2159764996], [u32(2135051596), 958016781]]

fn test_f32() {
	mut histo := []Histo{}
	mut candidate := f32(0.0)

	histo << Histo{1.0e-9, 0}
	histo << Histo{1.0e-6, 0}
	histo << Histo{1.0e-4, 0}
	histo << Histo{1.0e-2, 0}
	histo << Histo{1.0e00, 0}

	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. count {
			candidate = rand.f32()
			for mut p in histo {
				if candidate < p.lo {
					p.ct += 1
				}
			}
		}
	}
	println(' f32  ')
	println(histo)
	assert histo[0].ct == 4
	assert histo[1].ct == 21
	assert histo[2].ct == 1821
	assert histo[3].ct == 182583
	assert histo[4].ct == 18200200
	for mut p in histo {
		p.ct = 0
	}
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. count {
			candidate = rand.f32cp()
			for mut p in histo {
				if candidate < p.lo {
					p.ct += 1
				}
			}
		}
	}
	println(' f32cp')
	println(histo)
	assert histo[0].ct == 0
	assert histo[1].ct == 22
	assert histo[2].ct == 1829
	assert histo[3].ct == 142203
	assert histo[4].ct == 18200200
}

fn test_f64() {
	mut histo := []Histo{}
	mut candidate := f64(0.0)

	histo << Histo{1.0e-9, 0}
	histo << Histo{1.0e-6, 0}
	histo << Histo{1.0e-4, 0}
	histo << Histo{1.0e-2, 0}
	histo << Histo{1.0e00, 0}

	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. count {
			candidate = rand.f64()
			for mut p in histo {
				if candidate < p.lo {
					p.ct += 1
				}
			}
		}
	}
	println(' f64  ')
	println(histo)
	assert histo[0].ct == 0
	assert histo[1].ct == 25
	assert histo[2].ct == 1763
	assert histo[3].ct == 182552
	assert histo[4].ct == 18200200
	for mut p in histo {
		p.ct = 0
	}
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. count {
			candidate = rand.f64cp()
			for mut p in histo {
				if candidate < p.lo {
					p.ct += 1
				}
			}
		}
	}
	println(' f64cp')
	println(histo)
	assert histo[0].ct == 0
	assert histo[1].ct == 22
	assert histo[2].ct == 1787
	assert histo[3].ct == 182160
	assert histo[4].ct == 18200200
}
