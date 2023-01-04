import rand

struct Histo {
mut:
	lo f64
	ct int
}

const (
	// The sample size to be used
	count = 90100100
	// Two sets of seeds
	seeds = [[u32(2742798260), 2159764996], [u32(2135051596), 958016781]]
)

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
	assert histo[0].ct == 13
	assert histo[1].ct == 165
	assert histo[2].ct == 18087
	assert histo[3].ct == 1802475
	assert histo[4].ct == 180200200
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
	assert histo[0].ct == 1
	assert histo[1].ct == 176
	assert histo[2].ct == 18077
	assert histo[3].ct == 1408393
	assert histo[4].ct == 180200200
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
	assert histo[1].ct == 159
	assert histo[2].ct == 17893
	assert histo[3].ct == 1803670
	assert histo[4].ct == 180200200
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
	assert histo[1].ct == 174
	assert histo[2].ct == 18077
	assert histo[3].ct == 1803432
	assert histo[4].ct == 180200200
}
