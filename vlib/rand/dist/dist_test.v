import math
import rand
import rand.dist

const (
	// The sample size to be used
	count = 2000
	// Accepted error is within 5% of the actual values.
	error = 0.05
	// The seeds used (for reproducible testing)
	seeds = [[u32(0xffff24), 0xabcd], [u32(0x141024), 0x42851],
		[u32(0x1452), 0x90cd],
	]
)

fn test_bernoulli() {
	ps := [0.0, 0.1, 1.0 / 3.0, 0.5, 0.8, 17.0 / 18.0, 1.0]

	for seed in seeds {
		rand.seed(seed)
		for p in ps {
			mut successes := 0
			for _ in 0 .. count {
				if dist.bernoulli(p) {
					successes++
				}
			}
			assert math.abs(f64(successes) / count - p) < error
		}
	}
}

fn test_binomial() {
	ns := [100, 200, 1000]
	ps := [0.0, 0.5, 0.95, 1.0]

	for seed in seeds {
		rand.seed(seed)
		for n in ns {
			for p in ps {
				np := n * p
				npq := np * (1 - p)

				mut sum := 0
				mut var := 0.0
				for _ in 0 .. count {
					x := dist.binomial(n, p)
					sum += x
					dist := (x - np)
					var += dist * dist
				}

				assert math.abs(f64(sum / count) - np) / n < error
				assert math.abs(f64(var / count) - npq) / n < error
			}
		}
	}
}

fn test_normal_pair() {
	mus := [0, 10, 100, -40]
	sigmas := [1, 2, 40, 5]
	total := 2 * count

	for seed in seeds {
		rand.seed(seed)
		for mu in mus {
			for sigma in sigmas {
				mut sum := 0.0
				mut var := 0.0
				for _ in 0 .. count {
					x, y := dist.normal_pair(mu: mu, sigma: sigma)
					sum += x + y
					dist_x := x - mu
					dist_y := y - mu
					var += dist_x * dist_x
					var += dist_y * dist_y
				}

				variance := sigma * sigma
				assert math.abs(f64(sum / total) - mu) / sigma < 1
				assert math.abs(f64(var / total) - variance) / variance < 2 * error
			}
		}
	}
}

fn test_normal() {
	mus := [0, 10, 100, -40, 20]
	sigmas := [1, 2, 5]

	for seed in seeds {
		rand.seed(seed)
		for mu in mus {
			for sigma in sigmas {
				mut sum := 0.0
				mut var := 0.0
				for _ in 0 .. count {
					x := dist.normal(mu: mu, sigma: sigma)
					sum += x
					dist := x - mu
					var += dist * dist
				}

				variance := sigma * sigma
				assert math.abs(f64(sum / count) - mu) / sigma < 1
				assert math.abs(f64(var / count) - variance) / variance < 2 * error
			}
		}
	}
}
