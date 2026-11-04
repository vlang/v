module math

// frama returns fractal adaptive moving average (simplified).
pub fn frama(values []f64, period int) []f64 {
	if period <= 0 || values.len == 0 {
		return []f64{}
	}
	mut out := []f64{len: values.len}
	out[0] = values[0]
	for i in 1 .. values.len {
		start := if i >= period { i - period } else { 0 }
		mut h1 := values[start]
		mut l1 := values[start]
		for j in start .. i / 2 + start + 1 {
			if values[j] > h1 {
				h1 = values[j]
			}
			if values[j] < l1 {
				l1 = values[j]
			}
		}
		mut h2 := values[i / 2 + start]
		mut l2 := values[i / 2 + start]
		for j in i / 2 + start .. i + 1 {
			if values[j] > h2 {
				h2 = values[j]
			}
			if values[j] < l2 {
				l2 = values[j]
			}
		}
		mut h := values[start]
		mut l := values[start]
		for j in start .. i + 1 {
			if values[j] > h {
				h = values[j]
			}
			if values[j] < l {
				l = values[j]
			}
		}
		n1 := (h1 - l1) / f64((i - start + 1) / 2)
		n2 := (h2 - l2) / f64((i - start + 1) / 2)
		n3 := (h - l) / f64(i - start + 1)
		d := if n1 + n2 == 0 { 0 } else { math.log(n1 + n2) - math.log(n3) }
		alpha := math.exp(-4.6 * (d - 1))
		sc := if alpha < 0.01 {
			0.01
		} else if alpha > 1 {
			1
		} else {
			alpha
		}
		out[i] = out[i - 1] + sc * (values[i] - out[i - 1])
	}
	return out
}
