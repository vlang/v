import math.stats
import math

fn test_freq() {
	// Tests were also verified on Wolfram Alpha
	data := [f64(10.0), f64(10.0), f64(5.9), f64(2.7)]
	mut o := stats.freq(data, 10.0)
	assert o == 2
	o = stats.freq(data, 2.7)
	assert o == 1
	o = stats.freq(data, 15)
	assert o == 0
}

fn tst_res(str1 string, str2 string) bool {
	if (math.abs(str1.f64() - str2.f64())) < 1e-5 {
		return true
	}
	return false
}

fn test_mean() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '5.762500')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '17.650000')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '37.708000')
}

fn test_geometric_mean() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.geometric_mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '5.15993')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.geometric_mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert o.str().eq('nan') || o.str().eq('-nan') || o.str().eq('-1.#IND00') || o == f64(0) ||
		o.str().eq('-nan(ind)') // Because in math it yields a complex number
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.geometric_mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '25.064496')
}

fn test_harmonic_mean() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.harmonic_mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '4.626519')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.harmonic_mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '9.134577')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.harmonic_mean(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '16.555477')
}

fn test_median() {
	// Tests were also verified on Wolfram Alpha
	// Assumes sorted array
	// Even
	mut data := [f64(2.7), f64(4.45), f64(5.9), f64(10.0)]
	mut o := stats.median(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '5.175000')
	data = [f64(-3.0), f64(1.89), f64(4.4), f64(67.31)]
	o = stats.median(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '3.145000')
	data = [f64(7.88), f64(12.0), f64(54.83), f64(76.122)]
	o = stats.median(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '33.415000')
	// Odd
	data = [f64(2.7), f64(4.45), f64(5.9), f64(10.0), f64(22)]
	o = stats.median(data)
	assert o == f64(5.9)
	data = [f64(-3.0), f64(1.89), f64(4.4), f64(9), f64(67.31)]
	o = stats.median(data)
	assert o == f64(4.4)
	data = [f64(7.88), f64(3.3), f64(12.0), f64(54.83), f64(76.122)]
	o = stats.median(data)
	assert o == f64(12.0)
}

fn test_mode() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(2.7), f64(2.7), f64(4.45), f64(5.9), f64(10.0)]
	mut o := stats.mode(data)
	assert o == f64(2.7)
	data = [f64(-3.0), f64(1.89), f64(1.89), f64(1.89), f64(9), f64(4.4), f64(4.4), f64(9), f64(67.31)]
	o = stats.mode(data)
	assert o == f64(1.89)
	// Testing greedy nature
	data = [f64(2.0), f64(4.0), f64(2.0), f64(4.0)]
	o = stats.mode(data)
	assert o == f64(2.0)
}

fn test_rms() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.rms(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '6.362046')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.rms(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '33.773393')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.rms(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '47.452561')
}

fn test_population_variance() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.population_variance(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '7.269219')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.population_variance(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '829.119550')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.population_variance(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '829.852282')
}

fn test_sample_variance() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.sample_variance(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '9.692292')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.sample_variance(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '1105.492733')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.sample_variance(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '1106.469709')
}

fn test_population_stddev() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.population_stddev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '2.696149')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.population_stddev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '28.794436')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.population_stddev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '28.807157')
}

fn test_sample_stddev() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.sample_stddev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '3.113245')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.sample_stddev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '33.248951')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.sample_stddev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '33.263639')
}

fn test_mean_absdev() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.mean_absdev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '2.187500')
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.mean_absdev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '24.830000')
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.mean_absdev(data)
	// Some issue with precision comparison in f64 using == operator hence serializing to string
	assert tst_res(o.str(), '27.768000')
}

fn test_min() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.min(data)
	assert o == f64(2.7)
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.min(data)
	assert o == f64(-3.0)
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.min(data)
	assert o == f64(7.88)
}

fn test_max() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.max(data)
	assert o == f64(10.0)
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.max(data)
	assert o == f64(67.31)
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.max(data)
	assert o == f64(76.122)
}

fn test_range() {
	// Tests were also verified on Wolfram Alpha
	mut data := [f64(10.0), f64(4.45), f64(5.9), f64(2.7)]
	mut o := stats.range(data)
	assert o == f64(7.3)
	data = [f64(-3.0), f64(67.31), f64(4.4), f64(1.89)]
	o = stats.range(data)
	assert o == f64(70.31)
	data = [f64(12.0), f64(7.88), f64(76.122), f64(54.83)]
	o = stats.range(data)
	assert o == f64(68.242)
}

fn test_passing_empty() {
	data := []f64{}
	assert stats.freq(data, 0) == 0
	assert stats.mean(data) == f64(0)
	assert stats.geometric_mean(data) == f64(0)
	assert stats.harmonic_mean(data) == f64(0)
	assert stats.median(data) == f64(0)
	assert stats.mode(data) == f64(0)
	assert stats.rms(data) == f64(0)
	assert stats.population_variance(data) == f64(0)
	assert stats.sample_variance(data) == f64(0)
	assert stats.population_stddev(data) == f64(0)
	assert stats.sample_stddev(data) == f64(0)
	assert stats.mean_absdev(data) == f64(0)
	assert stats.min(data) == f64(0)
	assert stats.max(data) == f64(0)
	assert stats.range(data) == f64(0)
}
