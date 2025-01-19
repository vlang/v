import math
import math.stats

fn test_freq() {
	// Tests were also verified on Wolfram Alpha
	data := [10.0, 10.0, 5.9, 2.7]
	mut o := stats.freq(data, 10.0)
	assert o == 2
	o = stats.freq(data, 2.7)
	assert o == 1
	o = stats.freq(data, 15)
	assert o == 0

	// test for int, i64, f32 array
	assert stats.freq[int]([1, 3, 5, 7], 5) == 1
	assert stats.freq[i64]([i64(1), 3, 5, 7], 5) == 1
	assert stats.freq[f32]([f32(1.0), 3, 5, 7], 3.0) == 1
}

fn test_mean() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.mean(data)
	assert math.alike(o, 5.7625)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.mean(data)
	assert math.alike(o, 17.65)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.mean(data)
	assert math.alike(o, 37.708)

	// test for int, i64, f32 array
	assert stats.mean[int]([1, 2]) == 1
	assert stats.mean[i64]([i64(1), 2]) == 1
	o = stats.mean[f32]([f32(1.0), 3, 5, 7])
	assert math.alike(o, 4.0)
}

fn test_geometric_mean() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.geometric_mean(data)
	assert math.alike(o, 5.159931624158176)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.geometric_mean(data)
	assert math.is_nan(o) // Because in math it yields a complex number
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.geometric_mean(data)
	assert math.alike(o, 25.064495926603378)

	// test for int, i64, f32 array
	assert stats.geometric_mean[int]([1, 3, 5, 7]) == 3
	assert stats.geometric_mean[i64]([i64(1), 3, 5, 7]) == 3
	o = stats.geometric_mean[f32]([f32(1.0), 3, 5, 7])
	assert math.alike(o, 3.2010858058929443)
}

fn test_harmonic_mean() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.harmonic_mean(data)
	assert math.alike(o, 4.626518526616179)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.harmonic_mean(data)
	assert math.alike(o, 9.134577425605814)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.harmonic_mean(data)
	assert math.alike(o, 16.555477040152685)

	// test for int, i64, f32 array
	assert stats.harmonic_mean[int]([1, 2]) == 1
	assert stats.harmonic_mean[i64]([i64(1), 2]) == 1
	o = stats.harmonic_mean[f32]([f32(1.0), 3, 5, 7])
	assert math.alike(o, 2.3863635063171387)
}

fn test_median() {
	// Tests were also verified on Wolfram Alpha
	// Assumes sorted array

	// Even
	mut data := [2.7, 4.45, 5.9, 10.0]
	mut o := stats.median(data)
	assert math.alike(o, 5.175000000000001)
	data = [-3.0, 1.89, 4.4, 67.31]
	o = stats.median(data)
	assert math.alike(o, 3.145)
	data = [7.88, 12.0, 54.83, 76.122]
	o = stats.median(data)
	assert math.alike(o, 33.415)

	// Odd
	data = [2.7, 4.45, 5.9, 10.0, 22]
	o = stats.median(data)
	assert math.alike(o, 5.9)
	data = [-3.0, 1.89, 4.4, 9, 67.31]
	o = stats.median(data)
	assert math.alike(o, 4.4)
	data = [7.88, 3.3, 12.0, 54.83, 76.122]
	o = stats.median(data)
	assert math.alike(o, 12.0)

	// test for int, i64, f32 array
	assert stats.median[int]([1, 2, 3]) == 2
	assert stats.median[i64]([i64(1), 2, 3]) == 2
	o = stats.median[f32]([f32(1.0), 3, 5, 7])
	assert math.alike(o, 4)
}

fn test_mode() {
	// Tests were also verified on Wolfram Alpha
	mut data := [2.7, 2.7, 4.45, 5.9, 10.0]
	mut o := stats.mode(data)
	assert math.alike(o, 2.7)
	data = [-3.0, 1.89, 1.89, 1.89, 9, 4.4, 4.4, 9, 67.31]
	o = stats.mode(data)
	assert math.alike(o, 1.89)
	// Testing greedy nature
	data = [2.0, 4.0, 2.0, 4.0]
	o = stats.mode(data)
	assert math.alike(o, 2.0)

	// test for int, i64, f32 array
	assert stats.mode[int]([1, 2, 3, 1]) == 1
	assert stats.mode[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.mode[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 3)
}

fn test_rms() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.rms(data)
	assert math.alike(o, 6.362045661577729)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.rms(data)
	assert math.alike(o, 33.77339263384714)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.rms(data)
	assert math.alike(o, 47.45256100570337)

	// test for int, i64, f32 array
	assert stats.rms[int]([1, 2, 3, 1]) == 1
	assert stats.rms[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.rms[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 4.312771797180176)
}

fn test_population_variance() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.population_variance(data)
	assert math.alike(o, 7.269218749999999)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.population_variance(data)
	assert math.alike(o, 829.119550)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.population_variance(data)
	assert math.alike(o, 829.852282)

	// test for int, i64, f32 array
	assert stats.population_variance[int]([1, 2, 3, 1]) == 1
	assert stats.population_variance[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.population_variance[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 4.159999847412109)
}

fn test_sample_variance() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.sample_variance(data)
	assert math.alike(o, 9.692291666666666)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.sample_variance(data)
	assert math.alike(o, 1105.4927333333333)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.sample_variance(data)
	assert math.alike(o, 1106.4697093333332)

	// test for int, i64, f32 array
	assert stats.sample_variance[int]([1, 2, 3, 1]) == 1
	assert stats.sample_variance[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.sample_variance[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 5.199999809265137)
}

fn test_population_stddev() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.population_stddev(data)
	assert math.alike(o, 2.6961488738569312)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.population_stddev(data)
	assert math.alike(o, 28.794436094495754)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.population_stddev(data)
	assert math.alike(o, 28.80715678438259)

	// test for int, i64, f32 array
	assert stats.population_stddev[int]([1, 2, 3, 1]) == 1
	assert stats.population_stddev[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.population_stddev[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 2.0396077632904053)
}

fn test_sample_stddev() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.sample_stddev(data)
	assert math.alike(o, 3.1132445561932114)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.sample_stddev(data)
	assert math.alike(o, 33.2489508606412)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.sample_stddev(data)
	assert math.alike(o, 33.26363944810208)

	// test for int, i64, f32 array
	assert stats.sample_stddev[int]([1, 2, 3, 1]) == 1
	assert stats.sample_stddev[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.sample_stddev[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 2.280350923538208)
}

fn test_absdev() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.absdev(data)
	assert o == 2.187500048428774
	assert math.alike(o, 2.187500048428774)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.absdev(data)
	assert o == 24.829999923706055
	assert math.alike(o, 24.829999923706055)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.absdev(data)
	assert o == 27.76800012588501
	assert math.alike(o, 27.76800012588501)

	// test for int, i64, f32 array
	assert stats.absdev[int]([1, 2, 3, 1]) == 0
	assert stats.absdev[i64]([i64(1), 2, 3, 1]) == 0
	o = stats.absdev[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 1.7599999904632568)
}

fn test_tss() {
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.tss(data)
	assert math.alike(o, 29.076874999999998)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.tss(data)
	assert math.alike(o, 3316.4782)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.tss(data)
	assert math.alike(o, 3319.409128)

	// test for int, i64, f32 array
	assert stats.tss[int]([1, 2, 3, 1]) == 5
	assert stats.tss[i64]([i64(1), 2, 3, 1]) == 5
	o = stats.tss[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 20.799999237060547)
}

fn test_min() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.min(data)
	assert math.alike(o, 2.7)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.min(data)
	assert math.alike(o, -3.0)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.min(data)
	assert math.alike(o, 7.88)

	// test for int, i64, f32 array
	assert stats.min[int]([1, 2, 3, 1]) == 1
	assert stats.min[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.min[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 1.0)
}

fn test_max() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.max(data)
	assert math.alike(o, 10.0)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.max(data)
	assert math.alike(o, 67.31)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.max(data)
	assert math.alike(o, 76.122)

	// test for int, i64, f32 array
	assert stats.max[int]([1, 2, 3, 1]) == 3
	assert stats.max[i64]([i64(1), 2, 3, 1]) == 3
	o = stats.max[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 7.0)
}

fn test_minmax() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o_min, mut o_max := stats.minmax(data)
	assert [o_min, o_max] == [2.7, 10.0]
	data = [-3.0, 67.31, 4.4, 1.89]
	o_min, o_max = stats.minmax(data)
	assert [o_min, o_max] == [-3.0, 67.31]
	data = [12.0, 7.88, 76.122, 54.83]
	o_min, o_max = stats.minmax(data)
	assert [o_min, o_max] == [7.88, 76.122]

	// test for int, i64, f32 array
	o_min_int, o_max_int := stats.minmax[int]([1, 2, 3, 1])
	assert [o_min_int, o_max_int] == [1, 3]
	o_min_i64, o_max_i64 := stats.minmax[i64]([i64(1), 2, 3, 1])
	assert [o_min_i64, o_max_i64] == [i64(1), 3]
	o_min_f32, o_max_f32 := stats.minmax[f32]([f32(1.0), 3, 5, 7, 3])
	assert [o_min_f32, o_max_f32] == [f32(1.0), 7]
}

fn test_min_index() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.min_index(data)
	assert o == 3
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.min_index(data)
	assert o == 0
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.min_index(data)
	assert o == 1

	// test for int, i64, f32 array
	assert stats.min_index[int]([1, 2, 3, 1]) == 0
	assert stats.min_index[i64]([i64(1), 2, 3, 1]) == 0
	assert stats.min_index[f32]([f32(1.0), 3, 5, 7, 3]) == 0
}

fn test_max_index() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.max_index(data)
	assert o == 0
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.max_index(data)
	assert o == 1
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.max_index(data)
	assert o == 2

	// test for int, i64, f32 array
	assert stats.max_index[int]([1, 2, 3, 1]) == 2
	assert stats.max_index[i64]([i64(1), 2, 3, 1]) == 2
	assert stats.max_index[f32]([f32(1.0), 3, 5, 7, 3]) == 3
}

fn test_minmax_index() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o_min, mut o_max := stats.minmax_index(data)
	assert [o_min, o_max] == [3, 0]
	data = [-3.0, 67.31, 4.4, 1.89]
	o_min, o_max = stats.minmax_index(data)
	assert [o_min, o_max] == [0, 1]
	data = [12.0, 7.88, 76.122, 54.83]
	o_min, o_max = stats.minmax_index(data)
	assert [o_min, o_max] == [1, 2]

	// test for int, i64, f32 array
	o_min, o_max = stats.minmax_index[int]([1, 2, 3, 1])
	assert [o_min, o_max] == [0, 2]
	o_min, o_max = stats.minmax_index[i64]([i64(1), 2, 3, 1])
	assert [o_min, o_max] == [0, 2]
	o_min, o_max = stats.minmax_index[f32]([f32(1.0), 3, 5, 7, 3])
	assert [o_min, o_max] == [0, 3]
}

fn test_range() {
	// Tests were also verified on Wolfram Alpha
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.range(data)
	assert math.alike(o, 7.3)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.range(data)
	assert math.alike(o, 70.31)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.range(data)
	assert math.alike(o, 68.242)

	// test for int, i64, f32 array
	assert stats.range[int]([1, 2, 3, 1]) == 2
	assert stats.range[i64]([i64(1), 2, 3, 1]) == 2
	assert stats.range[f32]([f32(1.0), 3, 5, 7, 3]) == 6.0
}

fn test_covariance() {
	mut data0 := [10.0, 4.45, 5.9, 2.7]
	mut data1 := [5.0, 14.45, -15.9, 22.7]
	mut o := stats.covariance(data0, data1)
	assert math.alike(o, -17.37078125)
	data0 = [-3.0, 67.31, 4.4, 1.89]
	data1 = [5.0, 77.31, 44.4, 11.89]
	o = stats.covariance(data0, data1)
	assert math.alike(o, 740.06955)
	data0 = [12.0, 7.88, 76.122, 54.83]
	data1 = [2.0, 5.88, 7.122, 5.83]
	o = stats.covariance(data0, data1)
	assert math.alike(o, 36.650282000000004)

	// test for int, i64, f32 array
	data0_int := [1, 2, 3, 1]
	data1_int := [11, 22, 33, 11]
	o_int := stats.covariance[int](data0_int, data1_int)
	assert o_int == 8
	data0_i64 := [i64(1), 2, 3, 1]
	data1_i64 := [i64(11), 22, 33, 11]
	o_i64 := stats.covariance[i64](data0_i64, data1_i64)
	assert o_i64 == 8
	data0_f32 := [f32(1.0), 2, 3, 1]
	data1_f32 := [f32(11.0), 22, 33, 11]
	o_f32 := stats.covariance[f32](data0_f32, data1_f32)
	assert math.alike(o_f32, 7.562500476837158)
}

fn test_lag1_autocorrelation() {
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.lag1_autocorrelation(data)
	mut e := 0.0
	assert math.alike(o, -0.5542285481446095)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.lag1_autocorrelation(data)
	assert math.alike(o, -0.5102510654033415)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.lag1_autocorrelation(data)
	e = 0.10484450460892072
	assert math.alike(o, e), diff(o, e)

	// test for int, i64, f32 array
	assert stats.lag1_autocorrelation[int]([1, 2, 3, 1]) == 0
	assert stats.lag1_autocorrelation[i64]([i64(1), 2, 3, 1]) == 0
	o = stats.lag1_autocorrelation[f32]([f32(1.0), 3, 5, 7, 3])
	assert math.alike(o, 0.1975308507680893)
}

fn diff(actual f64, expected f64) string {
	return '\n  actual:${actual:40.35f}\nexpected:${expected:40.35f}\n    diff:${actual - expected:40.35f}'
}

fn test_kurtosis() {
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.kurtosis(data)
	mut e := -1.0443212849233845
	assert math.close(o, e), diff(o, e)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.kurtosis(data)
	e = -0.6884953374814851
	assert math.close(o, e), diff(o, e)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.kurtosis(data)
	assert math.alike(o, -1.7323772836921467)

	// test for int, i64, f32 array
	assert stats.kurtosis[int]([1, 2, 3, 1]) == 1
	assert stats.kurtosis[i64]([i64(1), 2, 3, 1]) == 1
	o = stats.kurtosis[f32]([f32(1.0), 3, 5, 7, 3])
	e = -1.044378399848938
	assert math.alike(o, e), diff(o, e)
}

fn test_skew() {
	mut data := [10.0, 4.45, 5.9, 2.7]
	mut o := stats.skew(data)
	mut e := 0.5754021106320453
	assert math.veryclose(o, e), diff(o, e)
	data = [-3.0, 67.31, 4.4, 1.89]
	o = stats.skew(data)
	e = 1.1248733711136492
	assert math.veryclose(o, e), diff(o, e)
	data = [12.0, 7.88, 76.122, 54.83]
	o = stats.skew(data)
	e = 0.19007911706827735
	assert math.alike(o, e), diff(o, e)

	// test for int, i64, f32 array
	assert stats.skew[int]([1, 2, 3, 1]) == 2
	assert stats.skew[i64]([i64(1), 2, 3, 1]) == 2
	o = stats.skew[f32]([f32(1.0), 3, 5, 7, 3])
	e = 0.27154541015625
	assert math.alike(o, e), diff(o, e)
}

fn test_quantile() {
	// Assumes sorted array

	mut data := [2.7, 4.45, 5.9, 10.0]
	mut o := stats.quantile(data, 0.1)!
	assert math.alike(o, 3.225)
	data = [-3.0, 1.89, 4.4, 67.31]
	o = stats.quantile(data, 0.2)!
	assert math.alike(o, -0.06599999999999961)
	data = [7.88, 12.0, 54.83, 76.122]
	o = stats.quantile(data, 0.3)!
	assert math.alike(o, 11.588)

	stats.quantile(data, -0.3) or { assert err.msg() == 'index out of range' }

	stats.quantile(data, 2) or { assert err.msg() == 'index out of range' }

	// test for int, i64, f32 array
	assert stats.quantile[int]([1, 2, 3], 1)! == 3
	assert stats.quantile[i64]([i64(1), 2, 3], 1)! == 3
	o = stats.quantile[f32]([f32(1.0), 3, 5, 7], 0.22)!
	assert math.alike(o, 2.319999933242798)
}

fn test_passing_empty() {
	data := []f64{}
	assert stats.freq(data, 0) == 0
	assert stats.mean(data) == 0
	assert stats.geometric_mean(data) == 0
	assert stats.harmonic_mean(data) == 0
	assert stats.median(data) == 0
	assert stats.mode(data) == 0
	assert stats.rms(data) == 0
	assert stats.population_variance(data) == 0
	assert stats.sample_variance(data) == 0
	assert stats.population_stddev(data) == 0
	assert stats.sample_stddev(data) == 0
	assert stats.absdev(data) == 0
	assert stats.min(data) == 0
	assert stats.max(data) == 0
	o_min, o_max := stats.minmax(data)
	assert [o_min, o_max] == [f64(0), 0]
	assert stats.min_index(data) == 0
	assert stats.max_index(data) == 0
	o_min_index, o_max_index := stats.minmax_index(data)
	assert [o_min_index, o_max_index] == [0, 0]
	assert stats.range(data) == 0
	assert stats.covariance(data, data) == 0
	assert stats.lag1_autocorrelation(data) == 0
	assert stats.kurtosis(data) == 0
	assert stats.skew(data) == 0
	assert stats.quantile(data, 0)! == 0
}

fn test_passing_one() {
	data := [100.0]
	assert stats.freq(data, 100.0) == 1
	assert stats.mean(data) == 100.0
	assert stats.geometric_mean(data) == 100.0
	assert stats.harmonic_mean(data) == 100.0
	assert stats.median(data) == 100.0
	assert stats.mode(data) == 100.0
	assert stats.rms(data) == 100.0
	assert stats.population_variance(data) == 0.0
	assert math.is_nan(stats.sample_variance(data))
	assert stats.population_stddev(data) == 0.0
	assert math.is_nan(stats.sample_stddev(data))
	assert stats.absdev(data) == 0.0
	assert stats.min(data) == 100.0
	assert stats.max(data) == 100.0
	o_min, o_max := stats.minmax(data)
	assert [o_min, o_max] == [f64(100), 100]
	assert stats.min_index(data) == 0
	assert stats.max_index(data) == 0
	o_min_index, o_max_index := stats.minmax_index(data)
	assert [o_min_index, o_max_index] == [0, 0]
	assert stats.range(data) == 0
	assert stats.covariance(data, data) == 0
	assert math.is_nan(stats.lag1_autocorrelation(data))
	assert math.is_nan(stats.kurtosis(data))
	assert math.is_nan(stats.skew(data))
	assert stats.quantile(data, 0)! == 100
}
