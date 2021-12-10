module stats

import math

// Measure of Occurance
// Frequency of a given number
// Based on
// https://www.mathsisfun.com/data/frequency-distribution.html
pub fn freq<T>(data []T, val T) int {
	if data.len == 0 {
		return 0
	}
	mut count := 0
	for v in data {
		if v == val {
			count++
		}
	}
	return count
}

// Measure of Central Tendancy
// Mean of the given input array
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn mean<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := T(0)
	for v in data {
		sum += v
	}
	return sum / T(data.len)
}

// Measure of Central Tendancy
// Geometric Mean of the given input array
// Based on
// https://www.mathsisfun.com/numbers/geometric-mean.html
pub fn geometric_mean<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := 1.0
	for v in data {
		sum *= v
	}
	return math.pow(sum, 1.0 / T(data.len))
}

// Measure of Central Tendancy
// Harmonic Mean of the given input array
// Based on
// https://www.mathsisfun.com/numbers/harmonic-mean.html
pub fn harmonic_mean<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := T(0)
	for v in data {
		sum += 1.0 / v
	}
	return T(data.len) / sum
}

// Measure of Central Tendancy
// Median of the given input array ( input array is assumed to be sorted )
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn median<T>(sorted_data []T) T {
	if sorted_data.len == 0 {
		return T(0)
	}
	if sorted_data.len % 2 == 0 {
		mid := (sorted_data.len / 2) - 1
		return (sorted_data[mid] + sorted_data[mid + 1]) / T(2)
	} else {
		return sorted_data[((sorted_data.len - 1) / 2)]
	}
}

// Measure of Central Tendancy
// Mode of the given input array
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn mode<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut freqs := []int{}
	for v in data {
		freqs << freq(data, v)
	}
	mut max := 0
	for i := 0; i < freqs.len; i++ {
		if freqs[i] > freqs[max] {
			max = i
		}
	}
	return data[max]
}

// Root Mean Square of the given input array
// Based on
// https://en.wikipedia.org/wiki/Root_mean_square
pub fn rms<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := T(0)
	for v in data {
		sum += math.pow(v, 2)
	}
	return math.sqrt(sum / T(data.len))
}

// Measure of Dispersion / Spread
// Population Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
[inline]
pub fn population_variance<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	data_mean := mean<T>(data)
	return population_variance_mean<T>(data, data_mean)
}

// Measure of Dispersion / Spread
// Population Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_variance_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := T(0)
	for v in data {
		sum += (v - mean) * (v - mean)
	}
	return sum / T(data.len)
}

// Measure of Dispersion / Spread
// Sample Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
[inline]
pub fn sample_variance<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	data_mean := mean<T>(data)
	return sample_variance_mean<T>(data, data_mean)
}

// Measure of Dispersion / Spread
// Sample Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_variance_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := T(0)
	for v in data {
		sum += (v - mean) * (v - mean)
	}
	return sum / T(data.len - 1)
}

// Measure of Dispersion / Spread
// Population Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
[inline]
pub fn population_stddev<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	return math.sqrt(population_variance<T>(data))
}

// Measure of Dispersion / Spread
// Population Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
[inline]
pub fn population_stddev_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	return T(math.sqrt(f64(population_variance_mean<T>(data, mean))))
}

// Measure of Dispersion / Spread
// Sample Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
[inline]
pub fn sample_stddev<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	return T(math.sqrt(f64(sample_variance<T>(data))))
}

// Measure of Dispersion / Spread
// Sample Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
[inline]
pub fn sample_stddev_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	return T(math.sqrt(f64(sample_variance_mean<T>(data, mean))))
}

// Measure of Dispersion / Spread
// Mean Absolute Deviation of the given input array
// Based on
// https://en.wikipedia.org/wiki/Average_absolute_deviation
[inline]
pub fn absdev<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	data_mean := mean<T>(data)
	return absdev_mean<T>(data, data_mean)
}

// Measure of Dispersion / Spread
// Mean Absolute Deviation of the given input array
// Based on
// https://en.wikipedia.org/wiki/Average_absolute_deviation
pub fn absdev_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	mut sum := T(0)
	for v in data {
		sum += math.abs(v - mean)
	}
	return sum / T(data.len)
}

// Sum of squares
[inline]
pub fn tss<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	data_mean := mean<T>(data)
	return tss_mean<T>(data, data_mean)
}

// Sum of squares about the mean
pub fn tss_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	mut tss := T(0)
	for v in data {
		tss += (v - mean) * (v - mean)
	}
	return tss
}

// Minimum of the given input array
pub fn min<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut min := data[0]
	for v in data {
		if v < min {
			min = v
		}
	}
	return min
}

// Maximum of the given input array
pub fn max<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	mut max := data[0]
	for v in data {
		if v > max {
			max = v
		}
	}
	return max
}

// Minimum and maximum of the given input array
pub fn minmax<T>(data []T) (T, T) {
	if data.len == 0 {
		return T(0), T(0)
	}
	mut max := data[0]
	mut min := data[0]
	for v in data[1..] {
		if v > max {
			max = v
		}
		if v < min {
			min = v
		}
	}
	return min, max
}

// Minimum of the given input array
pub fn min_index<T>(data []T) int {
	if data.len == 0 {
		return 0
	}
	mut min := data[0]
	mut min_index := 0
	for i, v in data {
		if v < min {
			min = v
			min_index = i
		}
	}
	return min_index
}

// Maximum of the given input array
pub fn max_index<T>(data []T) int {
	if data.len == 0 {
		return 0
	}
	mut max := data[0]
	mut max_index := 0
	for i, v in data {
		if v > max {
			max = v
			max_index = i
		}
	}
	return max_index
}

// Minimum and maximum of the given input array
pub fn minmax_index<T>(data []T) (int, int) {
	if data.len == 0 {
		return 0, 0
	}
	mut min := data[0]
	mut max := data[0]
	mut min_index := 0
	mut max_index := 0
	for i, v in data {
		if v < min {
			min = v
			min_index = i
		}
		if v > max {
			max = v
			max_index = i
		}
	}
	return min_index, max_index
}

// Measure of Dispersion / Spread
// Range ( Maximum - Minimum ) of the given input array
// Based on
// https://www.mathsisfun.com/data/range.html
pub fn range<T>(data []T) T {
	if data.len == 0 {
		return T(0)
	}
	min, max := minmax<T>(data)
	return max - min
}

[inline]
pub fn covariance<T>(data1 []T, data2 []T) T {
	mean1 := mean<T>(data1)
	mean2 := mean<T>(data2)
	return covariance_mean<T>(data1, data2, mean1, mean2)
}

// Compute the covariance of a dataset using
// the recurrence relation
pub fn covariance_mean<T>(data1 []T, data2 []T, mean1 T, mean2 T) T {
	n := int(math.min(data1.len, data2.len))
	if n == 0 {
		return T(0)
	}
	mut covariance := T(0)
	for i in 0 .. n {
		delta1 := data1[i] - mean1
		delta2 := data2[i] - mean2
		covariance += (delta1 * delta2 - covariance) / (T(i) + 1.0)
	}
	return covariance
}

[inline]
pub fn lag1_autocorrelation<T>(data []T) T {
	data_mean := mean<T>(data)
	return lag1_autocorrelation_mean<T>(data, data_mean)
}

// Compute the lag-1 autocorrelation of a dataset using
// the recurrence relation
pub fn lag1_autocorrelation_mean<T>(data []T, mean T) T {
	if data.len == 0 {
		return T(0)
	}
	mut q := T(0)
	mut v := (data[0] * mean) - (data[0] * mean)
	for i := 1; i < data.len; i++ {
		delta0 := data[i - 1] - mean
		delta1 := data[i] - mean
		q += (delta0 * delta1 - q) / (T(i) + 1.0)
		v += (delta1 * delta1 - v) / (T(i) + 1.0)
	}
	return q / v
}

[inline]
pub fn kurtosis<T>(data []T) T {
	data_mean := mean<T>(data)
	sd := population_stddev_mean<T>(data, data_mean)
	return kurtosis_mean_stddev<T>(data, data_mean, sd)
}

// Takes a dataset and finds the kurtosis
// using the fourth moment the deviations, normalized by the sd
pub fn kurtosis_mean_stddev<T>(data []T, mean T, sd T) T {
	mut avg := T(0) // find the fourth moment the deviations, normalized by the sd
	/*
	we use a recurrence relation to stably update a running value so
         * there aren't any large sums that can overflow
	*/
	for i, v in data {
		x := (v - mean) / sd
		avg += (x * x * x * x - avg) / (T(i) + 1.0)
	}
	return avg - T(3.0)
}

[inline]
pub fn skew<T>(data []T) T {
	data_mean := mean<T>(data)
	sd := population_stddev_mean<T>(data, data_mean)
	return skew_mean_stddev<T>(data, data_mean, sd)
}

pub fn skew_mean_stddev<T>(data []T, mean T, sd T) T {
	mut skew := T(0) // find the sum of the cubed deviations, normalized by the sd.
	/*
	we use a recurrence relation to stably update a running value so
         * there aren't any large sums that can overflow
	*/
	for i, v in data {
		x := (v - mean) / sd
		skew += (x * x * x - skew) / (T(i) + 1.0)
	}
	return skew
}

pub fn quantile<T>(sorted_data []T, f T) T {
	if sorted_data.len == 0 {
		return T(0)
	}
	index := f * (T(sorted_data.len) - 1.0)
	lhs := int(index)
	delta := index - T(lhs)
	return if lhs == sorted_data.len - 1 {
		sorted_data[lhs]
	} else {
		(1.0 - delta) * sorted_data[lhs] + delta * sorted_data[(lhs + 1)]
	}
}
