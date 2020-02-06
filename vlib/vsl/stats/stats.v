// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module stats

import vsl.math
// Measure of Occurance
// Frequency of a given number
// Based on
// https://www.mathsisfun.com/data/frequency-distribution.html
pub fn freq(data []f64, val f64) int {
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
pub fn mean(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in data {
		sum += v
	}
	return sum / f64(data.len)
}

// Measure of Central Tendancy
// Geometric Mean of the given input array
// Based on
// https://www.mathsisfun.com/numbers/geometric-mean.html
pub fn geometric_mean(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(1)
	for v in data {
		sum *= v
	}
	return math.pow(sum, f64(1) / data.len)
}

// Measure of Central Tendancy
// Harmonic Mean of the given input array
// Based on
// https://www.mathsisfun.com/numbers/harmonic-mean.html
pub fn harmonic_mean(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in data {
		sum += f64(1) / v
	}
	return f64(data.len) / sum
}

// Measure of Central Tendancy
// Median of the given input array ( input array is assumed to be sorted )
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn median_for_sorted_data(sorted_data []f64) f64 {
	if sorted_data.len == 0 {
		return f64(0)
	}
	if sorted_data.len % 2 == 0 {
		mid := (sorted_data.len / 2) - 1
		return (sorted_data[mid] + sorted_data[mid + 1]) / f64(2)
	}
	else {
		return sorted_data[((sorted_data.len - 1) / 2)]
	}
}

// Measure of Central Tendancy
// Mode of the given input array
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn mode(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut freqs := []int
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
pub fn rms(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in data {
		sum += math.pow(v, 2)
	}
	return math.sqrt(sum / f64(data.len))
}

// Measure of Dispersion / Spread
// Population Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_variance(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mean := mean(data)
	mut sum := f64(0)
	for v in data {
		sum += (v - mean) * (v - mean)
	}
	return sum / f64(data.len)
}

// Measure of Dispersion / Spread
// Population Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_variance_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in data {
		sum += (v - mean) * (v - mean)
	}
	return sum / f64(data.len)
}

// Measure of Dispersion / Spread
// Sample Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_variance(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mean := mean(data)
	mut sum := f64(0)
	for v in data {
		sum += (v - mean) * (v - mean)
	}
	return sum / f64(data.len - 1)
}

// Measure of Dispersion / Spread
// Sample Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_variance_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in data {
		sum += (v - mean) * (v - mean)
	}
	return sum / f64(data.len - 1)
}

// Measure of Dispersion / Spread
// Population Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_stddev(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	return math.sqrt(population_variance(data))
}

// Measure of Dispersion / Spread
// Population Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_stddev_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	return math.sqrt(population_variance_mean(data, mean))
}

// Measure of Dispersion / Spread
// Sample Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_stddev(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	return math.sqrt(sample_variance(data))
}

// Measure of Dispersion / Spread
// Sample Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_stddev_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	return math.sqrt(sample_variance_mean(data, mean))
}

// Measure of Dispersion / Spread
// Mean Absolute Deviation of the given input array
// Based on
// https://en.wikipedia.org/wiki/Average_absolute_deviation
pub fn absdev(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mean := mean(data)
	mut sum := f64(0)
	for v in data {
		sum += math.abs(v - mean)
	}
	return sum / f64(data.len)
}

// Measure of Dispersion / Spread
// Mean Absolute Deviation of the given input array
// Based on
// https://en.wikipedia.org/wiki/Average_absolute_deviation
pub fn absdev_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in data {
		sum += math.abs(v - mean)
	}
	return sum / f64(data.len)
}

// Sum of squares
pub fn tss(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mean := mean(data)
	mut tss := f64(0)
	for v in data {
		tss += (v - mean) * (v - mean)
	}
	return tss
}

// Sum of squares about the mean
pub fn tss_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut tss := f64(0)
	for v in data {
		tss += (v - mean) * (v - mean)
	}
	return tss
}

// Minimum of the given input array
pub fn min(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
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
pub fn max(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
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
pub fn minmax(data []f64) (f64,f64) {
	if data.len == 0 {
		return f64(0),f64(0)
	}
	mut max := data[0]
	mut min := data[0]
	for v in data {
		if v > max {
			max = v
		}
		if v < min {
			min = v
		}
	}
	return min,max
}

// Minimum of the given input array
pub fn min_index(data []f64) int {
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
pub fn max_index(data []f64) int {
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
pub fn minmax_index(data []f64) (int,int) {
	if data.len == 0 {
		return 0,0
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
	return min_index,max_index
}

// Measure of Dispersion / Spread
// Range ( Maximum - Minimum ) of the given input array
// Based on
// https://www.mathsisfun.com/data/range.html
pub fn range(data []f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	return max(data) - min(data)
}

[inline]
pub fn covariance(data1 []f64, data2 []f64) f64 {
	mean1 := mean(data1)
	mean2 := mean(data2)
	return covariance_mean(data1, data2, mean1, mean2)
}

// Compute the covariance of a dataset using
// the recurrence relation
pub fn covariance_mean(data1 []f64, data2 []f64, mean1 f64, mean2 f64) f64 {
	n := int(math.min(data1.len, data2.len))
	if n == 0 {
		return f64(0)
	}
	mut covariance := f64(0)
	for i := 0; i < n; i++ {
		delta1 := data1[i] - mean1
		delta2 := data2[i] - mean2
		covariance += (delta1 * delta2 - covariance) / (f64(i) + 1.0)
	}
	return covariance
}

[inline]
pub fn lag1_autocorrelation(data []f64) f64 {
	mean := mean(data)
	return lag1_autocorrelation_mean(data, mean)
}

// Compute the lag-1 autocorrelation of a dataset using
// the recurrence relation
pub fn lag1_autocorrelation_mean(data []f64, mean f64) f64 {
	if data.len == 0 {
		return f64(0)
	}
	mut q := f64(0)
	mut v := (data[0] * mean) - (data[0] * mean)
	for i := 1; i < data.len; i++ {
		delta0 := data[i - 1] - mean
		delta1 := data[i] - mean
		q += (delta0 * delta1 - q) / (f64(i) + 1.0)
		v += (delta1 * delta1 - v) / (f64(i) + 1.0)
	}
	return q / v
}

[inline]
pub fn kurtosis(data []f64) f64 {
	mean := mean(data)
	sd := population_stddev_mean(data, mean)
	return kurtosis_mean_stddev(data, mean, sd)
}

// Takes a dataset and finds the kurtosis
// using the fourth moment the deviations, normalized by the sd
pub fn kurtosis_mean_stddev(data []f64, mean f64, sd f64) f64 {
	mut avg := f64(0)
	/* find the fourth moment the deviations, normalized by the sd */

	/* we use a recurrence relation to stably update a running value so
         * there aren't any large sums that can overflow
         */

	for i, v in data {
		x := (v - mean) / sd
		avg += (x * x * x * x - avg) / (f64(i) + 1.0)
	}
	return avg - f64(3.0)
}

[inline]
pub fn skew(data []f64) f64 {
	mean := mean(data)
	sd := population_stddev_mean(data, mean)
	return skew_mean_stddev(data, mean, sd)
}

pub fn skew_mean_stddev(data []f64, mean f64, sd f64) f64 {
	mut skew := f64(0)
	/* find the sum of the cubed deviations, normalized by the sd. */

	/* we use a recurrence relation to stably update a running value so
         * there aren't any large sums that can overflow
         */

	for i, v in data {
		x := (v - mean) / sd
		skew += (x * x * x - skew) / (f64(i) + 1.0)
	}
	return skew
}

pub fn quantile_for_sorted_data(sorted_data []f64, f f64) f64 {
	if sorted_data.len == 0 {
		return f64(0)
	}
	index := f * (f64(sorted_data.len) - 1.0)
	lhs := int(index)
	delta := index - f64(lhs)
	return if lhs == sorted_data.len - 1 { sorted_data[lhs] } else { (f64(1.0) - delta) * sorted_data[lhs] + delta * sorted_data[(lhs + 1)] }
}
