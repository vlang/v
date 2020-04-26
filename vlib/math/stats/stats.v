module stats

import math

// TODO: Implement all of them with generics

// This module defines the following statistical operations on f64 array
//  ---------------------------
// |   Summary of Functions    |
//  ---------------------------
// -----------------------------------------------------------------------
// freq - Frequency
// mean - Mean
// geometric_mean - Geometric Mean
// harmonic_mean - Harmonic Mean
// median - Median
// mode - Mode
// rms - Root Mean Square
// population_variance - Population Variance
// sample_variance - Sample Variance
// population_stddev - Population Standard Deviation
// sample_stddev - Sample Standard Deviation
// mean_absdev - Mean Absolute Deviation
// min - Minimum of the Array
// max - Maximum of the Array
// range - Range of the Array ( max - min )
// -----------------------------------------------------------------------


// Measure of Occurance
// Frequency of a given number
// Based on
// https://www.mathsisfun.com/data/frequency-distribution.html
pub fn freq(arr []f64, val f64) int {
	if arr.len == 0 {
		return 0
	}
	mut count := 0
	for v in arr {
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
pub fn mean(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in arr {
		sum += v
	}
	return sum/f64(arr.len)
}

// Measure of Central Tendancy
// Geometric Mean of the given input array
// Based on
// https://www.mathsisfun.com/numbers/geometric-mean.html
pub fn geometric_mean(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut sum := f64(1)
	for v in arr {
		sum *= v
	}
	return math.pow(sum,f64(1)/arr.len)
}

// Measure of Central Tendancy
// Harmonic Mean of the given input array
// Based on
// https://www.mathsisfun.com/numbers/harmonic-mean.html
pub fn harmonic_mean(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in arr {
		sum += f64(1)/v
	}
	return f64(arr.len)/sum
}

// Measure of Central Tendancy
// Median of the given input array ( input array is assumed to be sorted )
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn median(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	if arr.len % 2 == 0 {
		mid := (arr.len/2)-1
		return (arr[mid] + arr[mid+1])/f64(2)
	}
	else {
		return arr[((arr.len-1)/2)]
	}
}

// Measure of Central Tendancy
// Mode of the given input array
// Based on
// https://www.mathsisfun.com/data/central-measures.html
pub fn mode(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut freqs := []int{}
	for v in arr {
		freqs<<freq(arr,v)
	}
	mut max := 0
	for i in 0..freqs.len {
		if freqs[i] > freqs[max] {
			max = i
		}
	}
	return arr[max]
}

// Root Mean Square of the given input array
// Based on
// https://en.wikipedia.org/wiki/Root_mean_square
pub fn rms(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut sum := f64(0)
	for v in arr {
		sum += math.pow(v,2)
	}
	return math.sqrt(sum/f64(arr.len))
}

// Measure of Dispersion / Spread
// Population Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_variance(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	m := mean(arr)
	mut sum := f64(0)
	for v in arr {
		sum += math.pow(v-m,2)
	}
	return sum/f64(arr.len)
}

// Measure of Dispersion / Spread
// Sample Variance of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_variance(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	m := mean(arr)
	mut sum := f64(0)
	for v in arr {
		sum += math.pow(v-m,2)
	}
	return sum/f64(arr.len-1)
}

// Measure of Dispersion / Spread
// Population Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn population_stddev(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	return math.sqrt(population_variance(arr))
}

// Measure of Dispersion / Spread
// Sample Standard Deviation of the given input array
// Based on
// https://www.mathsisfun.com/data/standard-deviation.html
pub fn sample_stddev(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	return math.sqrt(sample_variance(arr))
}

// Measure of Dispersion / Spread
// Mean Absolute Deviation of the given input array
// Based on
// https://en.wikipedia.org/wiki/Average_absolute_deviation
pub fn mean_absdev(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mean := mean(arr)
	mut sum := f64(0)
	for v in arr {
		sum += math.abs(v-mean)
	}
	return sum/f64(arr.len)
}

// Minimum of the given input array
pub fn min(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut min := arr[0]
	for v in arr {
		if v < min {
			min = v
		}
	}
	return min
}

// Maximum of the given input array
pub fn max(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	mut max := arr[0]
	for v in arr {
		if v > max {
			max = v
		}
	}
	return max
}

// Measure of Dispersion / Spread
// Range ( Maximum - Minimum ) of the given input array
// Based on
// https://www.mathsisfun.com/data/range.html
pub fn range(arr []f64) f64 {
	if arr.len == 0 {
		return f64(0)
	}
	return max(arr) - min(arr)
}
