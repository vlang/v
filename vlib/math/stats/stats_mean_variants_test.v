import math
import math.stats

const data = [f64(1.0), 2.0, 3.0, 4.0, 5.0]

fn test_variance_mean_variants_match_the_short_forms() {
	m := stats.mean[f64](data)
	assert stats.population_variance_mean[f64](data, m) == stats.population_variance[f64](data)
	assert stats.sample_variance_mean[f64](data, m) == stats.sample_variance[f64](data)
}

fn test_stddev_mean_variants_match_the_short_forms() {
	m := stats.mean[f64](data)
	assert stats.population_stddev_mean[f64](data, m) == stats.population_stddev[f64](data)
	assert stats.sample_stddev_mean[f64](data, m) == stats.sample_stddev[f64](data)
}

fn test_absdev_and_tss_mean_variants_match_the_short_forms() {
	m := stats.mean[f64](data)
	assert stats.absdev_mean[f64](data, m) == stats.absdev[f64](data)
	assert stats.tss_mean[f64](data, m) == stats.tss[f64](data)
}

fn test_covariance_mean_variant_matches_the_short_form() {
	xs := [f64(1.0), 2.0, 3.0]
	ys := [f64(2.0), 4.0, 6.0]
	assert stats.covariance_mean[f64](xs, ys, stats.mean[f64](xs), stats.mean[f64](ys)) == stats.covariance[f64](xs, ys)
}

fn test_lag1_autocorrelation_mean_variant_matches_the_short_form() {
	m := stats.mean[f64](data)
	assert stats.lag1_autocorrelation_mean[f64](data, m) == stats.lag1_autocorrelation[f64](data)
}

fn test_kurtosis_and_skew_mean_stddev_variants_match_the_short_forms() {
	m := stats.mean[f64](data)
	sd := stats.population_stddev[f64](data)
	assert stats.kurtosis_mean_stddev[f64](data, m, sd) == stats.kurtosis[f64](data)
	assert stats.skew_mean_stddev[f64](data, m, sd) == stats.skew[f64](data)
}

fn test_mean_variants_reference_values() {
	m := stats.mean[f64](data)
	assert m == 3.0
	assert stats.population_variance[f64](data) == 2.0
	assert stats.sample_variance[f64](data) == 2.5
	assert stats.population_stddev[f64](data) == math.sqrt(2.0)
	assert stats.sample_stddev[f64](data) == 1.5811388300841898
	assert stats.absdev[f64](data) == 1.2
	assert stats.tss[f64](data) == 10.0
}

fn test_mean_variants_sample_is_n_over_n_minus_one_times_population() {
	n := f64(data.len)
	pv := stats.population_variance[f64](data)
	assert math.abs(stats.sample_variance[f64](data) - pv * n / (n - 1)) < 1e-12
	assert math.abs(stats.sample_stddev[f64](data) - math.sqrt(pv * n / (n - 1))) < 1e-12
}

fn test_mean_variants_on_empty_input() {
	empty := []f64{}
	assert stats.population_variance_mean[f64](empty, 0.0) == 0.0
	assert stats.sample_variance_mean[f64](empty, 0.0) == 0.0
	assert stats.population_stddev_mean[f64](empty, 0.0) == 0.0
	assert stats.sample_stddev_mean[f64](empty, 0.0) == 0.0
	assert stats.absdev_mean[f64](empty, 0.0) == 0.0
	assert stats.tss_mean[f64](empty, 0.0) == 0.0
}

// NOTE: sample variance divides by n-1, so a single sample gives 0/0 == nan.
// This is the current behaviour, asserted rather than corrected.
fn test_mean_variants_single_sample_is_nan() {
	assert stats.sample_variance_mean[f64]([f64(7.0)], 7.0) != stats.sample_variance_mean[f64]([f64(7.0)],
		7.0)
	assert stats.population_variance_mean[f64]([f64(7.0)], 7.0) == 0.0
}
