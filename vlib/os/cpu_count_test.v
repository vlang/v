import os

fn test_cpu_count_sane() {
	n := os.cpu_count()
	assert n >= 1
}
