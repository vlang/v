import time.misc as tmisc
import rand

fn test_random() {
	// guarantee CI test stability, by seeding the random number generator with a known seed
	rand.seed([u32(0), 0])
	t1 := tmisc.random()
	t2 := tmisc.random()
	t3 := tmisc.random()
	t4 := tmisc.random()
	assert t1.unix_time() != t2.unix_time()
	assert t1.unix_time() != t3.unix_time()
	assert t1.unix_time() != t4.unix_time()
	assert t2.unix_time() != t3.unix_time()
	assert t2.unix_time() != t4.unix_time()
	assert t3.unix_time() != t4.unix_time()
}
