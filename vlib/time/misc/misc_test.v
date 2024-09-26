import time.misc as tmisc
import rand

fn test_random() {
	// guarantee CI test stability, by seeding the random number generator with a known seed
	rand.seed([u32(0), 0])
	t1 := tmisc.random()
	t2 := tmisc.random()
	t3 := tmisc.random()
	t4 := tmisc.random()
	assert t1.unix() != t2.unix()
	assert t1.unix() != t3.unix()
	assert t1.unix() != t4.unix()
	assert t2.unix() != t3.unix()
	assert t2.unix() != t4.unix()
	assert t3.unix() != t4.unix()
}
