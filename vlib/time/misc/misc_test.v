import time.misc as tmisc
import rand.wyrand

fn test_random() {
	// guarantee CI test stability, by seeding the random number generator with a known seed
	mut rng := wyrand.WyRandRNG{}
	rng.seed([]u32{len: 2})
	t1 := tmisc.random(mut rng)
	t2 := tmisc.random(mut rng)
	t3 := tmisc.random(mut rng)
	t4 := tmisc.random(mut rng)
	assert t1.unix != t2.unix
	assert t1.unix != t3.unix
	assert t1.unix != t4.unix
	assert t2.unix != t3.unix
	assert t2.unix != t4.unix
	assert t3.unix != t4.unix
}
