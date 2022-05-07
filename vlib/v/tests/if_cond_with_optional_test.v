module main

import rand

interface Sample {
mut:
	get_next() int
}

struct SampleA {
mut:
	state int
}

fn (mut sample SampleA) get_next() int {
	sample.state++
	return sample.state
}

struct SampleB {
mut:
	state int = 1
}

fn (mut sample SampleB) get_next() int {
	sample.state += 2
	return sample.state
}

fn create_sampler() Sample {
	return if rand.intn(1) or { 0 } == 0 { Sample(SampleA{}) } else { Sample(SampleB{}) }
}

fn test_if_cond_with_optional() {
	mut sample := create_sampler()
	mut ret := sample.get_next()
	println(ret)
	assert ret == 1

	ret = sample.get_next()
	println(ret)
	assert ret == 2
}
