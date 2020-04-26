
import rand
import time

fn show_u32s(a []u32){
   mut res := []string{}
   for x in a {
	  res << x.str()
   }
   print('[')
   print(res.join(', '))
   println(']')
}
fn gen_randoms(initstate i64, initseq i64, bound int) []u32 {
	mut randoms := [u32(0)].repeat(20)
	mut rnd := rand.new_pcg32( u64(initstate), u64(initseq) )
	for i in 0..20 {
		randoms[i] = rnd.bounded_next(u32(bound))
	}
	return randoms
}

fn test_pcg32_reproducibility() {
	t := time.ticks()
	tseq := t % 23237671
	println('t: $t | tseq: $tseq')
	randoms1 := gen_randoms(t, tseq, 1000)
	randoms2 := gen_randoms(t, tseq, 1000)
	assert randoms1.len == randoms2.len
	show_u32s(randoms1)
	show_u32s(randoms2)
	len := randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}
}
