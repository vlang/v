// NOTE: the order of these import statements is the opposite of the one in import_order_2_test.v,
// but *both* should compile and work:
import x.benchmark
import benchmark as jj

fn test_runs() {
	mut b := jj.start()
	mut action := benchmark.setup(fn () ! {
		return error('no')
	})!
	action.run()
	b.measure('nothing')
}
