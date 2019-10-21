import os

struct Fib {
        nums []int
}

fn (fib mut Fib) calc(n int) int {
        if n <= 1 {
                return n
        }
        if fib.nums[n] != 0 {
                return fib.nums[n]
        }
        fib.nums[n] = fib.calc(n - 1) + fib.calc(n - 2)
        return fib.nums[n]
}

fn main() {
		if os.args.len != 2 {
			println('usage: fibonacci [rank]')

			//Exit
			return
		}

		max := os.args[1].int()

        mut fib := Fib {
                nums: [0].repeat(max)
        }
        for i := 0; i < max; i++ {
                println(fib.calc(i))
        }
}
