import rand

fn main() {
	n := rand.intn(1000) or { 0 }
	println(n)
	u := rand.uuid_v4()
	println(u)
}
