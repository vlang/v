// v doc -f html -o doc rec.v (doc should be an empty folder)
// I would like to see these 2 lines in HTML-generated header documentation
module rec

// fib Calculates the recursive fibonacci series
// `n` is the rank to pass to function
// I See these 3 lines only
pub fn fib(n int) int {
	return if n < 2 { n } else { fib(n - 1) + fib(n - 2) }
}
