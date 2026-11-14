module math

// stirling2 returns Stirling numbers of the second kind S(n,k).
pub fn stirling2(n int, k int) i64 {
	if n == 0 && k == 0 {
		return 1
	}
	if n == 0 || k == 0 || k > n {
		return 0
	}
	// DP
	mut dp := [][]i64{len: n + 1, init: []i64{len: k + 1}}
	dp[0][0] = 1
	for i in 1 .. n + 1 {
		for j in 1 .. k + 1 {
			dp[i][j] = i64(j) * dp[i - 1][j] + dp[i - 1][j - 1]
		}
	}
	return dp[n][k]
}
