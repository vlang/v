fn test_runes() {
	// Test basic ASCII + emoji
	r1 := 'hello🎅'.runes()
	assert r1.len == 6
	assert r1[0] == `h`
	assert r1[1] == `e`
	assert r1[2] == `l`
	assert r1[3] == `l`
	assert r1[4] == `o`
	assert r1[5] == `🎅`

	// Test empty string
	r2 := ''.runes()
	assert r2.len == 0

	// Test single character
	r3 := 'A'.runes()
	assert r3.len == 1
	assert r3[0] == `A`

	// Test Unicode characters
	r4 := 'café'.runes()
	assert r4.len == 4
	assert r4[0] == `c`
	assert r4[1] == `a`
	assert r4[2] == `f`
	assert r4[3] == `é`

	// Test multiple emojis
	r5 := '🎅🎄🎁'.runes()
	assert r5.len == 3
	assert r5[0] == `🎅`
	assert r5[1] == `🎄`
	assert r5[2] == `🎁`

	// Test mixed ASCII, Unicode and emojis
	r6 := 'hello世界🌍'.runes()
	assert r6.len == 8
	assert r6[0] == `h`
	assert r6[1] == `e`
	assert r6[2] == `l`
	assert r6[3] == `l`
	assert r6[4] == `o`
	assert r6[5] == `世`
	assert r6[6] == `界`
	assert r6[7] == `🌍`

	// Test special characters
	r7 := '\n\t'.runes()
	assert r7.len == 2
	assert r7[0] == `\n`
	assert r7[1] == `\t`

	// Test mathematical symbols
	r8 := 'α+β=γ'.runes()
	assert r8.len == 5
	assert r8[0] == `α`
	assert r8[1] == `+`
	assert r8[2] == `β`
	assert r8[3] == `=`
	assert r8[4] == `γ`
}

fn main() {
	test_runes()
}
