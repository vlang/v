const (
	alpha = 'a'
	beta = 'b'
	m = map{
		alpha : 'Alpha'
		beta : 'Beta'
	}
)

fn test_const_keys(){
	assert m[alpha] == 'Alpha'
	assert m[beta] == 'Beta'
}
