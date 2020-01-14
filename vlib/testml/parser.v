module testml

struct Parser{
mut:
	token 		[]Token
	is_function bool
}