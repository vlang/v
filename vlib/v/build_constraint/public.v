module build_constraint

// Environment represents the current build environment.
@[heap]
pub struct Environment {
pub mut:
	facts   map[string]bool
	defines map[string]bool
}

// new_environment creates a new Environment.
// `facts` is a list of predefined platforms, compilers, build options etc, for example: ['linux', 'tinyc', 'prod', 'amd64']
// `defines` is a list of the user defines, for example: ['abc', 'gcboehm_opt', 'gg_record', 'show_fps']
pub fn new_environment(facts []string, defines []string) &Environment {
	mut b := &Environment{}
	b.facts['true'] = true
	for f in facts {
		b.facts[f] = true
	}
	for d in defines {
		b.defines[d] = true
	}
	return b
}

// eval evaluates the given build `constraint` against the current environment.
// The constraint can be for example something simple like just `linux`,
// but it can be also a more complex logic expression like: `(windows && tinyc) || prod`
pub fn (b &Environment) eval(constraint string) !bool {
	mut parser := BParser{
		tokens: lex(constraint)!
	}
	expr := parser.parse()!
	return expr.eval(b)
}

// is_fact checks whether the given `fact` is present in the environment.
pub fn (b &Environment) is_fact(fact string) bool {
	return fact in b.facts
}

// is_define checks whether the given `define` is present in the environment.
pub fn (b &Environment) is_define(define string) bool {
	return define in b.defines
}
