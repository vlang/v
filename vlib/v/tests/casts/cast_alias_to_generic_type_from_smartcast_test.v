type ParseRes = Result[[]Token, ParseErr]

struct Token {}

struct ParseErr {}

type Opt[T] = None[T] | Some[T]

struct None[T] {}

struct Some[T] {
	value T
}

type Result[T, U] = Err[U] | Ok[T]

struct Ok[T] {
	value T
}

struct Err[U] {
	value U
}

fn test_cast_alias_to_generic_type_from_smartcasted_wrapper() {
	r := Opt[ParseRes](Some[ParseRes]{
		value: ParseRes(Ok[[]Token]{
			value: [Token{}]
		})
	})
	match r {
		Some[ParseRes] {
			rx := Result[[]Token, ParseErr](r)
			match rx {
				Ok[[]Token] {
					assert rx.value.len == 1
				}
				Err[ParseErr] {
					assert false
				}
			}
		}
		None[ParseRes] {
			assert false
		}
	}
}
