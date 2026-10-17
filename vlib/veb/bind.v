module veb

import strconv
import json2 as json

// bind decodes the request into a struct of type `T` and validates it,
// returning the filled value. A JSON body (by content type) decodes into the
// full type, nested values included; anything else fills the scalar `string`,
// `int` and `bool` fields from the query and the form, form winning on
// conflict. A body that cannot fill the type, and a value that breaks a rule,
// is an error naming the field — never a silently coerced zero.
//
// Validation reads struct attributes. Only this vocabulary is understood, and
// everything else — `sql:`, `json:` and whatever other layers annotate the
// same struct with — is ignored, so one struct can serve the table, the wire
// and the form at once:
//   - `required`: the input must contain the key, and a string must be
//     non-empty.
//   - `min: N` / `max: N`: string length bounds, integer value bounds.
//   - `email`: the string looks like `local@domain.tld`.
// An attribute outside this vocabulary that *looks* like validation is still
// ignored; misspelling a rule fails safe in the other direction, toward an
// unchecked field rather than a rejected request, so spell them right.
//
// ```v
// struct SignupInput {
//     name     string @[required; max: 64]
//     email    string @[required; email]
//     password string @[required; min: 12]
//     age      int    @[min: 13]
// }
//
// @['/signup'; post]
// pub fn (app &App) signup(mut ctx Context) veb.Result {
//     input := ctx.bind[SignupInput]() or {
//         return ctx.request_error(err.msg())
//     }
//     return ctx.json(input)
// }
// ```
pub fn (ctx &Context) bind[T]() !T {
	if ctx.is_json_request() {
		return ctx.bind_json[T]()
	}
	return ctx.bind_map[T]()
}

// is_json_request reports whether the request carries a JSON body.
fn (ctx &Context) is_json_request() bool {
	if ctx.req.data.len == 0 {
		return false
	}
	content_type := ctx.req.header.get(.content_type) or { return false }
	return content_type.contains('application/json')
}

// bind_json decodes a JSON object body into `T`, checking `required` against
// the keys the body actually contains.
fn (ctx &Context) bind_json[T]() !T {
	keys := json.decode[map[string]json.Any](ctx.req.data) or {
		return error('invalid JSON body: ${err.msg()}')
	}
	mut res := json.decode[T](ctx.req.data) or {
		return error('JSON body does not fit: ${err.msg()}')
	}
	$for field in T.fields {
		if bind_has_attr(field.attrs, 'required') {
			key := bind_json_key(field.name, field.attrs)
			if key !in keys {
				return error('missing required field "${field.name}"')
			}
		}
	}
	validate_bound_value[T](res)!
	return res
}

// bind_map fills the scalar fields of `T` from the query and the form, then
// validates the result.
fn (ctx &Context) bind_map[T]() !T {
	mut data := ctx.query.clone()
	for key, value in ctx.form {
		data[key] = value
	}
	mut res := T{}
	$for field in T.fields {
		value := data[field.name] or { '' }
		present := field.name in data
		$if field.typ is string {
			if value != '' || present {
				res.$(field.name) = value
			}
		} $else $if field.typ is int {
			if present {
				res.$(field.name) = strconv.atoi(value) or {
					return error('invalid integer value "${value}" for field "${field.name}"')
				}
			}
		} $else $if field.typ is bool {
			if present {
				if value != 'true' && value != 'false' {
					return error('invalid boolean value "${value}" for field "${field.name}": expected true or false')
				}
				res.$(field.name) = value == 'true'
			}
		}
		if bind_has_attr(field.attrs, 'required') {
			if !present {
				return error('missing required field "${field.name}"')
			}
		}
	}
	validate_bound_value[T](res)!
	return res
}

// validate_bound_value checks the value rules (`required`, `min`, `max`,
// `email`) of every field against its attributes.
fn validate_bound_value[T](res T) ! {
	$for field in T.fields {
		$if field.typ is string {
			value := res.$(field.name)
			if bind_has_attr(field.attrs, 'required') && value == '' {
				return error('required field "${field.name}" must not be empty')
			}
			if min := bind_int_attr(field.attrs, 'min') {
				if value.len < min {
					return error('field "${field.name}" is shorter than ${min} characters')
				}
			}
			if max := bind_int_attr(field.attrs, 'max') {
				if value.len > max {
					return error('field "${field.name}" is longer than ${max} characters')
				}
			}
			if bind_has_attr(field.attrs, 'email') && value != '' {
				if !is_email_like(value) {
					return error('field "${field.name}" is not a valid email address')
				}
			}
		} $else $if field.typ is int {
			value := res.$(field.name)
			if min := bind_int_attr(field.attrs, 'min') {
				if value < min {
					return error('field "${field.name}" is smaller than ${min}')
				}
			}
			if max := bind_int_attr(field.attrs, 'max') {
				if value > max {
					return error('field "${field.name}" is larger than ${max}')
				}
			}
		}
	}
}

// is_email_like is a syntactic check — one `@`, a dot after it, no spaces —
// not a proof of deliverability.
fn is_email_like(s string) bool {
	parts := s.split('@')
	if parts.len != 2 || parts[0] == '' || parts[1] == '' {
		return false
	}
	if s.contains(' ') {
		return false
	}
	dot := parts[1].last_index('.') or { return false }
	return dot > 0 && dot < parts[1].len - 1
}

// bind_json_key resolves the serialized JSON key for a struct field, using
// the same first `json:` attribute the json2 decoder uses. A missing
// attribute, an empty value, or a `skip` marker (`json: '-'`) keeps the V
// field name, so `required` is checked against the key the decoder fills.
fn bind_json_key(field_name string, attrs []string) string {
	for attr in attrs {
		if attr.starts_with('json:') {
			mut val := attr[5..].trim_space()
			if val.len > 1 {
				if (val[0] == `'` && val[val.len - 1] == `'`)
					|| (val[0] == `"` && val[val.len - 1] == `"`) {
					val = val[1..val.len - 1]
				}
			}
			if val == '' {
				continue
			}
			if val == '-' {
				return field_name
			}
			return val
		}
	}
	return field_name
}

// bind_has_attr reports whether an attribute list holds `name`, either bare
// (`required`) or valued (`min: 12`). Only the validation vocabulary is ever
// asked about; attributes from other layers pass through untouched.
fn bind_has_attr(attrs []string, name string) bool {
	for attr in attrs {
		if attr == name || attr.starts_with(name + ':') {
			return true
		}
	}
	return false
}

// bind_int_attr reads a valued attribute like `min: 12`, or returns `none`
// when it is absent or unparsable.
fn bind_int_attr(attrs []string, name string) ?int {
	for attr in attrs {
		if attr.starts_with(name + ':') {
			return strconv.atoi(attr.all_after(':').trim_space()) or { none }
		}
	}
	return none
}
