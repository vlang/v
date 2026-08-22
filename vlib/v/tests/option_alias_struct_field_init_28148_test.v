// Regression test for https://github.com/vlang/v/issues/28148:
// initializing a struct field declared as `?Alias` (where `type Alias = T`)
// from an expression whose type is `?T` (e.g. a function returning `?T`)
// must compile and produce the wrapped value, both for `none` and for a
// present value. Before the fix, cgen emitted the field with the alias's
// own option C type (`_option_main__LocaleCode`) while the init expression
// had the base type's option C type (`_option_string`), causing a
// incompatible-pointer-types C error for the non-none path.

type LocaleCode28148 = string

struct LocaleContext28148 {
	locale_code ?LocaleCode28148
}

fn get_locale_or_none_28148(m map[string]string, k string) ?string {
	if k in m {
		return m[k]
	}
	return none
}

fn test_option_alias_struct_field_from_opt_string_present() {
	m := {
		'locale': 'en_US'
	}
	lctx := LocaleContext28148{
		locale_code: get_locale_or_none_28148(m, 'locale')
	}
	assert lctx.locale_code? == 'en_US'
}

fn test_option_alias_struct_field_from_opt_string_none() {
	m := map[string]string{}
	lctx := LocaleContext28148{
		locale_code: get_locale_or_none_28148(m, 'locale')
	}
	assert lctx.locale_code == none
}

fn test_option_alias_struct_field_from_plain_string() {
	lctx := LocaleContext28148{
		locale_code: 'zh_CN'
	}
	assert lctx.locale_code? == 'zh_CN'
}

fn test_option_alias_struct_field_from_alias_value() {
	code := LocaleCode28148('ja_JP')
	lctx := LocaleContext28148{
		locale_code: code
	}
	assert lctx.locale_code? == 'ja_JP'
}
