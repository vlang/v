module fastc

// FastcFloatFormat is a float interpolation format: `${x:.1}`, `${x:8.2}`, `${x:-8.2}`,
// `${x:.2f}`.
struct FastcFloatFormat {
	width      int
	left_align bool
	precision  int
	fixed      bool
}

// fastc_float_interpolation_format reads the float formats the C backend renders through
// v3_f64_trimmed (trailing zeros removed) or, with an `f`, v3_f64_fixed: an optional `-`,
// an optional width and a `.precision`. none for anything else.
fn fastc_float_interpolation_format(spec string) ?FastcFloatFormat {
	mut s := spec
	left_align := s.starts_with('-')
	if left_align {
		s = s[1..]
	}
	fixed := s.ends_with('f')
	if fixed {
		s = s[..s.len - 1]
	}
	dot := s.index_u8(`.`)
	if dot < 0 {
		return none
	}
	width_text := s[..dot]
	precision_text := s[dot + 1..]
	if precision_text.len == 0 || (width_text.len > 1 && width_text[0] == `0`) {
		return none
	}
	for c in width_text {
		if !c.is_digit() {
			return none
		}
	}
	for c in precision_text {
		if !c.is_digit() {
			return none
		}
	}
	return FastcFloatFormat{
		width:      if width_text.len > 0 { width_text.int() } else { 0 }
		left_align: left_align
		precision:  precision_text.int()
		fixed:      fixed
	}
}

// fastc_float_format_helper emits, once per program, the C backend's float formatting for
// a self-host build and returns the name of its trimmed or fixed form.
fn (mut g Parser) fastc_float_format_helper(fixed bool) string {
	if 'v_fastc_f64_trimmed' !in g.spawn_helpers {
		g.protos.writeln('string v_fastc_f64_fixed(double x, int precision);')
		g.protos.writeln('string v_fastc_f64_trimmed(double x, int precision);')
		g.spawn_helpers['v_fastc_f64_trimmed'] = fastc_float_format_c
	}
	return if fixed { 'v_fastc_f64_fixed' } else { 'v_fastc_f64_trimmed' }
}

// The C backend's v3_f64_fixed, v3_f64_exp (with v3_float_normalize_exponent, which strips
// the third exponent digit some CRTs print) and v3_f64_trimmed, so a FastC build prints
// formatted floats exactly as the C backend does. Character codes are numeric: 45 `-`,
// 43 `+`, 46 `.`, 48 `0`, 57 `9`, 69 `E`, 101 `e`.
const fastc_float_format_c = r'double (pow)(double, double);
string v_fastc_f64_fixed(double x, int precision) {
	u64 v_bits;
	memcpy(&v_bits, &x, sizeof(v_bits));
	if (precision < 0) precision = 6;
	if (x != x || x - x != 0.0) return builtin__f64_str(x);
	int clamped_precision = precision > 35 ? 35 : precision;
	double rounder = 0.5 * pow(10.0, -clamped_precision);
	int negative = (int)(v_bits >> 63);
	string decimal = builtin__f64_str((negative ? -x : x) + rounder);
	u8 digits[32];
	int digit_count = 0;
	int decimal_pos = -1;
	int exponent = 0;
	int exponent_sign = 1;
	for (int i = 0; i < decimal.len; ++i) {
		u8 c = decimal.str[i];
		if (c >= 48 && c <= 57) {
			if (digit_count < 32) digits[digit_count++] = c;
		} else if (c == 46) {
			decimal_pos = digit_count;
		} else if (c == 101 || c == 69) {
			++i;
			if (i < decimal.len && decimal.str[i] == 45) {
				exponent_sign = -1;
				++i;
			} else if (i < decimal.len && decimal.str[i] == 43) {
				++i;
			}
			for (; i < decimal.len; ++i) exponent = exponent * 10 + decimal.str[i] - 48;
			break;
		}
	}
	if (decimal_pos < 0) decimal_pos = digit_count;
	decimal_pos += exponent_sign * exponent;
	int whole_digits = decimal_pos > 0 ? decimal_pos : 1;
	int out_len = negative + whole_digits + (precision > 0 ? precision + 1 : 0);
	u8* out = (u8*)malloc((size_t)out_len + 1);
	int pos = 0;
	if (negative) out[pos++] = 45;
	if (decimal_pos <= 0) out[pos++] = 48;
	else for (int i = 0; i < whole_digits; ++i) out[pos++] = i < digit_count ? digits[i] : 48;
	if (precision > 0) {
		out[pos++] = 46;
		for (int i = 0; i < precision; ++i) {
			int digit = decimal_pos + i;
			out[pos++] = digit >= 0 && digit < digit_count ? digits[digit] : 48;
		}
	}
	out[pos] = 0;
	return (string){.str = out, .len = out_len, .is_lit = 0};
}
static int v_fastc_float_normalize_exponent(char* text, int n) {
	int digits = n;
	while (digits > 0 && text[digits - 1] >= 48 && text[digits - 1] <= 57) --digits;
	if (digits < 2 || (text[digits - 1] != 43 && text[digits - 1] != 45) || (text[digits - 2] != 101 && text[digits - 2] != 69)) return n;
	int first = digits;
	while (n - first > 2 && text[first] == 48) ++first;
	if (first > digits) {
		memmove(text + digits, text + first, (size_t)(n - first + 1));
		n -= first - digits;
	}
	return n;
}
static string v_fastc_f64_exp(double x, int precision) {
	char tmp[128];
	int n = snprintf(tmp, sizeof(tmp), "%.*e", precision, x);
	if (n < 0) return _S("");
	u8* out = (u8*)malloc((size_t)n + 1);
	if (n < (int)sizeof(tmp)) memcpy(out, tmp, (size_t)n + 1);
	else snprintf((char*)out, (size_t)n + 1, "%.*e", precision, x);
	n = v_fastc_float_normalize_exponent((char*)out, n);
	return (string){.str = out, .len = n, .is_lit = 0};
}
string v_fastc_f64_trimmed(double x, int precision) {
	u64 v_bits;
	memcpy(&v_bits, &x, sizeof(v_bits));
	if (x == 0.0) return (v_bits >> 63) ? _S("-0") : _S("0");
	double d = x < 0 ? -x : x;
	int finite = !(x != x || x - x != 0.0);
	string s = !finite || (d < 999999.0 && d >= 0.00001) ? v_fastc_f64_fixed(x, precision) : v_fastc_f64_exp(x, precision > 0 ? precision - 1 : 0);
	if (s.is_lit) return s;
	int mant = s.len;
	int dot = -1;
	for (int i = 0; i < s.len; ++i) {
		if (s.str[i] == 46) dot = i;
		if (s.str[i] == 101) {
			mant = i;
			break;
		}
	}
	if (dot < 0) return s;
	int end = mant;
	while (end > dot + 1 && s.str[end - 1] == 48) --end;
	if (end == dot + 1) --end;
	memmove(s.str + end, s.str + mant, (size_t)(s.len - mant));
	s.len -= mant - end;
	s.str[s.len] = 0;
	return s;
}
'
