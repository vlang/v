/*
   Copyright (c) 2015, Andreas Fett
   All rights reserved.
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.
   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <assert.h>

typedef int utf8_state;

static utf8_state next_state(utf8_state, unsigned char);

// Public API see utf8-validate.h for docs of the following function

bool utf8_validate(utf8_state *const state, int c)
{
	assert(state);
	return (*state = next_state(*state, c)) != -1;
}

bool utf8_validate_some(utf8_state *const state, const void * const src, size_t len)
{
	assert(state);
	assert(src);
	for (size_t i = 0; i < len; ++i) {
		*state = next_state(*state, *((unsigned char *)src + i));
		if (*state == -1) {
			return false;
		}
	}
	return true;
}

bool utf8_validate_mem(const void * const src, size_t len)
{
	assert(src);
	utf8_state state = 0;
	for (size_t i = 0; i < len; ++i) {
		state = next_state(state, *((unsigned char *)src + i));
		if (state == -1) {
			return false;
		}
	}

	// detect unterminated sequence
	return state == 0;
}

bool utf8_validate_str(const char * const str)
{
	assert(str);
	utf8_state state = 0;
	for (size_t i = 0; str[i] != 0; ++i) {
		state = next_state(state, str[i]);
		if (state == -1) {
			return false;
		}
	}
	// detect unterminated sequence
	return state == 0;
}

/* Private state engine
 *
 * The macros below assemble the cases for a switch statement
 * matching the language of the ABNF grammar given in rfc3629.
 *
 * Each SEQ# macro adds the states to match a # char long sequence.
 *
 * The SEQ#_HELPERs all have a 'fall through' to the next sequence.
 * for # > 1 this is an explicit goto
 */

#define SEQ_END(n) SEQ_ ## n ## _END

#define SEQ1_HELPER(s, r0)                                     \
case (s * 4) + 0: if (r0) return 0; goto SEQ_END(s);           \
SEQ_END(s):

#define SEQ2_HELPER(s, r0, r1)                                 \
case (s * 4) + 0: if (r0) { printf("ehe"); return (s * 4) + 1; } goto SEQ_END(s); \
case (s * 4) + 1: if (r1) return 0;           return -1;       \
SEQ_END(s):

#define SEQ3_HELPER(s, r0, r1, r2)                             \
case (s * 4) + 0: if (r0) return (s * 4) + 1; goto SEQ_END(s); \
case (s * 4) + 1: if (r1) return (s * 4) + 2; return -1;       \
case (s * 4) + 2: if (r2) return 0;           return -1;       \
SEQ_END(s):

#define SEQ4_HELPER(s, r0, r1, r2, r3)                         \
case (s * 4) + 0: if (r0) return (s * 4) + 1; goto SEQ_END(s); \
case (s * 4) + 1: if (r1) return (s * 4) + 2; return -1;       \
case (s * 4) + 2: if (r2) return (s * 4) + 3; return -1;       \
case (s * 4) + 3: if (r3) return 0;           return -1;       \
SEQ_END(s):

#define SEQ1(s, r0)             SEQ1_HELPER(s, r0)
#define SEQ2(s, r0, r1)         SEQ2_HELPER(s, r0, r1)
#define SEQ3(s, r0, r1, r2)     SEQ3_HELPER(s, r0, r1, r2)
#define SEQ4(s, r0, r1, r2, r3) SEQ4_HELPER(s, r0, r1, r2, r3)

// Matcher macros

#define VALUE(v)     (c == v)
#define RANGE(s, e)  (c >= s && c <= e)
/* workaround for "-Wtype-limits" as c >= s is allways true for
 * the unsigned char in the case of c == 0 */
#define EGNAR(s, e) ((c >= s + 1 && c <= e) || c == s)

/* from rfc3629
 *
 * UTF8-octets = *( UTF8-char )
 *    UTF8-char   = UTF8-1 / UTF8-2 / UTF8-3 / UTF8-4
 *    UTF8-1      = %x00-7F
 *    UTF8-2      = %xC2-DF UTF8-tail
 *    UTF8-3      = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
 *                  %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
 *    UTF8-4      = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
 *                  %xF4 %x80-8F 2( UTF8-tail )
 *    UTF8-tail   = %x80-BF
 */

#define TAIL RANGE(0x80, 0xBF)

static inline utf8_state next_state(utf8_state state, unsigned char c)
{
	printf("C: %d\n", c);
	switch (state) {
	SEQ1(0, EGNAR(0x00, 0x7F))
	SEQ2(1, RANGE(0xC2, 0xDF), TAIL)
	SEQ3(2, VALUE(0xE0),       RANGE(0xA0, 0xBF), TAIL)
	SEQ3(3, RANGE(0xE1, 0xEC), TAIL,              TAIL)
	SEQ3(4, VALUE(0xED),       RANGE(0x80, 0x9F), TAIL)
	SEQ3(5, RANGE(0xEE, 0xEF), TAIL,              TAIL)
	SEQ4(6, VALUE(0xF0),       RANGE(0x90, 0xBF), TAIL, TAIL)
	SEQ4(7, RANGE(0xF1, 0xF3), TAIL,              TAIL, TAIL)
	SEQ4(8, VALUE(0xF4),       RANGE(0x80, 0x8F), TAIL, TAIL)
		// no sequence start matched
		break;
	default:
		/*
		 * This should not happen, unless you feed an error
		 * state or an uninitialized utf8_state to this function.
		 */
		assert(false && "invalid utf8 state");
	}

	return -1;
}