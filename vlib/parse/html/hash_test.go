package html

import (
	"bytes"
	"testing"

	"github.com/tdewolff/test"
)

func TestHashTable(t *testing.T) {
	test.T(t, ToHash([]byte("address")), Address, "'address' must resolve to Address")
	test.T(t, Address.String(), "address")
	test.T(t, Accept_Charset.String(), "accept-charset")
	test.T(t, ToHash([]byte("")), Hash(0), "empty string must resolve to zero")
	test.T(t, Hash(0xffffff).String(), "")
	test.T(t, ToHash([]byte("iter")), Hash(0), "'iter' must resolve to zero")
	test.T(t, ToHash([]byte("test")), Hash(0), "'test' must resolve to zero")
}

////////////////////////////////////////////////////////////////

var result int

// naive scenario
func BenchmarkCompareBytes(b *testing.B) {
	var r int
	val := []byte("span")
	for n := 0; n < b.N; n++ {
		if bytes.Equal(val, []byte("span")) {
			r++
		}
	}
	result = r
}

// using-atoms scenario
func BenchmarkFindAndCompareAtom(b *testing.B) {
	var r int
	val := []byte("span")
	for n := 0; n < b.N; n++ {
		if ToHash(val) == Span {
			r++
		}
	}
	result = r
}

// using-atoms worst-case scenario
func BenchmarkFindAtomCompareBytes(b *testing.B) {
	var r int
	val := []byte("zzzz")
	for n := 0; n < b.N; n++ {
		if h := ToHash(val); h == 0 && bytes.Equal(val, []byte("zzzz")) {
			r++
		}
	}
	result = r
}
