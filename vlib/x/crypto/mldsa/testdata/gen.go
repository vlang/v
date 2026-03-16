// generates ML-DSA test vectors using Go's crypto/internal/fips140/mldsa
// run from within the Go source tree because mldsa is not public yet
// see https://github.com/golang/go/issues/77626
//
// this is normally ran via gen.vsh
//
// GOROOT=. ./bin/go run ./src/crypto/internal/fips140/mldsa/gen
package main

import (
	"crypto/internal/fips140/mldsa"
	"crypto/internal/fips140/sha3"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
)

type Vector struct {
	Kind      string `json:"kind"`
	Seed      string `json:"seed"`
	Msg       string `json:"msg"`
	PkSha256  string `json:"pk_sha256"`
	SigSha256 string `json:"sig_sha256"`
	Context   string `json:"context,omitempty"`
}

type variant struct {
	name          string
	newPrivateKey func([]byte) (*mldsa.PrivateKey, error)
	newPublicKey  func([]byte) (*mldsa.PublicKey, error)
}

func main() {
	variants := []variant{
		{"ml_dsa_44", mldsa.NewPrivateKey44, mldsa.NewPublicKey44},
		{"ml_dsa_65", mldsa.NewPrivateKey65, mldsa.NewPublicKey65},
		{"ml_dsa_87", mldsa.NewPrivateKey87, mldsa.NewPublicKey87},
	}

	s := sha3.NewShake128()
	seed := make([]byte, 32)
	var vectors []Vector

	for _, v := range variants {
		for i := 0; i < 3; i++ {
			s.Read(seed)
			priv, err := v.newPrivateKey(seed)
			if err != nil {
				panic(err)
			}
			pk := priv.PublicKey().Bytes()

			msg := make([]byte, 32+i*17)
			s.Read(msg)

			sig, err := mldsa.SignDeterministic(priv, msg, "")
			if err != nil {
				panic(err)
			}

			pub, err := v.newPublicKey(pk)
			if err != nil {
				panic(err)
			}
			if err := mldsa.Verify(pub, msg, sig, ""); err != nil {
				panic(fmt.Sprintf("verify failed: %v", err))
			}

			pkHash := sha256.Sum256(pk)
			sigHash := sha256.Sum256(sig)
			vectors = append(vectors, Vector{
				Kind:      v.name,
				Seed:      hex.EncodeToString(seed),
				Msg:       hex.EncodeToString(msg),
				PkSha256:  hex.EncodeToString(pkHash[:]),
				SigSha256: hex.EncodeToString(sigHash[:]),
			})
		}

		s.Read(seed)
		priv, err := v.newPrivateKey(seed)
		if err != nil {
			panic(err)
		}
		pk := priv.PublicKey().Bytes()
		msg := make([]byte, 40)
		s.Read(msg)

		sig, err := mldsa.SignDeterministic(priv, msg, "test-context")
		if err != nil {
			panic(err)
		}
		pub, err := v.newPublicKey(pk)
		if err != nil {
			panic(err)
		}
		if err := mldsa.Verify(pub, msg, sig, "test-context"); err != nil {
			panic(fmt.Sprintf("verify with context failed: %v", err))
		}

		pkHash := sha256.Sum256(pk)
		sigHash := sha256.Sum256(sig)
		vectors = append(vectors, Vector{
			Kind:      v.name,
			Seed:      hex.EncodeToString(seed),
			Msg:       hex.EncodeToString(msg),
			PkSha256:  hex.EncodeToString(pkHash[:]),
			SigSha256: hex.EncodeToString(sigHash[:]),
			Context:   "test-context",
		})
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(vectors); err != nil {
		panic(err)
	}
}
