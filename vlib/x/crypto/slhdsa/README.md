# slhdsa
Experimental module of NIST FIPS-205 Stateless Hash-Based Digital Signature Standard (SLH-DSA) in V

## About
SLH-DSA was a quantum resistent cryptographic digital signature standard that was approved and publicly published by NIST at August, 2024. Its availables on [NIST FIPS 205](https://csrc.nist.gov/pubs/fips/205/final). <br>
SLH-DSA allow builds relatively big signaturue size with small key (`16 - 32` bytes key).
The signatures range from `±8K - ±50K` depending on the type chosen.

> [!NOTE]  
> This module wraps and written on top of SLH-DSA functionality availables on latest release
> of recent OpenSSL library. Based on the history, this functionality was added in OpenSSL 3.5.
> So, make sure, you have required version of OpenSSL library installed. For simple guides how
> to build and install latest OpenSSL library on unix-like box, 
> see [install-latest-ssl.md](https://github.com/vlang/v/blob/master/vlib/x/crypto/slhdsa/install-latest-ssl.md)

## Basic
SLH-DSA signature scheme is constructed using other hash-based signature schemes as components. 
SLH-DSA was comes with set of predefined parameter that describes security categories, ie:<br>

- What is underlying hash algorithm used in the mean of process. <br>
The standard defines two's hash algorithm family availables, `sha2` family and `shake` family
- Security bits number of parameter set
- Whether the parameter set was designed to create relatively small signatures ('s') or to have 
relatively fast signature generation ('f').<br>
See Table. 2 SLH-DSA parameter sets in the standard document.  <br>

This parameter set represented by this opaque on this module:
```codeblock
pub enum Kind {
	// SHA2-based family
	sha2_128s = C.NID_SLH_DSA_SHA2_128s
	sha2_128f = C.NID_SLH_DSA_SHA2_128f
	sha2_192s = C.NID_SLH_DSA_SHA2_192s
	sha2_192f = C.NID_SLH_DSA_SHA2_192f
	sha2_256s = C.NID_SLH_DSA_SHA2_256s
	sha2_256f = C.NID_SLH_DSA_SHA2_256f
	// SHAKE-based family
	shake_128s = C.NID_SLH_DSA_SHAKE_128s
	shake_128f = C.NID_SLH_DSA_SHAKE_128f
	shake_192s = C.NID_SLH_DSA_SHAKE_192s
	shake_192f = C.NID_SLH_DSA_SHAKE_192f
	shake_256s = C.NID_SLH_DSA_SHAKE_256s
	shake_256f = C.NID_SLH_DSA_SHAKE_256f
}
```

Example
-------
```v cgen
import x.crypto.slhdsa

fn main() {
	// you can choose and pass the kind of the SLH-DSA parameter to the constructor
	opt := slhdsa.KeyOpts{
		kind: .sha2_128s
		// other options was availables
	}
	mut pv := slhdsa.PrivateKey.new(opt)!

	// Example message
	msg := 'SLH-DSA example message'.bytes()

	// Sign a message using constructed key
	sig := pv.sign(msg)!

	// Then the public key part can verify this signature
	mut pb := pv.public_key()!
	verified := pb.verify(sig, msg)!
	assert verified // true

	// release the resource
	pv.free()
	pb.free()
}
```
