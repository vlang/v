This file contains examples on about how to use this `asn1` module to write your ASN.1 element, 
how to serialize it and write routines how to decode it. Its contains 4 of examples, ie:

- `example0.v` contains examples on how to write simple thing for some parts of 
Kerberos Network Authentication protocol parser, how to define your structure 
based on the ASN.1 Schema, use tagged field for wrapping element, how to do serialization 
and deserialization of this object, etc.

- `example1.v` contains sample from already availables on the internet world, taken from 
[oss](https://www.oss.com/asn1/resources/asn1-made-simple/asn1-quick-reference/sequence.html). 
Its contains more complex structure, write serializer and deserializer for that.

- `example2.v` contains samples encoding for DER mode comes from `ITU X.690 Document`, 
Especially from Annex A. Example of encodings of the document. 
This examples contains some complex nested elements wrapping detailed in step by step 
for building the ASN.1 DER serializer with this modules.

- `examples3.v` is an example from README files.
