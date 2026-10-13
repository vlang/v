The LEB128 decoders return the decoded integer and the number of input bytes consumed.
The unsigned decoders return zero bytes consumed for empty input and the actual input length
when a continuation sequence ends without a terminating byte.
