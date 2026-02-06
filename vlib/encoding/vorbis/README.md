## encoding.vorbis

This module is a thin wrapper around [stb_vorbis](https://github.com/nothings/stb/blob/master/stb_vorbis.c),
which is a public domain Ogg Vorbis audio decoder, originally written by Sean Barrett.

Example:
```v
import encoding.vorbis

x := vorbis.decode_file('coin.ogg')!
dump(x)
unsafe { x.free() }
```

... may produce something like this:
```
[x.v:4] x: vorbis.VorbisData{
    path: 'coin.ogg'
    channels: 2
    sample_rate: 48000
    len: 5760800
    data: &0
}				
```

Note: x.data points to the actual memory block that has the decoded content.
