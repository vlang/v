# Description

`ringbuf` is a small library that makes it possible to create a ringbuffer for processing all kinds of data, e.g. when creating applications that handle audio streams.


**If you want to exchange data between multiple threads, it is highly recommended to declare and pass a ringbuffer as a shared variable.**
**[Read more about shared objects](https://github.com/vlang/v/blob/master/doc/docs.md#shared-objects)**

## Example:

```
module main

import ringbuffer

fn main() {

    // create a new ringbuffer with size 4
    // the generic argument sets the type
    mut r := ringbuffer.new<int>(4)

    // push elements
    r.push(3)?
    r.push(4)?

    // get the oldest element. It will be removed from the buffer when calling `pop()`
    oldest_value := r.pop()?

    println(oldest_value) // Output: 3

    }

```