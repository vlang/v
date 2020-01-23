picoev
======

A *tiny*, *lightning fast* event loop for network applications

The text below is copied from the [original publication](http://developer.cybozu.co.jp/archives/kazuho/2009/08/picoev-a-tiny-e.html)

I am sure many programmers writing network applications have their own abstracting layers hiding the differences between various I/O multiplex APIs, like select(2), poll(2), epoll(2), ... And of course, I am one among them.  While writing mycached ([see Mycached: memcached protocol support for MySQL for more information](http://developer.cybozu.co.jp/archives/kazuho/2009/08/mycached-memcac.html)), I was at first considering of using [libev](http://software.schmorp.de/pkg/libev.html) for multiplexing socket I/Os. [Libevent](http://www.monkey.org/~provos/libevent/) was not an option since it does not (yet) provide multithreading support.

But it was a great pain for me to learn how to use libev.  I do not mean that its is an ugly product.  In fact, I think that it is a very well written, excellent library.  However, for me it was too much a boring task to learn how the things are abstracted, already being familiar with the problems it tries to hide.

So instead I thought it might be a good occasion to write my own library that could be used in any programs I may write in the future.  The result is picoev, and it is faster than libevent or libev!  The benchmark used is a re-modified version taken from libev.schmorp.de/bench.html and can be found [here](http://coderepos.org/share/browser/lang/c/picoev/trunk/example/bench.c).
![setup time](http://developer.cybozu.co.jp/archives/kazuho/files/picoev_setup.png)
![event processing time](http://developer.cybozu.co.jp/archives/kazuho/files/picoev_event.png)

Why is it faster?  It is because it uses an array and a ring buffer of bit vectors as its internal structure.  Libevent and libev seem to use some kind of sorted tree to represent file descriptors.  However, if we concentrate on Un*x systems, there is a guarantee that the descriptors will be a small positive integer.  Picoev utilizes the fact and stores information related to file descriptors (such as pointers to callback functions or callback arguments) in an array, resulting in a faster manipulation of socket states.

Another optimization technique used by picoev is not to use an ordered tree for keeping timeout information.  Generally speaking, most network applications do not require accurate timeouts.  Thus it is possible to use a ring buffer (a sliding array) of bit vectors for the purpose.  Each bit vector represents a set of file descriptors that time-outs at a given time.  Picoev uses 128 of bit vectors to represent timeouts, for example, the first bit vector represents the sockets that timeout a second after, the second bit vector representing them of two seconds after..., and the bit vectors slide every second.  If the maximum timeout required by the web application is greater than 128, the minimum granurality of timeout becomes two seconds.

I would like to reiterate that both libevent and libev are great libraries.  Picoev is not at comparable to them especially in maturity and the number of features.  It only supports select(2), epoll(2), and kqueue(2) for the time being.  However the design is simple than the other two, and I think it will be a good starting point to write network applications, or to use as a basis for writing one's own network libraries.

