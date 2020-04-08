# WebSockets Library for V

Originally located at [thecodrr/vws](https://github.com/thecodrr/vws) (contains example usage)

**This is still work-in-progress!**

Heavily inspired (and used **very** liberally) from [cwebsockets](https://github.com/jeremyhahn/cwebsocket). 

The websockets library itself is ready and working (passes all tests of AutoBahn). What's left:

1. It needs to be updated and made to run with latest V.
2. No Windows Support (SSL issues)
3. No proper AutoBahn test client (a prototype is in the main.v but nothing clean and neat).
4. No Websocket Server.

## What's needed for Windows support:

1. SSL (either make the VSChannel work or OpenSSL)

General code cleanup etc. is also needed.
