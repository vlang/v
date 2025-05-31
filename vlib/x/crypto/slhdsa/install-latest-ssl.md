This `slhdsa` module requires latest OpenSSL library and development headers.
It required minimum of OpenSLL 3.5 version.
Likely most of the linux (unix) distros do not ship with this version on this time 
of writing, at March, 2025.

You can manually download and install OpenSSL library by your self. This documents acts as a simple
guide on how to build and install latest OpenSSL library to your box.

This guide was for unix-like system, for others, see the guides more detail goes to 
[install.md](https://github.com/openssl/openssl/blob/master/INSTALL.md#building-openssl)<br>
1. Go to https://openssl-library.org/source/ and download the latest version
```bash
$ wget -v https://github.com/openssl/openssl/releases/download/openssl-3.5.0-beta1/openssl-3.5.0-beta1.tar.gz
```
2. Extract your downloaded file
```bash
$ tar -xzvf openssl-3.5.0-beta1.tar.gz
$ cd openssl-3.5.0-beta1
```
3. Configure the build flag, this module for optional build requires to use `/opt/ssl` prefix. 
```bash
$ ./Configure --prefix=/opt/ssl --openssldir=/opt/ssl
$ make
$ sudo make install
```
By successfully completing this step, the latest OpenSSL has been built and installed onto 
configured path. You need to tell the system to add this path. <br>

4. Add `/opt/ssl/lib64` entry onto `/etc/ld.so.conf.d/libcrypto.conf`

5. Run `ldconfig`
```bash
$sudo ldconfig
```
6. Verify if the modules works.