To update this, change the version/branch in `thirdparty/mbedtls/update.vsh`,
then run `./v thirdparty/mbedtls/update.vsh` .

The patch containing the local changes, that needs to be applied is in:
`thirdparty/mbedtls/mbedtls.patch` . It may need changes for the new version too.

BTW, you can generate a new patch by `diff -ur mbedtls.orig mbedtls > mbedtls.patch`
