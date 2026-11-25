#include "probe.h"
#include <stdio.h>
int main(int argc, char **argv) {
    if (argc != 2) return 2;
    int status = v_freestanding_format_contracts();
    if (!status) status = v_freestanding_format_oracle(argv[1]);
    if (status) fprintf(stderr, "native format probe failed at C line %d\n", status);
    return status ? 1 : 0;
}
