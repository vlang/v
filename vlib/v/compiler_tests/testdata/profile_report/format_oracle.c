#include "format_oracle.h"
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int profile_report_expected(char *buffer, size_t size, uint64_t calls,
    double total, double self, const char *name) {
    return snprintf(buffer, size, "%14llu %14.3fms %14.3fms %14.0fns %s \n",
        (unsigned long long)calls, total / 1000000.0, self / 1000000.0,
        total / calls, name);
}

int profile_report_fd_is_open(int fd) { return fcntl(fd, F_GETFD) >= 0; }
int profile_report_close(int fd) { return close(fd); }
