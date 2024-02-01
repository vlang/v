#ifndef __CPUINFO_H__
#define __CPUINFO_H__

// for x86
void cpuidex_asm(unsigned int *, unsigned int, unsigned int);
void xgetbv_asm(unsigned int *, unsigned int);
void rdtscp_asm(unsigned int *);

// for arm64
void read_aarch64_features(void *p);
#endif