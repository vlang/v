#ifndef V_FREESTANDING_COMPAT_PROBE_H
#define V_FREESTANDING_COMPAT_PROBE_H
int v_freestanding_compat_check(const char *directory);
void v_freestanding_compat_increment(void);
int v_freestanding_compat_total(void);
int v_freestanding_format_oracle(const char *path);
int v_freestanding_format_contracts(void);
int v_freestanding_stdio_check(const char *directory);
void v_freestanding_stdio_exit_output(int explicit_exit);
int v_freestanding_mutex_check(void);
int v_freestanding_resolver_check(const char *directory, unsigned short port, int configured);
int v_freestanding_dns_parser_fuzz(void);
int v_freestanding_fontstack_check(const char *directory);
void v_freestanding_condition_worker(void);
int v_freestanding_condition_release(void);
int v_freestanding_fontstack_environment(void);
#endif
