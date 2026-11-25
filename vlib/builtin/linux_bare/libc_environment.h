#ifndef V_LINUX_BARE_ENVIRONMENT_H
#define V_LINUX_BARE_ENVIRONMENT_H
void v_freestanding_environment_lock(void);
void v_freestanding_environment_unlock(void);
char *v_freestanding_environment_get_locked(const char *name);
#endif
