#define _GNU_SOURCE 1
#include <spawn.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>

/* The public types are opaque. Copy this representation in and out so the
 * adapters work with native headers without depending on their field names. */
struct spawn_attributes { uint64_t defaults, mask; int32_t flags, group, policy, priority; };
_Static_assert(sizeof(struct spawn_attributes) <= sizeof(posix_spawnattr_t), "spawn attributes");
_Static_assert(sizeof(posix_spawn_file_actions_t) >= 2 * sizeof(void *), "spawn action list");
_Static_assert(sizeof(sigset_t) >= sizeof(uint64_t), "Linux kernel signal set");
extern int32_t v_spawn_run(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, int32_t);
extern int32_t v_spawn_action_add(uintptr_t, int32_t, int32_t, int32_t, uintptr_t, int32_t, uint32_t);
extern void v_spawn_actions_free(uintptr_t);
int posix_spawnattr_init(posix_spawnattr_t *a) { memset(a, 0, sizeof *a); return 0; }
int posix_spawnattr_destroy(posix_spawnattr_t *a) { (void)a; return 0; }
#define ATTR_GETSET(name,type,field,valid) \
int posix_spawnattr_get##name(const posix_spawnattr_t *a, type *out) { \
    struct spawn_attributes s; memcpy(&s, a, sizeof s); *out = (type)s.field; return 0; \
} \
int posix_spawnattr_set##name(posix_spawnattr_t *a, type value) { \
    if (!(valid)) return EINVAL; \
    struct spawn_attributes s; memcpy(&s, a, sizeof s); s.field = value; memcpy(a, &s, sizeof s); return 0; \
}
ATTR_GETSET(pgroup,pid_t,group,1)
ATTR_GETSET(schedpolicy,int,policy,value == SCHED_OTHER || value == SCHED_FIFO || value == SCHED_RR)
int posix_spawnattr_getflags(const posix_spawnattr_t *a, short *out) {
    struct spawn_attributes s; memcpy(&s,a,sizeof s); *out=(short)s.flags; return 0;
}
int posix_spawnattr_setflags(posix_spawnattr_t *a, short flags) {
    if ((unsigned short)flags & ~(POSIX_SPAWN_RESETIDS | POSIX_SPAWN_SETPGROUP |
        POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSCHEDPARAM |
        POSIX_SPAWN_SETSCHEDULER | POSIX_SPAWN_USEVFORK | POSIX_SPAWN_SETSID)) return EINVAL;
    struct spawn_attributes s; memcpy(&s,a,sizeof s); s.flags=flags; memcpy(a,&s,sizeof s); return 0;
}
#define ATTR_MASK(name,field) \
int posix_spawnattr_get##name(const posix_spawnattr_t *a, sigset_t *out) { \
    struct spawn_attributes s; memcpy(&s,a,sizeof s); memset(out,0,sizeof *out); memcpy(out,&s.field,8); return 0; \
} \
int posix_spawnattr_set##name(posix_spawnattr_t *a,const sigset_t *mask) { \
    struct spawn_attributes s; memcpy(&s,a,sizeof s); memcpy(&s.field,mask,8); memcpy(a,&s,sizeof s); return 0; \
}
ATTR_MASK(sigdefault,defaults)
ATTR_MASK(sigmask,mask)
int posix_spawnattr_getschedparam(const posix_spawnattr_t *a, struct sched_param *p) {
    struct spawn_attributes s; memcpy(&s,a,sizeof s); p->sched_priority=s.priority; return 0;
}
int posix_spawnattr_setschedparam(posix_spawnattr_t *a,const struct sched_param *p) {
    struct spawn_attributes s; memcpy(&s,a,sizeof s); s.priority=p->sched_priority; memcpy(a,&s,sizeof s); return 0;
}
int posix_spawn_file_actions_init(posix_spawn_file_actions_t *a) { memset(a,0,sizeof *a); return 0; }
int posix_spawn_file_actions_destroy(posix_spawn_file_actions_t *a) { v_spawn_actions_free((uintptr_t)a); return 0; }
int posix_spawn_file_actions_addclose(posix_spawn_file_actions_t *a,int fd) {
    return v_spawn_action_add((uintptr_t)a,1,fd,0,0,0,0);
}
int posix_spawn_file_actions_adddup2(posix_spawn_file_actions_t *a,int oldfd,int newfd) {
    return v_spawn_action_add((uintptr_t)a,2,oldfd,newfd,0,0,0);
}
int posix_spawn_file_actions_addopen(posix_spawn_file_actions_t *a,int fd,const char *path,int flags,mode_t mode) {
    return v_spawn_action_add((uintptr_t)a,3,fd,0,(uintptr_t)path,flags,mode);
}
int posix_spawn_file_actions_addchdir_np(posix_spawn_file_actions_t *a,const char *path) {
    return v_spawn_action_add((uintptr_t)a,4,0,0,(uintptr_t)path,0,0);
}
int posix_spawn_file_actions_addfchdir_np(posix_spawn_file_actions_t *a,int fd) {
    return v_spawn_action_add((uintptr_t)a,5,fd,0,0,0,0);
}
int posix_spawn_file_actions_addclosefrom_np(posix_spawn_file_actions_t *a,int fd) {
    return v_spawn_action_add((uintptr_t)a,6,fd,0,0,0,0);
}
int posix_spawn(pid_t *pid,const char *path,const posix_spawn_file_actions_t *actions,
                const posix_spawnattr_t *attr,char *const argv[],char *const envp[]) {
    struct spawn_attributes s={0}; if(attr) memcpy(&s,attr,sizeof s);
    return v_spawn_run((uintptr_t)pid,(uintptr_t)path,(uintptr_t)actions,(uintptr_t)&s,(uintptr_t)argv,(uintptr_t)envp,0);
}
int posix_spawnp(pid_t *pid,const char *file,const posix_spawn_file_actions_t *actions,
                 const posix_spawnattr_t *attr,char *const argv[],char *const envp[]) {
    struct spawn_attributes s={0}; if(attr) memcpy(&s,attr,sizeof s);
    return v_spawn_run((uintptr_t)pid,(uintptr_t)file,(uintptr_t)actions,(uintptr_t)&s,(uintptr_t)argv,(uintptr_t)envp,1);
}
