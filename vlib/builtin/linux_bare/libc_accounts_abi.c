#include "libc_compat.h"
#include "libc_accounts_abi.h"
#include <grp.h>
#include <pwd.h>
/* Native storage, field marshalling and public prototypes only. */
uintptr_t v_account_size(int32_t group) { return group ? sizeof(struct group) : sizeof(struct passwd); }
void v_account_assign(uintptr_t output,uintptr_t fields,uintptr_t members,uint32_t uid,uint32_t gid,int32_t group) {
    char **f=(char **)fields;
    if (group) *(struct group *)output=(struct group){.gr_name=f[0],.gr_passwd=f[1],.gr_gid=gid,.gr_mem=(char **)members};
    else *(struct passwd *)output=(struct passwd){.pw_name=f[0],.pw_passwd=f[1],.pw_uid=uid,.pw_gid=gid,.pw_gecos=f[4],.pw_dir=f[5],.pw_shell=f[6]};
}
extern int32_t v_account_lookup(uintptr_t,uint32_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t,int32_t);
extern uintptr_t v_account_static(uintptr_t,uint32_t,int32_t);
int getgrgid_r(gid_t id,struct group *e,char *b,size_t n,struct group **r) { return v_account_lookup(0,id,(uintptr_t)e,(uintptr_t)b,n,(uintptr_t)r,1); }
int getgrnam_r(const char *name,struct group *e,char *b,size_t n,struct group **r) { return v_account_lookup((uintptr_t)name,0,(uintptr_t)e,(uintptr_t)b,n,(uintptr_t)r,1); }
int getpwuid_r(uid_t id,struct passwd *e,char *b,size_t n,struct passwd **r) { return v_account_lookup(0,id,(uintptr_t)e,(uintptr_t)b,n,(uintptr_t)r,0); }
int getpwnam_r(const char *name,struct passwd *e,char *b,size_t n,struct passwd **r) { return v_account_lookup((uintptr_t)name,0,(uintptr_t)e,(uintptr_t)b,n,(uintptr_t)r,0); }
struct group *getgrgid(gid_t id) { return (struct group *)v_account_static(0,id,1); }
struct group *getgrnam(const char *name) { return (struct group *)v_account_static((uintptr_t)name,0,1); }
struct passwd *getpwuid(uid_t id) { return (struct passwd *)v_account_static(0,id,0); }
struct passwd *getpwnam(const char *name) { return (struct passwd *)v_account_static((uintptr_t)name,0,0); }
