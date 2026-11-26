#define _GNU_SOURCE
#include "dynamic_probe.h"
#include <dlfcn.h>
#include <link.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
extern int v_dynamic_root(int);
#define CHECK(c) do { if(!(c))return __LINE__; } while(0)
static void *thread_error(void *unused) {
    (void)unused;
    if(dlerror())return (void *)1;
    if(dlsym(RTLD_DEFAULT,"v_dynamic_absent"))return (void *)2;
    if(!dlerror()||dlerror())return (void *)3;
    return 0;
}
static int visit(struct dl_phdr_info *info,size_t size,void *data) {
    if(size<offsetof(struct dl_phdr_info,dlpi_adds)||!info->dlpi_phdr||!info->dlpi_phnum)return 91;
    if(strstr(info->dlpi_name,"libv_dynamic_root.so"))(*(int *)data)++;
    return 0;
}
int32_t v_dynamic_probe(void) {
    CHECK(v_dynamic_root(6)==43); dlerror();
    void *handle=dlopen("libv_dynamic_root.so",RTLD_NOW|RTLD_LOCAL); CHECK(handle);
    CHECK(dlsym(handle,"v_dynamic_root")==v_dynamic_root);
    int (*leaf)(int)=dlsym(handle,"v_dynamic_leaf"); CHECK(leaf&&leaf(6)==42);
    CHECK(dlsym(RTLD_DEFAULT,"v_dynamic_root")==v_dynamic_root);
    CHECK(!dlsym(handle,"v_dynamic_zero") && !dlerror());
    CHECK(dlsym(RTLD_NEXT,"v_dynamic_root")==v_dynamic_root);
    void *main=dlopen(0,RTLD_NOW); CHECK(main);
    CHECK(dlsym(main,"v_dynamic_root")==v_dynamic_root);
    int *preloaded=dlsym(RTLD_DEFAULT,"v_dynamic_preloaded");
    CHECK(preloaded && *preloaded==17 && dlsym(main,"v_dynamic_preloaded")==preloaded);
    CHECK(!dlsym(handle,"v_dynamic_preloaded") && dlerror());
    CHECK(!dlclose(main) && !dlsym(main,"v_dynamic_preloaded") && dlerror());
    Dl_info executable={0};
    CHECK(dladdr(v_dynamic_probe,&executable));
    CHECK(executable.dli_fname && !strcmp(executable.dli_fname,program_invocation_name));
    Dl_info info={0}; CHECK(dladdr(v_dynamic_root,&info));
    CHECK(info.dli_fname&&strstr(info.dli_fname,"libv_dynamic_root.so"));
    CHECK(info.dli_sname&&!strcmp(info.dli_sname,"v_dynamic_root"));
#ifdef __GLIBC__
    struct link_map *map=NULL,*map_again=NULL;
    CHECK(dladdr1(v_dynamic_root,&info,(void **)&map,RTLD_DL_LINKMAP)&&map);
    CHECK(dlinfo(map,RTLD_DI_LINKMAP,&map_again)==0 && map_again==map);
    Lmid_t namespace_id=-1;
    CHECK(!dlinfo(map,RTLD_DI_LMID,&namespace_id)&&namespace_id==LM_ID_BASE);
    const ElfW(Sym) *symbol=NULL;
    CHECK(dladdr1(v_dynamic_root,&info,(void **)&symbol,RTLD_DL_SYMENT)&&symbol);
    CHECK((void *)(map->l_addr+symbol->st_value)==(void *)v_dynamic_root);
#if __GLIBC_PREREQ(2,34)
    const ElfW(Phdr) *headers=NULL;
    const ElfW(Ehdr) *elf=(const void *)map->l_addr;
    CHECK(dlinfo(map,RTLD_DI_PHDR,&headers)==elf->e_phnum);
    CHECK((const void *)headers==(const char *)elf+elf->e_phoff);
#endif
    struct link_map *leaf_map=NULL;
    CHECK(dladdr1(leaf,&info,(void **)&leaf_map,RTLD_DL_LINKMAP)&&leaf_map);
    CHECK(!dlinfo(leaf_map,RTLD_DI_LINKMAP,&map_again)&&map_again==leaf_map);
    Dl_info guarded;memset(&guarded,0xa5,sizeof guarded);Dl_info unchanged=guarded;
    void *extra=(void *)17;
    CHECK(!dladdr1((void *)1,&guarded,&extra,RTLD_DL_LINKMAP));
    CHECK(extra==(void *)17&&!memcmp(&guarded,&unchanged,sizeof guarded));
    char expected[PATH_MAX],origin[PATH_MAX],alias[PATH_MAX];
    CHECK(getcwd(expected,sizeof expected));
    CHECK(snprintf(alias,sizeof alias,"%s/libv_dynamic_alias.so",expected)>0);
    int cwd=open(".",O_RDONLY|O_DIRECTORY|O_CLOEXEC);CHECK(cwd>=0&&!chdir("/"));
    errno=E2BIG;
    CHECK(!dlinfo(map,RTLD_DI_ORIGIN,origin)&&!strcmp(origin,expected)&&errno==E2BIG);
    void *alias_handle=dlopen(alias,RTLD_NOW);CHECK(alias_handle&&errno==E2BIG&&!dlerror());
    CHECK(dlsym(alias_handle,"v_dynamic_root")==v_dynamic_root&&!dlclose(alias_handle));
    alias_handle=dlopen("libv_dynamic_alias.so",RTLD_NOW);CHECK(alias_handle&&errno==E2BIG&&!dlerror());
    CHECK(!dlclose(alias_handle)&&!fchdir(cwd)&&!close(cwd));
    uintptr_t untouched=19;
    CHECK(dlinfo(map,-1,&untouched)==-1&&untouched==19&&dlerror()&&!dlerror());
#endif
    int count=0; CHECK(!dl_iterate_phdr(visit,&count)&&count==1);
    CHECK(!dlopen("v_dynamic_missing_library.so",RTLD_NOW));
    pthread_t thread; void *result=(void *)1;
    CHECK(!pthread_create(&thread,0,thread_error,0));
    CHECK(!pthread_join(thread,&result)&&!result);
    CHECK(dlerror()&&!dlerror());
    CHECK(!dlclose(handle)); CHECK(!dlsym(handle,"v_dynamic_root")&&dlerror());
    CHECK(dlclose(handle)==-1&&dlerror()&&!dlerror());
    return 0;
}
