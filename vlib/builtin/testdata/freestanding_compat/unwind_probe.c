#define _GNU_SOURCE
#include "unwind_probe.h"
#include "../../linux_bare/libc_dynamic_abi.h"
#include <dlfcn.h>
#include <errno.h>
#include <link.h>
#include <pthread.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unwind.h>
#define CHECK(c) do { if(!(c))return __LINE__; } while(0)
struct trace { int frames,main,root,leaf,failed; uintptr_t failed_address; };
static _Unwind_Reason_Code visit_frame(struct _Unwind_Context *context,void *argument) {
    struct trace *trace=argument;
    uintptr_t address=(uintptr_t)_Unwind_GetIP(context);
    if(!address)return _URC_NO_REASON;
    struct dl_find_object info;
    if(_dl_find_object((void *)(address-1),&info)) { trace->failed=1; trace->failed_address=address-1; }
    else if(!info.dlfo_link_map || !*info.dlfo_link_map->l_name)trace->main=1;
    else {
        trace->root|=strstr(info.dlfo_link_map->l_name,"libv_dynamic_root.so")!=NULL;
        trace->leaf|=strstr(info.dlfo_link_map->l_name,"libv_dynamic_leaf.so")!=NULL;
    }
    return ++trace->frames<64 ? _URC_NO_REASON:_URC_END_OF_STACK;
}
__attribute__((noinline)) static void collect_trace(void *argument) {
    struct trace *trace=argument;
    if(_Unwind_Backtrace(visit_frame,trace)!=_URC_END_OF_STACK)trace->failed=1;
}
__attribute__((noinline)) static void static_trace_layer(void *argument) { collect_trace(argument); }
static void *metadata_thread(void *address) {
    for(int i=0;i<100;i++) {
        struct dl_find_object info;
        if(_dl_find_object(address,&info) || !info.dlfo_eh_frame ||
           (uintptr_t)address<(uintptr_t)info.dlfo_map_start ||
           (uintptr_t)address>=(uintptr_t)info.dlfo_map_end)return (void *)1;
    }
    return NULL;
}
struct expected_image { void *address; uintptr_t start,end,eh,sframe; int found; };
static int expected_image(struct dl_phdr_info *info,size_t size,void *argument) {
    (void)size; struct expected_image *image=argument;
    uintptr_t start=UINTPTR_MAX,end=0,eh=0,sframe=0; int contains=0;
    for(ElfW(Half) i=0;i<info->dlpi_phnum;i++) {
        const ElfW(Phdr) *ph=&info->dlpi_phdr[i]; uintptr_t at=info->dlpi_addr+ph->p_vaddr;
        if(ph->p_type==PT_LOAD && ph->p_memsz) {
            if(at<start)start=at; if(at+ph->p_memsz>end)end=at+ph->p_memsz;
            contains|=(uintptr_t)image->address>=at && (uintptr_t)image->address<at+ph->p_memsz;
        }
        if(ph->p_type==PT_GNU_EH_FRAME)eh=at;
#ifdef PT_GNU_SFRAME
        if(ph->p_type==PT_GNU_SFRAME)sframe=at;
#endif
    }
    if(contains) { image->start=start; image->end=end; image->eh=eh; image->sframe=sframe; image->found=1; return 1; }
    return 0;
}
static int check_image(void *address,int dynamic) {
    struct { uint64_t before; struct dl_find_object info; uint64_t after; } guarded;
    memset(&guarded,0xa5,sizeof guarded); guarded.before=0x12345678; guarded.after=0x76543210;
    errno=E2BIG; CHECK(!_dl_find_object(address,&guarded.info) && errno==E2BIG);
    CHECK(guarded.before==0x12345678 && guarded.after==0x76543210);
    CHECK((uintptr_t)address>=(uintptr_t)guarded.info.dlfo_map_start &&
          (uintptr_t)address<(uintptr_t)guarded.info.dlfo_map_end);
    CHECK(guarded.info.dlfo_eh_frame && *(const unsigned char *)guarded.info.dlfo_eh_frame==1);
    if(dynamic) {
        struct expected_image expected={.address=address};
        CHECK(dl_iterate_phdr(expected_image,&expected)==1 && expected.found);
        CHECK((uintptr_t)guarded.info.dlfo_map_start==expected.start &&
              (uintptr_t)guarded.info.dlfo_map_end==expected.end && (uintptr_t)guarded.info.dlfo_eh_frame==expected.eh);
#ifdef DLFO_FLAG_SFRAME
        CHECK((uintptr_t)guarded.info.dlfo_sframe==expected.sframe);
        CHECK(guarded.info.dlfo_flags==(expected.sframe ? DLFO_FLAG_SFRAME:0));
#endif
    } else CHECK(!guarded.info.dlfo_link_map);
    return 0;
}
__attribute__((noinline)) int32_t v_unwind_probe(int32_t dynamic) {
    int result=check_image((void *)v_unwind_probe,dynamic); CHECK(!result);
    struct trace trace={0};
    if(dynamic) {
        void (*root)(void (*)(void *),void *)=dlsym(RTLD_DEFAULT,"v_dynamic_root_trace");
        void *leaf=dlsym(RTLD_DEFAULT,"v_dynamic_leaf_trace"); CHECK(root && leaf);
        CHECK(!check_image((void *)root,1) && !check_image(leaf,1));
        root(collect_trace,&trace);
        CHECK(trace.root && trace.leaf);
        pthread_t threads[2];
        CHECK(!pthread_create(&threads[0],NULL,metadata_thread,(void *)root));
        CHECK(!pthread_create(&threads[1],NULL,metadata_thread,leaf));
        for(int i=0;i<2;i++) { void *value=(void *)1; CHECK(!pthread_join(threads[i],&value) && !value); }
    } else static_trace_layer(&trace);
    if(trace.failed || !trace.main || trace.frames<4 || trace.frames>=64)
        fprintf(stderr,"native unwind: failed=%d main=%d frames=%d missing=%p\n",trace.failed,trace.main,trace.frames,(void *)trace.failed_address);
    CHECK(!trace.failed && trace.main && trace.frames>=4 && trace.frames<64);
    struct dl_find_object before,after; memset(&before,0xa5,sizeof before); after=before;
    errno=ENOENT; CHECK(_dl_find_object(NULL,&after)==-1 && errno==ENOENT && !memcmp(&before,&after,sizeof before));
    CHECK(_dl_find_object(&result,&after)==-1 && !memcmp(&before,&after,sizeof before));
    void *area=mmap(NULL,4096,PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0); CHECK(area!=MAP_FAILED);
    CHECK(_dl_find_object(area,&after)==-1 && !memcmp(&before,&after,sizeof before)); CHECK(!munmap(area,4096));
    if(dynamic) {
        dlerror(); CHECK(!dlsym(RTLD_DEFAULT,"v_unwind_missing"));
        CHECK(!_dl_find_object((void *)v_unwind_probe,&after) && dlerror() && !dlerror());
    }
    return 0;
}
