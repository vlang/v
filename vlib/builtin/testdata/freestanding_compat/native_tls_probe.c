#define _GNU_SOURCE
#include "native_tls_probe.h"
#include <archive.h>
#include <archive_entry.h>
#include <curl/curl.h>
#include <errno.h>
#include <dlfcn.h>
#include <link.h>
#include <pthread.h>
#include <gpgme.h>
#include <mbedtls/version.h>
#include <seccomp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define CHECK(condition) do { if(!(condition)) return __LINE__; } while(0)
extern unsigned int vlibc_private_mbedtls_version_get_number(void);
static void *dynamic_error_thread(void *unused) {
    (void)unused;
    if(dlerror()) return (void *)1;
    if(dlsym(RTLD_DEFAULT,"v_no_such_dynamic_symbol")) return (void *)2;
    if(!dlerror() || dlerror()) return (void *)3;
    return 0;
}
static int dynamic_visit(struct dl_phdr_info *info,size_t size,void *data) {
    unsigned *count=data;
    if(size<offsetof(struct dl_phdr_info,dlpi_adds)||!info->dlpi_phdr||!info->dlpi_phnum)return 73;
    (*count)++; return 0;
}
static int dynamic_libraries(void) {
    dlerror();
    void *handle=dlopen("libcurl-vlibc.so",RTLD_NOW|RTLD_LOCAL);
    CHECK(handle);
    CHECK(dlsym(handle,"curl_version_info")==curl_version_info);
    CHECK(dlsym(RTLD_DEFAULT,"curl_version_info")==curl_version_info);
    CHECK(dlsym(handle,"vlibc_private_mbedtls_version_get_number")==vlibc_private_mbedtls_version_get_number);
    CHECK(!dlerror());
    CHECK(dlsym(RTLD_NEXT,"curl_version_info")==curl_version_info);
    Dl_info address={0}; CHECK(dladdr(curl_version_info,&address));
    CHECK(address.dli_fname && strstr(address.dli_fname,"libcurl-vlibc.so"));
    CHECK(address.dli_sname && !strcmp(address.dli_sname,"curl_version_info"));
    unsigned objects=0; CHECK(!dl_iterate_phdr(dynamic_visit,&objects) && objects>10);
    void *brotli=dlopen("libbrotlidec.so",RTLD_LAZY|RTLD_LOCAL); CHECK(brotli);
    CHECK(dlsym(brotli,"BrotliDecoderCreateInstance")); CHECK(!dlclose(brotli));
    CHECK(!dlopen("v_no_such_library.so",RTLD_NOW));
    pthread_t thread; void *result=(void *)1;
    CHECK(!pthread_create(&thread,0,dynamic_error_thread,0));
    CHECK(!pthread_join(thread,&result) && !result);
    CHECK(dlerror() && !dlerror());
    CHECK(!dlclose(handle));
    CHECK(!dlsym(handle,"curl_version_info") && dlerror());
    CHECK(dlclose(handle)==-1 && dlerror() && !dlerror());
    return 0;
}
int32_t v_native_libraries_prepare(void) {
    int dynamic_result=dynamic_libraries(); if(dynamic_result)return dynamic_result;
    CHECK(vlibc_private_mbedtls_version_get_number()==0x03060800);
    CHECK(mbedtls_version_get_number!=vlibc_private_mbedtls_version_get_number);
    CHECK(!curl_global_init(CURL_GLOBAL_DEFAULT));
    const curl_version_info_data *info=curl_version_info(CURLVERSION_NOW);
    CHECK(info && (info->features&CURL_VERSION_SSL) && (info->features&CURL_VERSION_HTTP2));
    CHECK(info->ssl_version && strstr(info->ssl_version,"mbedTLS/3.6.8"));
    CHECK(gpgme_check_version("2.3.1"));
    return 0;
}
struct response { char body[128]; size_t length; };
static size_t body_write(char *data,size_t size,size_t count,void *argument) {
    struct response *r=argument;
    if(size && count>SIZE_MAX/size) return 0;
    size_t bytes=size*count;
    if(bytes>sizeof r->body-r->length) return 0;
    memcpy(r->body+r->length,data,bytes); r->length+=bytes; return bytes;
}
int32_t v_native_libraries_fetch(const char *url,const char *certificate) {
    CURL *c=curl_easy_init(); CHECK(c); struct response r={0};
    CHECK(!curl_easy_setopt(c,CURLOPT_URL,url));
    CHECK(!curl_easy_setopt(c,CURLOPT_CAINFO,certificate));
    CHECK(!curl_easy_setopt(c,CURLOPT_HTTP_VERSION,(long)CURL_HTTP_VERSION_2TLS));
    CHECK(!curl_easy_setopt(c,CURLOPT_SSL_VERIFYPEER,1L));
    CHECK(!curl_easy_setopt(c,CURLOPT_SSL_VERIFYHOST,2L));
    CHECK(!curl_easy_setopt(c,CURLOPT_TIMEOUT,5L));
    CHECK(!curl_easy_setopt(c,CURLOPT_NOSIGNAL,1L));
    CHECK(!curl_easy_setopt(c,CURLOPT_WRITEFUNCTION,body_write));
    CHECK(!curl_easy_setopt(c,CURLOPT_WRITEDATA,&r));
    CURLcode result=curl_easy_perform(c);
    if(result)fprintf(stderr,"curl HTTPS: %s\n",curl_easy_strerror(result));
    CHECK(result==CURLE_OK);
    long status=0; CHECK(!curl_easy_getinfo(c,CURLINFO_RESPONSE_CODE,&status));
    long version=0; CHECK(!curl_easy_getinfo(c,CURLINFO_HTTP_VERSION,&version) && version==CURL_HTTP_VERSION_2_0);
    CHECK(status==200 && r.length==18 && !memcmp(r.body,"native HTTPS body\n",18));
    curl_easy_cleanup(c); return 0;
}
int32_t v_native_libraries_formats(void) {
    static const int filters[]={ARCHIVE_FILTER_NONE,ARCHIVE_FILTER_GZIP,ARCHIVE_FILTER_BZIP2,ARCHIVE_FILTER_XZ,ARCHIVE_FILTER_ZSTD,ARCHIVE_FILTER_LZ4};
    const char text[]="native archive body\n";
    for(size_t i=0;i<sizeof filters/sizeof *filters;i++) {
        char *memory=malloc(65536); CHECK(memory); size_t used=0;
        struct archive *writer=archive_write_new(); CHECK(writer);
        CHECK(archive_write_set_format_pax_restricted(writer)==ARCHIVE_OK);
        CHECK(archive_write_add_filter(writer,filters[i])==ARCHIVE_OK);
        CHECK(archive_write_open_memory(writer,memory,65536,&used)==ARCHIVE_OK);
        struct archive_entry *entry=archive_entry_new(); CHECK(entry);
        archive_entry_set_pathname(entry,"native.txt");
        archive_entry_set_filetype(entry,AE_IFREG); archive_entry_set_perm(entry,0644);
        archive_entry_set_uid(entry,123); archive_entry_set_gid(entry,456);
        archive_entry_set_size(entry,sizeof text-1);
        CHECK(archive_write_header(writer,entry)==ARCHIVE_OK);
        CHECK(archive_write_data(writer,text,sizeof text-1)==sizeof text-1);
        archive_entry_free(entry);
        CHECK(archive_write_close(writer)==ARCHIVE_OK); CHECK(archive_write_free(writer)==ARCHIVE_OK);
        struct archive *reader=archive_read_new(); CHECK(reader);
        CHECK(archive_read_support_filter_all(reader)==ARCHIVE_OK);
        CHECK(archive_read_support_format_tar(reader)==ARCHIVE_OK);
        CHECK(archive_read_open_memory(reader,memory,used)==ARCHIVE_OK);
        CHECK(archive_read_next_header(reader,&entry)==ARCHIVE_OK);
        CHECK(!strcmp(archive_entry_pathname(entry),"native.txt"));
        CHECK(archive_entry_uid(entry)==123 && archive_entry_gid(entry)==456 && archive_entry_perm(entry)==0644);
        char output[64]; CHECK(archive_read_data(reader,output,sizeof output)==sizeof text-1);
        CHECK(!memcmp(output,text,sizeof text-1));
        CHECK(archive_read_next_header(reader,&entry)==ARCHIVE_EOF);
        CHECK(archive_read_free(reader)==ARCHIVE_OK); free(memory);
    }
    scmp_filter_ctx filter=seccomp_init(SCMP_ACT_ALLOW); CHECK(filter);
    CHECK(seccomp_rule_add(filter,SCMP_ACT_ERRNO(EACCES),SCMP_SYS(chdir),0)==0);
    int descriptors[2]; CHECK(!pipe(descriptors));
    CHECK(seccomp_export_bpf(filter,descriptors[1])==0);
    char byte; CHECK(read(descriptors[0],&byte,1)==1);
    CHECK(!close(descriptors[0]) && !close(descriptors[1])); seccomp_release(filter);
    return 0;
}
int32_t v_native_libraries_verify(const char *signature,const char *data) {
    gpgme_ctx_t context=NULL; CHECK(!gpgme_new(&context));
    CHECK(!gpgme_set_protocol(context,GPGME_PROTOCOL_OpenPGP));
    gpgme_data_t signed_data=NULL,plain_data=NULL;
    CHECK(!gpgme_data_new_from_file(&signed_data,signature,1));
    CHECK(!gpgme_data_new_from_file(&plain_data,data,1));
    gpgme_error_t code=gpgme_op_verify(context,signed_data,plain_data,NULL);
    if(code)fprintf(stderr,"GPGME verification: %s\n",gpgme_strerror(code));
    CHECK(!code);
    gpgme_verify_result_t result=gpgme_op_verify_result(context);
    CHECK(result && result->signatures && !result->signatures->status && result->signatures->fpr);
    CHECK(gpgme_data_seek(signed_data,0,SEEK_SET)==0);
    gpgme_data_release(plain_data); plain_data=NULL;
    CHECK(!gpgme_data_new_from_mem(&plain_data,"tampered data\n",14,1));
    CHECK(!gpgme_op_verify(context,signed_data,plain_data,NULL));
    result=gpgme_op_verify_result(context);
    CHECK(result && result->signatures && gpg_err_code(result->signatures->status)==GPG_ERR_BAD_SIGNATURE);
    gpgme_data_release(signed_data); gpgme_data_release(plain_data); gpgme_release(context);
    return 0;
}
void v_native_libraries_finish(void) { curl_global_cleanup(); }

int32_t v_native_libraries_sandbox(void) {
    scmp_filter_ctx filter=seccomp_init(SCMP_ACT_ALLOW); CHECK(filter);
    CHECK(seccomp_rule_add(filter,SCMP_ACT_ERRNO(EACCES),SCMP_SYS(chdir),0)==0);
    CHECK(seccomp_load(filter)==0);
    errno=0; CHECK(chdir("/")==-1 && errno==EACCES);
    seccomp_release(filter);
    return 0;
}
int32_t v_native_libraries_reject_certificate(const char *url,const char *certificate) {
    CURL *c=curl_easy_init(); CHECK(c);
    CHECK(!curl_easy_setopt(c,CURLOPT_URL,url));
    CHECK(!curl_easy_setopt(c,CURLOPT_CAINFO,certificate));
    CHECK(!curl_easy_setopt(c,CURLOPT_TIMEOUT,5L));
    CHECK(!curl_easy_setopt(c,CURLOPT_NOSIGNAL,1L));
    CURLcode code=curl_easy_perform(c);
    CHECK(code==CURLE_PEER_FAILED_VERIFICATION);
    curl_easy_cleanup(c); return 0;
}
