#include "native_cpp_probe.h"
#include <algorithm>
#include <atomic>
#include <cerrno>
#include <charconv>
#include <chrono>
#include <condition_variable>
#include <cstdio>
#include <cstring>
#include <csignal>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <locale>
#include <map>
#include <mutex>
#include <new>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>
#include <fcntl.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <quadmath.h>
#define CHECK(c) do { if(!(c))return __LINE__; } while(0)
namespace {
std::string final_path;
bool constructed;
struct lifecycle {
    lifecycle() {
        try { std::ostringstream out; out<<17; constructed=out.str()=="17"; }
        catch(const std::exception& error) { std::fprintf(stderr,"native C++ constructor: %s\n",error.what()); }
    }
    ~lifecycle() {
        if(final_path.empty())return;
        int fd=::open(final_path.c_str(),O_WRONLY|O_CREAT|O_TRUNC,0600);
        if(fd>=0) { const char message[]="native C++ destructor\n"; ::write(fd,message,sizeof message-1); ::close(fd); }
    }
} lifetime;
std::atomic<int> local_destructors{0};
struct local_lifetime { ~local_lifetime() { ++local_destructors; } int value=37; };
thread_local local_lifetime local;


extern "C" void v_cpp_with_cleanup(void (*)(void *), void *, void (*)(void *));
struct cancel_state {
    std::atomic<int> ready{0}, count{0}, caught{0}, spins{0};
    int order[3]{};
    int kind=0, fd=-1;
};
struct cancel_lifetime {
    cancel_state *state; int index;
    ~cancel_lifetime() { state->order[state->count.fetch_add(1)]=index; }
};
void cancel_callback(void *argument) {
    auto *state=static_cast<cancel_state *>(argument);
    state->order[state->count.fetch_add(1)]=1;
}
void cancel_body(void *argument) {
    auto *state=static_cast<cancel_state *>(argument);
    try {
        cancel_lifetime inner{state,0};
        if(state->kind==2 && ::pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,nullptr))std::terminate();
        state->ready.store(1);
        if(state->kind==2)for(;;)state->spins.fetch_add(1,std::memory_order_relaxed);
        if(state->kind==3)::pthread_exit(reinterpret_cast<void *>(uintptr_t(0xace)));
        char byte; (void)::read(state->fd,&byte,1);
    } catch(...) { ++state->caught; throw; }
}
void *cancel_start(void *argument) {
    auto *state=static_cast<cancel_state *>(argument);
    cancel_lifetime outer{state,2};
    if(local.value!=37)std::terminate();
    if(state->kind==1 || state->kind==3)v_cpp_with_cleanup(cancel_body,state,cancel_callback);
    else {
        pthread_cleanup_push(cancel_callback,state);
        cancel_body(state);
        pthread_cleanup_pop(0);
    }
    return nullptr;
}
int cancellation_cpp_probe() {
    const int initial=local_destructors.load();
    for(int kind=0;kind<4;++kind) {
        cancel_state state; state.kind=kind;
        int descriptors[2]; CHECK(!::pipe(descriptors)); state.fd=descriptors[0];
        pthread_t thread; CHECK(!::pthread_create(&thread,nullptr,cancel_start,&state));
        for(int i=0;i<3000 && !state.ready.load();++i)std::this_thread::sleep_for(std::chrono::milliseconds(1));
        CHECK(state.ready.load());
        if(kind!=3)CHECK(!::pthread_cancel(thread));
        void *result=nullptr; CHECK(!::pthread_join(thread,&result));
        CHECK(result==(kind==3?reinterpret_cast<void *>(uintptr_t(0xace)):PTHREAD_CANCELED));
        CHECK(state.count==3 && state.order[0]==0 && state.order[1]==1 && state.order[2]==2 && state.caught==1);
        CHECK(local_destructors==initial+kind+1);
        CHECK(!::close(descriptors[0]) && !::close(descriptors[1]));
    }
    return 0;
}

int float128_probe() {
    const char *text="0x1.0000000000000000000000000001p+0tail"; char *end=nullptr;
    __float128 value=::strtof128(text,&end); uint64_t words[2]; std::memcpy(words,&value,sizeof words);
    CHECK(!std::strcmp(end,"tail") && words[0]==1 && words[1]==UINT64_C(0x3fff000000000000));
    char output[256];
    CHECK(::strfromf128(output,sizeof output,"%.28a",value)==35 && !std::strcmp(output,"0x1.0000000000000000000000000001p+0"));
    int length=::strfromf128(nullptr,0,"%.36g",value); CHECK(length>30 && length<256);
    CHECK(::strfromf128(output,sizeof output,"%.36g",value)==length);
    __float128 roundtrip=::strtof128(output,&end); CHECK(!*end);
    uint64_t back[2]; std::memcpy(back,&roundtrip,sizeof back); CHECK(back[0]==words[0] && back[1]==words[1]);
    struct { char before; char text[4]; char after; } guard={'a',{},'z'};
    CHECK(::strfromf128(guard.text,4,"%.36g",value)==length && !guard.text[3] && guard.before=='a' && guard.after=='z');
    errno=0; CHECK(::strfromf128(output,sizeof output,"%.2147483648g",value)==-1 && errno==EOVERFLOW);
    value=::strtof128("0x1p-16494",&end); std::memcpy(words,&value,sizeof words);
    CHECK(!*end && words[0]==1 && words[1]==0);
    errno=0; value=::strtof128("0x1p16384",&end); std::memcpy(words,&value,sizeof words);
    CHECK(!*end && !words[0] && words[1]==UINT64_C(0x7fff000000000000) && errno==ERANGE);
    const char *invalid[]={"%d","%+g","%Lg","%gextra","%","%."};
    for(const char *format:invalid) {
        pid_t child=::fork(); CHECK(child>=0);
        if(!child) {
            struct rlimit no_core={0,0}; ::setrlimit(RLIMIT_CORE,&no_core);
            int sink=::open("/dev/null",O_WRONLY); if(sink>=0) { ::dup2(sink,2); ::close(sink); }
            ::strfromf128(output,sizeof output,format,value); ::_exit(99);
        }
        int status=0; CHECK(::waitpid(child,&status,0)==child && WIFSIGNALED(status) && WTERMSIG(status)==SIGABRT);
    }
    return 0;
}
}
extern "C" int32_t v_native_cpp_probe(const char *directory) {
    try {
        CHECK(constructed);
        final_path=std::string(directory)+"/destructor.marker";
        std::vector<int> values={7,3,11,1}; std::sort(values.begin(),values.end()); CHECK(values.front()==1 && values.back()==11);
        std::map<std::string,int> index; index["Ж"]=17; CHECK(index.at("Ж")==17);
        int unwound=0;
        struct cleanup { int& count; ~cleanup() { ++count; } };
        try { cleanup guard{unwound}; (void)values.at(100); CHECK(false); }
        catch(const std::out_of_range&) { CHECK(unwound==1); }
        std::smatch match; std::string input="name-314";
        CHECK(std::regex_match(input,match,std::regex("([a-z]+)-([0-9]+)")) && match[1]=="name" && match[2]=="314");
        std::locale utf("C.UTF-8");
        std::wostringstream wide; wide.imbue(utf); wide<<L"Ж "<<std::fixed<<std::setprecision(1)<<12.5;
        CHECK(wide.str()==L"Ж 12.5");
        CHECK(std::use_facet<std::ctype<wchar_t>>(utf).tolower(L'Ж')==L'ж');
        std::tm date{}; std::wistringstream date_input(L"Thursday February 29 2024"); date_input.imbue(utf);
        date_input>>std::get_time(&date,L"%A %B %d %Y");
        CHECK(!date_input.fail() && date.tm_year==124 && date.tm_mon==1 && date.tm_mday==29 && date.tm_wday==4);
        std::wostringstream date_output; date_output.imbue(utf); date_output<<std::put_time(&date,L"%A %B %d %Y");
        CHECK(date_output.str()==L"Thursday February 29 2024");
        std::ostringstream numeric; numeric.imbue(utf); numeric<<std::fixed<<std::setprecision(3)<<12.125;
        CHECK(numeric.str()=="12.125");
        long double precise=1.0L+0x1p-60L,restored=0; char buffer[256];
        auto formatted=std::to_chars(buffer,buffer+sizeof buffer,precise,std::chars_format::hex);
        CHECK(formatted.ec==std::errc{});
        auto parsed=std::from_chars(buffer,formatted.ptr,restored,std::chars_format::hex);
        CHECK(parsed.ec==std::errc{} && parsed.ptr==formatted.ptr && restored==precise);
        const auto path=std::filesystem::path(directory)/"native-Ж.txt";
        const std::string content=std::string(8192,'x')+"UTF-8 Ж\n";
        { std::ofstream file(path,std::ios::binary); CHECK(file); file<<content; CHECK(file.good()); }
        { std::ifstream file(path,std::ios::binary); std::string read((std::istreambuf_iterator<char>(file)),{}); CHECK(read==content); }
        CHECK(std::filesystem::file_size(path)==content.size() && std::filesystem::exists(path));
        int entries=0; for(const auto& entry:std::filesystem::directory_iterator(directory))if(entry.path()==path)++entries;
        CHECK(entries==1); std::filesystem::remove(path);
        struct alignas(64) aligned { int value=73; };
        void *(*volatile allocate_aligned)(std::size_t,std::align_val_t)=::operator new;
        using aligned_delete=void (*)(void *,std::align_val_t) noexcept;
        aligned_delete volatile release_aligned=::operator delete;
        void *raw=allocate_aligned(sizeof(aligned),std::align_val_t{64});
        CHECK(reinterpret_cast<uintptr_t>(raw)%64==0);
        auto over_aligned=new(raw) aligned; CHECK(over_aligned->value==73);
        over_aligned->~aligned(); release_aligned(raw,std::align_val_t{64});
        std::recursive_mutex recursive; recursive.lock(); CHECK(recursive.try_lock()); recursive.unlock(); recursive.unlock();
        std::mutex mutex; std::condition_variable condition; bool ready=false;
        std::atomic<int> sum{0}; std::vector<std::thread> workers;
        for(int i=0;i<2;i++)workers.emplace_back([&] {
            if(local.value!=37)std::terminate();
            std::unique_lock<std::mutex> lock(mutex); condition.wait(lock,[&]{return ready;}); sum+=local.value;
        });
        { std::lock_guard<std::mutex> lock(mutex); ready=true; } condition.notify_all();
        for(auto& worker:workers)worker.join(); CHECK(sum==74 && local_destructors==2);
        { std::unique_lock<std::mutex> lock(mutex); CHECK(condition.wait_for(lock,std::chrono::milliseconds(5))==std::cv_status::timeout); }
        int code=float128_probe(); if(code)return code;
        code=cancellation_cpp_probe(); if(code)return code;
        std::cout<<"native C++ streams, locales, filesystem, shared exceptions, threads, cancellation and binary128 passed\n";
        return 0;
    } catch(const std::exception& error) {
        std::fprintf(stderr,"native C++ probe: %s\n",error.what()); return __LINE__;
    }
}
