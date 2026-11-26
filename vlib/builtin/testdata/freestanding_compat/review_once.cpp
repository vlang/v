#include <mutex>
static std::once_flag flag, nested;
static int attempts, nested_attempts;
static void initialize() {
    std::call_once(nested, [] { ++nested_attempts; });
    if (++attempts == 1) throw 7;
}
extern "C" int v_review_once_probe() {
    try { std::call_once(flag, initialize); return 1; }
    catch (int value) { if (value != 7) return 2; }
    std::call_once(flag, initialize);
    std::call_once(flag, initialize);
    return attempts == 2 && nested_attempts == 1 ? 0 : 3;
}
#ifdef HOSTED_REVIEW
int main() { return v_review_once_probe(); }
#endif
