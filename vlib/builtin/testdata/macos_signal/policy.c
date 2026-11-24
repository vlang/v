#include <errno.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>

typedef void (*handler_t)(int);
typedef handler_t (*signal_api_t)(int, handler_t);
static signal_api_t volatile signal_api = &signal;

struct input_action {
	handler_t handler;
	void (*trampoline)(void);
	uint32_t mask;
	int32_t flags;
};
struct previous_action {
	handler_t handler;
	uint32_t mask;
	int32_t flags;
};
_Static_assert(sizeof(struct input_action) == 24, "XNU input action ABI");
_Static_assert(offsetof(struct input_action, trampoline) == 8, "XNU trampoline ABI");
_Static_assert(offsetof(struct input_action, mask) == 16, "XNU input mask ABI");
_Static_assert(sizeof(struct previous_action) == 16, "XNU old action ABI");
#ifdef __APPLE__
_Static_assert(sizeof(struct __sigaction) == sizeof(struct input_action), "SDK input action ABI");
_Static_assert(sizeof(struct sigaction) == sizeof(struct previous_action), "SDK old action ABI");
_Static_assert(SA_RESTART == 2, "Darwin restart flag ABI");
#endif

#ifdef VMAC_SIGNAL_POLICY_MOCK
static struct previous_action actions[32];
static unsigned calls;
int32_t v_macos_signal_test_call(uint32_t number, uint64_t a1, uint64_t a2,
	uint64_t a3, uint64_t a4, uint64_t a5, uint64_t a6, uint64_t *result) {
	(void)result;
	if (number != 46 || a4 || a5 || a6) return EINVAL;
	if (a1 == 0 || a1 >= 32 || a1 == 9 || a1 == 17) return EINVAL;
	const struct input_action *input = (const void *)(uintptr_t)a2;
	struct previous_action *previous = (void *)(uintptr_t)a3;
	if (!input || !previous || !input->trampoline || input->mask || input->flags != 0x402)
		return EINVAL;
	*previous = actions[a1];
	actions[a1].handler = input->handler;
	actions[a1].mask = input->mask;
	actions[a1].flags = input->flags;
	calls++;
	return 0;
}
#endif

static void first(int signal_number) { (void)signal_number; }
static void second(int signal_number) { (void)signal_number; }

int v_macos_signal_policy_probe(void) {
	const int number = SIGUSR1;
	handler_t inherited = signal_api(number, SIG_DFL);
	if (inherited == SIG_ERR) return 9;
	errno = 77;
	if (signal_api(number, first) != SIG_DFL || errno != 77) return 1;
	if (signal_api(number, second) != first || errno != 77) return 2;
	if (signal_api(number, SIG_IGN) != second || errno != 77) return 3;
	if (signal_api(number, SIG_DFL) != SIG_IGN || errno != 77) return 4;
	const int invalid[] = {0, 32, 9, 17};
	for (unsigned i = 0; i < sizeof invalid / sizeof invalid[0]; i++) {
		errno = 77;
		if (signal_api(invalid[i], first) != SIG_ERR || errno != EINVAL) return 5;
	}
	errno = 77;
	if (signal_api(number, first) != SIG_DFL || errno != 77) return 6;
	if (signal_api(number, SIG_DFL) != first || errno != 77) return 7;
	if (signal_api(number, inherited) != SIG_DFL || errno != 77) return 8;
#ifdef VMAC_SIGNAL_POLICY_MOCK
	if (calls < 8) return 23;
#endif
	return 0;
}

#ifndef VMAC_SIGNAL_POLICY_MOCK
#include "../../macos_bare/kernel_services.h"
#include <stdatomic.h>

static volatile sig_atomic_t deliveries;
static volatile sig_atomic_t nested_deliveries;
static volatile sig_atomic_t delivery_error;

static int send_self(int number) {
	uint64_t pid = 0;
	int code = v_macos_kernel_bsd_call6(20, 0, 0, 0, 0, 0, 0, &pid);
	if (code) return code;
	return v_macos_kernel_bsd_call6(37, pid, (uint64_t)number, 0, 0, 0, 0, 0);
}

static void receive_second(int number) {
	if (number != SIGUSR2) delivery_error = 1;
	nested_deliveries++;
}

static void receive_first(int number) {
	if (number != SIGUSR1) delivery_error = 2;
	deliveries++;
	if (send_self(SIGUSR2)) delivery_error = 3;
}

int v_macos_signal_delivery_probe(void) {
	uint32_t original = 0;
	if (v_macos_kernel_bsd_call6(329, 3, 0, (uintptr_t)&original, 0, 0, 0, 0)) return 10;
	uint32_t unblocked = original & ~((1u << (SIGUSR1 - 1)) | (1u << (SIGUSR2 - 1)));
	if (v_macos_kernel_bsd_call6(329, 3, (uintptr_t)&unblocked, 0, 0, 0, 0, 0)) return 11;
	errno = 77;
	handler_t old_first = signal_api(SIGUSR1, receive_first);
	handler_t old_second = signal_api(SIGUSR2, receive_second);
	if (old_first == SIG_ERR || old_second == SIG_ERR) return 12;
	for (int i = 1; i <= 128; i++) {
		if (send_self(SIGUSR1)) return 13;
		if (deliveries != i || nested_deliveries != i || delivery_error) return 14;
		uint32_t current = 0;
		if (v_macos_kernel_bsd_call6(329, 3, 0, (uintptr_t)&current, 0, 0, 0, 0)) return 15;
		if (current != unblocked || errno != 77) return 16;
	}
	if (signal_api(SIGUSR1, SIG_IGN) != receive_first || send_self(SIGUSR1)) return 17;
	if (deliveries != 128 || nested_deliveries != 128 || errno != 77) return 18;
	if (signal_api(SIGUSR1, old_first) != SIG_IGN) return 19;
	if (signal_api(SIGUSR2, old_second) != receive_second) return 20;
	if (v_macos_kernel_bsd_call6(329, 3, (uintptr_t)&original, 0, 0, 0, 0, 0)) return 21;
	return errno == 77 ? 0 : 22;
}

static _Atomic uint64_t worker_id;
static _Atomic int worker_seen;
static handler_t worker_previous;

int v_macos_signal_install_worker(handler_t handler) {
	worker_previous = signal_api(SIGUSR1, handler);
	return worker_previous == SIG_ERR ? errno : 0;
}

int v_macos_signal_restore_worker(void) {
	return signal_api(SIGUSR1, worker_previous) == SIG_ERR ? errno : 0;
}

void v_macos_signal_expect_worker(uint64_t id) {
	atomic_store_explicit(&worker_id, id, memory_order_relaxed);
	atomic_store_explicit(&worker_seen, 0, memory_order_release);
}

void v_macos_signal_note_worker(int number, uint64_t id, int bf_error, int native_error) {
	int good = number == SIGUSR1 && id != 0 &&
		id == atomic_load_explicit(&worker_id, memory_order_relaxed) &&
		bf_error == 9 && native_error == 77;
	atomic_store_explicit(&worker_seen, good ? 1 : -1, memory_order_release);
}

int v_macos_signal_worker_seen(void) {
	return atomic_load_explicit(&worker_seen, memory_order_acquire);
}
#endif
