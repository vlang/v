#ifndef __KQUEUE_H
#define __KQUEUE_H

#include <sys/event.h>

// Due to the renaming of 'struct kevent' and function 'kevent',
// they are wrapped here to avoid conflicts.
int __kevent__(int handle, const struct kevent* changelist, int nchanges, struct kevent* eventlist, int nevents, const struct timespec* timeout) {
	return kevent(handle, changelist, nchanges, eventlist, nevents, timeout);
}

#endif
