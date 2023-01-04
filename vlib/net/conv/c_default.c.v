module conv

#include <arpa/inet.h>

fn C.htonl(host u32) u32
fn C.htons(host u16) u16

fn C.ntohl(net u32) u32
fn C.ntohs(net u16) u16
