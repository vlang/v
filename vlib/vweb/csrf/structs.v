// This module provides csrf-protection for apps written with libe vweb.

module csrf

import vweb
import net.http

type CsrfCookie = http.Cookie

interface CheckedApp {}

pub struct App {
	vweb.Context
	csrf_cookie_value string
}

pub struct HttpOnly {
	http_only bool
}

struct CsrfError {
	Error
	m string
}

fn (err CsrfError) msg() string {
	return err.m
}

// Written by flopetautschnig (floscodes) 2022
