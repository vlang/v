// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// This file contains JS functions only present in the browser.
// They have been ported from their TypeScript definitions.

module builtin

// Window
fn JS.atob(string) string
fn JS.btoa(string) string
fn JS.clearInterval(int)
fn JS.clearTimeout(int)

// fn JS.createImageBitmap(ImageBitmapSource, ImageBitmapOptions) Promise<ImageBitmap>
// fn JS.createImageBitmap(ImageBitmapSource, int, int, int, int, ImageBitmapOptions) Promise<ImageBitmap>

// TODO: js async attribute
// [js_async]
// fn JS.fetch(RequestInfo, RequestInit) Promise<Response>
fn JS.queueMicrotask(fn ())
fn JS.setInterval(any, int, ...any) int
fn JS.setTimeout(any, int, ...any) int

fn JS.alert(any)
fn JS.blur()
fn JS.captureEvents()
fn JS.close()
fn JS.confirm(string) bool

// fn JS.departFocus(NavigationReason, FocusNavigationOrigin)
fn JS.focus()

// fn JS.getComputedStyle(Element, string | null) CSSStyleDeclaration
// fn JS.getMatchedCSSRules(Element, string | null) CSSRuleList
// fn JS.getSelection() Selection | null
// fn JS.matchMedia(string) MediaQueryList
fn JS.moveBy(int, int)
fn JS.moveTo(int, int)
fn JS.msWriteProfilerMark(string)

// fn JS.open(string, string, string, bool)?Window
// fn JS.postMessage(any, string, []Transferable)
fn JS.print()
fn JS.prompt(string, string) ?string
fn JS.releaseEvents()
fn JS.resizeBy(int, int)
fn JS.resizeTo(int, int)

// fn JS.scroll(ScrollToOptions)
fn JS.scroll(int, int)

// fn JS.scrollBy(ScrollToOptions)
fn JS.scrollBy(int, int)

// fn JS.scrollTo(ScrollToOptions)
fn JS.scrollTo(int, int)
fn JS.stop()
