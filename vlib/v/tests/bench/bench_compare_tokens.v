import v.token
import benchmark

const max_repetitions = 4_000_000

fn main() {
	mut res := token.Kind.unknown
	km_trie := token.new_keywords_matcher_trie(token.keywords)
	for kw in ['for', 'val', 'int', 'f32', 'struct', 'return', 'if', 'in', 'as', 'or', 'else',
		'unsafe', 'return', 'assert', 'Abc', 'my_identifier', 'a', 'assez', 'returned'] {
		mut bmark := benchmark.start()
		for _ in 0 .. max_repetitions {
			res = token.keywords[kw]
		}
		bmark.measure('${max_repetitions} repetitions of token.keywords["${kw}"] = ${res}')

		for _ in 0 .. max_repetitions {
			res = unsafe { token.Kind(km_trie.find(kw)) }
		}
		bmark.measure('${max_repetitions} repetitions of km_trie.find("${kw}") = ${res}')

		for _ in 0 .. max_repetitions {
			res = from_string(kw)
		}
		bmark.measure('${max_repetitions} repetitions of from_string("${kw}") = ${res}')

		println('--------------------------------')
	}
}

@[direct_array_access]
fn from_string(name string) token.Kind {
	match name.len {
		2 {
			match name[0] {
				`a` {
					match name[1] {
						`s` { return .key_as }
						else { return .unknown }
					}
				}
				`f` {
					match name[1] {
						`n` { return .key_fn }
						else { return .unknown }
					}
				}
				`g` {
					match name[1] {
						`o` { return .key_go }
						else { return .unknown }
					}
				}
				`i` {
					match name[1] {
						`f` { return .key_if }
						`n` { return .key_in }
						`s` { return .key_is }
						else { return .unknown }
					}
				}
				`o` {
					match name[1] {
						`r` { return .key_orelse }
						else { return .unknown }
					}
				}
				else {
					return .unknown
				}
			}
			return .unknown
		}
		3 {
			match name[0] {
				`a` {
					if name[1] == `s` && name[2] == `m` {
						return .key_asm
					} else {
						return .unknown
					}
				}
				`f` {
					if name[1] == `o` && name[2] == `r` {
						return .key_for
					} else {
						return .unknown
					}
				}
				`m` {
					if name[1] == `u` && name[2] == `t` {
						return .key_mut
					} else {
						return .unknown
					}
				}
				`n` {
					if name[1] == `i` && name[2] == `l` {
						return .key_nil
					} else {
						return .unknown
					}
				}
				`p` {
					if name[1] == `u` && name[2] == `b` {
						return .key_pub
					} else {
						return .unknown
					}
				}
				else {
					return .unknown
				}
			}
			return .unknown
		}
		4 {
			match name[0] {
				`d` {
					if name[1] == `u` && name[2] == `m` && name[3] == `p` {
						return .key_dump
					} else {
						return .unknown
					}
				}
				`e` {
					match name[1] {
						`l` {
							if name[2] == `s` && name[3] == `e` {
								return .key_else
							} else {
								return .unknown
							}
						}
						`n` {
							if name[2] == `u` && name[3] == `m` {
								return .key_enum
							} else {
								return .unknown
							}
						}
						else {
							return .unknown
						}
					}
				}
				`g` {
					if name[1] == `o` && name[2] == `t` && name[3] == `o` {
						return .key_goto
					} else {
						return .unknown
					}
				}
				`l` {
					if name[1] == `o` && name[2] == `c` && name[3] == `k` {
						return .key_lock
					} else {
						return .unknown
					}
				}
				`n` {
					if name[1] == `o` && name[2] == `n` && name[3] == `e` {
						return .key_none
					} else {
						return .unknown
					}
				}
				`t` {
					match name[1] {
						`r` {
							if name[2] == `u` && name[3] == `e` {
								return .key_true
							} else {
								return .unknown
							}
						}
						`y` {
							if name[2] == `p` && name[3] == `e` {
								return .key_type
							} else {
								return .unknown
							}
						}
						else {
							return .unknown
						}
					}
				}
				else {
					return .unknown
				}
			}
		}
		5 {
			match name[0] {
				`b` {
					if name[1] == `r` && name[2] == `e` && name[3] == `a` && name[4] == `k` {
						return .key_break
					} else {
						return .unknown
					}
				}
				`c` {
					if name[1] == `o` && name[2] == `n` && name[3] == `s` && name[4] == `t` {
						return .key_const
					} else {
						return .unknown
					}
				}
				`d` {
					if name[1] == `e` && name[2] == `f` && name[3] == `e` && name[4] == `r` {
						return .key_defer
					} else {
						return .unknown
					}
				}
				`f` {
					if name[1] == `a` && name[2] == `l` && name[3] == `s` && name[4] == `e` {
						return .key_false
					} else {
						return .unknown
					}
				}
				`m` {
					if name[1] == `a` && name[2] == `t` && name[3] == `c` && name[4] == `h` {
						return .key_match
					} else {
						return .unknown
					}
				}
				`r` {
					if name[1] == `l` && name[2] == `o` && name[3] == `c` && name[4] == `k` {
						return .key_rlock
					} else {
						return .unknown
					}
				}
				`s` {
					if name[1] == `p` && name[2] == `a` && name[3] == `w` && name[4] == `n` {
						return .key_spawn
					} else {
						return .unknown
					}
				}
				`u` {
					if name[1] == `n` && name[2] == `i` && name[3] == `o` && name[4] == `n` {
						return .key_union
					} else {
						return .unknown
					}
				}
				else {
					return .unknown
				}
			}
		}
		6 {
			match name[0] {
				`a` {
					match name[1] {
						`s` {
							if name[2] == `s` && name[3] == `e` && name[4] == `r` && name[5] == `t` {
								return .key_assert
							} else {
								return .unknown
							}
						}
						`t` {
							if name[2] == `o` && name[3] == `m` && name[4] == `i` && name[5] == `c` {
								return .key_atomic
							} else {
								return .unknown
							}
						}
						else {
							return .unknown
						}
					}
				}
				`i` {
					if name[1] == `m` && name[2] == `p` && name[3] == `o` && name[4] == `r`
						&& name[5] == `t` {
						return .key_import
					} else {
						return .unknown
					}
				}
				`m` {
					if name[1] == `o` && name[2] == `d` && name[3] == `u` && name[4] == `l`
						&& name[5] == `e` {
						return .key_module
					} else {
						return .unknown
					}
				}
				`r` {
					if name[1] == `e` && name[2] == `t` && name[3] == `u` && name[4] == `r`
						&& name[5] == `n` {
						return .key_return
					} else {
						return .unknown
					}
				}
				`s` {
					match name[1] {
						`e` {
							if name[2] == `l` && name[3] == `e` && name[4] == `c` && name[5] == `t` {
								return .key_select
							} else {
								return .unknown
							}
						}
						`h` {
							if name[2] == `a` && name[3] == `r` && name[4] == `e` && name[5] == `d` {
								return .key_shared
							} else {
								return .unknown
							}
						}
						`i` {
							if name[2] == `z` && name[3] == `e` && name[4] == `o` && name[5] == `f` {
								return .key_sizeof
							} else {
								return .unknown
							}
						}
						`t` {
							match name[2] {
								`a` {
									if name[3] == `t` && name[4] == `i` && name[5] == `c` {
										return .key_static
									} else {
										return .unknown
									}
								}
								`r` {
									if name[3] == `u` && name[4] == `c` && name[5] == `t` {
										return .key_struct
									} else {
										return .unknown
									}
								}
								else {
									return .unknown
								}
							}
						}
						else {
							return .unknown
						}
					}
				}
				`t` {
					if name[1] == `y` && name[2] == `p` && name[3] == `e` && name[4] == `o`
						&& name[5] == `f` {
						return .key_typeof
					} else {
						return .unknown
					}
				}
				`u` {
					if name[1] == `n` && name[2] == `s` && name[3] == `a` && name[4] == `f`
						&& name[5] == `e` {
						return .key_unsafe
					} else {
						return .unknown
					}
				}
				else {
					return .unknown
				}
			}
		}
		8 {
			match name[0] {
				`_` {
					match name[1] {
						`_` {
							if name[2] == `g` && name[3] == `l` && name[4] == `o` && name[5] == `b`
								&& name[6] == `a` && name[7] == `l` {
								return .key_global
							} else {
								return .unknown
							}
						}
						`l` {
							if name[2] == `i` && name[3] == `k` && name[4] == `e` && name[5] == `l`
								&& name[6] == `y` && name[7] == `_` {
								return .key_likely
							} else {
								return .unknown
							}
						}
						else {
							return .unknown
						}
					}
				}
				`c` {
					if name[1] == `o` && name[2] == `n` && name[3] == `t` && name[4] == `i`
						&& name[5] == `n` && name[6] == `u` && name[7] == `e` {
						return .key_continue
					} else {
						return .unknown
					}
				}
				`v` {
					if name[1] == `o` && name[2] == `l` && name[3] == `a` && name[4] == `t`
						&& name[5] == `i` && name[6] == `l` && name[7] == `e` {
						return .key_volatile
					} else {
						return .unknown
					}
				}
				else {
					return .unknown
				}
			}
		}
		9 {
			match name[0] {
				`i` {
					match name[1] {
						`n` {
							if name[2] == `t` && name[3] == `e` && name[4] == `r` && name[5] == `f`
								&& name[6] == `a` && name[7] == `c` && name[8] == `e` {
								return .key_interface
							} else {
								return .unknown
							}
						}
						`s` {
							if name[2] == `r` && name[3] == `e` && name[4] == `f` && name[5] == `t`
								&& name[6] == `y` && name[7] == `p` && name[8] == `e` {
								return .key_isreftype
							} else {
								return .unknown
							}
						}
						else {
							return .unknown
						}
					}
				}
				else {
					return .unknown
				}
			}
		}
		10 {
			match name[0] {
				`_` {
					match name[1] {
						`_` {
							if name[2] == `o` && name[3] == `f` && name[4] == `f` && name[5] == `s`
								&& name[6] == `e` && name[7] == `t` && name[8] == `o`
								&& name[9] == `f` {
								return .key_offsetof
							} else {
								return .unknown
							}
						}
						`u` {
							if name[2] == `n` && name[3] == `l` && name[4] == `i` && name[5] == `k`
								&& name[6] == `e` && name[7] == `l` && name[8] == `y`
								&& name[9] == `_` {
								return .key_unlikely
							} else {
								return .unknown
							}
						}
						else {
							return .unknown
						}
					}
				}
				else {
					return .unknown
				}
			}
		}
		else {
			return .unknown
		}
	}
}
