module token

// NOTE: add keyword tokens here
// TODO: allow overriding this method in main v compiler
// that is why this method was renamed from `from_string`
// vfmt off
@[direct_array_access]
pub fn Token.from_string_tinyv(name string) Token {
	match name.len {
		2 {
			match name[0] {
				`a` {
					match name[1] {
						`s` { return .key_as }
						else { return .name }
					}
				}
				`f` {
					match name[1] {
						`n` { return .key_fn }
						else { return .name }
					}
				}
				`g` {
					match name[1] {
						`o` { return .key_go }
						else { return .name }
					}
				}
				`i` {
					match name[1] {
						`f` { return .key_if }
						`n` { return .key_in }
						`s` { return .key_is }
						else { return .name }
					}
				}
				`o` {
					match name[1] {
						`r` { return .key_or }
						else { return .name }
					}
				}
				else {
					return .name
				}
			}
			return .name
		}
		3 {
			match name[0] {
				`a` {
					if name[1] == `s` && name[2] == `m` {
						return .key_asm
					} else {
						return .name
					}
				}
				`f` {
					if name[1] == `o` && name[2] == `r` {
						return .key_for
					} else {
						return .name
					}
				}
				`m` {
					if name[1] == `u` && name[2] == `t` {
						return .key_mut
					} else {
						return .name
					}
				}
				`n` {
					if name[1] == `i` && name[2] == `l` {
						return .key_nil
					} else {
						return .name
					}
				}
				`p` {
					if name[1] == `u` && name[2] == `b` {
						return .key_pub
					} else {
						return .name
					}
				}
				else {
					return .name
				}
			}
			return .name
		}
		4 {
			match name[0] {
				`d` {
					if name[1] == `u` && name[2] == `m` && name[3] == `p` {
						return .key_dump
					} else {
						return .name
					}
				}
				`e` {
					match name[1] {
						`l` {
							if name[2] == `s` && name[3] == `e` {
								return .key_else
							} else {
								return .name
							}
						}
						`n` {
							if name[2] == `u` && name[3] == `m` {
								return .key_enum
							} else {
								return .name
							}
						}
						else {
							return .name
						}
					}
				}
				`g` {
					if name[1] == `o` && name[2] == `t` && name[3] == `o` {
						return .key_goto
					} else {
						return .name
					}
				}
				`l` {
					if name[1] == `o` && name[2] == `c` && name[3] == `k` {
						return .key_lock
					} else {
						return .name
					}
				}
				`n` {
					if name[1] == `o` && name[2] == `n` && name[3] == `e` {
						return .key_none
					} else {
						return .name
					}
				}
				`t` {
					match name[1] {
						`r` {
							if name[2] == `u` && name[3] == `e` {
								return .key_true
							} else {
								return .name
							}
						}
						`y` {
							if name[2] == `p` && name[3] == `e` {
								return .key_type
							} else {
								return .name
							}
						}
						else {
							return .name
						}
					}
				}
				else {
					return .name
				}
			}
		}
		5 {
			match name[0] {
				`b` {
					if name[1] == `r` && name[2] == `e` && name[3] == `a` && name[4] == `k` {
						return .key_break
					} else {
						return .name
					}
				}
				`c` {
					if name[1] == `o` && name[2] == `n` && name[3] == `s` && name[4] == `t` {
						return .key_const
					} else {
						return .name
					}
				}
				`d` {
					if name[1] == `e` && name[2] == `f` && name[3] == `e` && name[4] == `r` {
						return .key_defer
					} else {
						return .name
					}
				}
				`f` {
					if name[1] == `a` && name[2] == `l` && name[3] == `s` && name[4] == `e` {
						return .key_false
					} else {
						return .name
					}
				}
				`m` {
					if name[1] == `a` && name[2] == `t` && name[3] == `c` && name[4] == `h` {
						return .key_match
					} else {
						return .name
					}
				}
				`r` {
					if name[1] == `l` && name[2] == `o` && name[3] == `c` && name[4] == `k` {
						return .key_rlock
					} else {
						return .name
					}
				}
				`s` {
					if name[1] == `p` && name[2] == `a` && name[3] == `w` && name[4] == `n` {
						return .key_spawn
					} else {
						return .name
					}
				}
				`u` {
					if name[1] == `n` && name[2] == `i` && name[3] == `o` && name[4] == `n` {
						return .key_union
					} else {
						return .name
					}
				}
				else {
					return .name
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
								return .name
							}
						}
						`t` {
							if name[2] == `o` && name[3] == `m` && name[4] == `i` && name[5] == `c` {
								return .key_atomic
							} else {
								return .name
							}
						}
						else {
							return .name
						}
					}
				}
				`i` {
					if name[1] == `m` && name[2] == `p` && name[3] == `o` && name[4] == `r`
						&& name[5] == `t` {
						return .key_import
					} else {
						return .name
					}
				}
				`m` {
					if name[1] == `o` && name[2] == `d` && name[3] == `u` && name[4] == `l`
						&& name[5] == `e` {
						return .key_module
					} else {
						return .name
					}
				}
				`r` {
					if name[1] == `e` && name[2] == `t` && name[3] == `u` && name[4] == `r`
						&& name[5] == `n` {
						return .key_return
					} else {
						return .name
					}
				}
				`s` {
					match name[1] {
						`e` {
							if name[2] == `l` && name[3] == `e` && name[4] == `c` && name[5] == `t` {
								return .key_select
							} else {
								return .name
							}
						}
						`h` {
							if name[2] == `a` && name[3] == `r` && name[4] == `e` && name[5] == `d` {
								return .key_shared
							} else {
								return .name
							}
						}
						`i` {
							if name[2] == `z` && name[3] == `e` && name[4] == `o` && name[5] == `f` {
								return .key_sizeof
							} else {
								return .name
							}
						}
						`t` {
							match name[2] {
								`a` {
									if name[3] == `t` && name[4] == `i` && name[5] == `c` {
										return .key_static
									} else {
										return .name
									}
								}
								`r` {
									if name[3] == `u` && name[4] == `c` && name[5] == `t` {
										return .key_struct
									} else {
										return .name
									}
								}
								else {
									return .name
								}
							}
						}
						else {
							return .name
						}
					}
				}
				`t` {
					if name[1] == `y` && name[2] == `p` && name[3] == `e` && name[4] == `o`
						&& name[5] == `f` {
						return .key_typeof
					} else {
						return .name
					}
				}
				`u` {
					if name[1] == `n` && name[2] == `s` && name[3] == `a` && name[4] == `f`
						&& name[5] == `e` {
						return .key_unsafe
					} else {
						return .name
					}
				}
				else {
					return .name
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
								return .name
							}
						}
						`l` {
							if name[2] == `i` && name[3] == `k` && name[4] == `e` && name[5] == `l`
								&& name[6] == `y` && name[7] == `_` {
								return .key_likely
							} else {
								return .name
							}
						}
						else {
							return .name
						}
					}
				}
				`c` {
					if name[1] == `o` && name[2] == `n` && name[3] == `t` && name[4] == `i`
						&& name[5] == `n` && name[6] == `u` && name[7] == `e` {
						return .key_continue
					} else {
						return .name
					}
				}
				`v` {
					if name[1] == `o` && name[2] == `l` && name[3] == `a` && name[4] == `t`
						&& name[5] == `i` && name[6] == `l` && name[7] == `e` {
						return .key_volatile
					} else {
						return .name
					}
				}
				else {
					return .name
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
								return .name
							}
						}
						`s` {
							if name[2] == `r` && name[3] == `e` && name[4] == `f` && name[5] == `t`
								&& name[6] == `y` && name[7] == `p` && name[8] == `e` {
								return .key_isreftype
							} else {
								return .name
							}
						}
						else {
							return .name
						}
					}
				}
				else {
					return .name
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
								return .name
							}
						}
						`u` {
							if name[2] == `n` && name[3] == `l` && name[4] == `i` && name[5] == `k`
								&& name[6] == `e` && name[7] == `l` && name[8] == `y`
								&& name[9] == `_` {
								return .key_unlikely
							} else {
								return .name
							}
						}
						else {
							return .name
						}
					}
				}
				else {
					return .name
				}
			}
		}
		else {
			return .name
		}
	}
}
// vfmt on
