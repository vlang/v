chdir('examples/password')!
assert execute('./correct.expect').exit_code == 0
assert execute('./incorrect.expect').exit_code == 0
