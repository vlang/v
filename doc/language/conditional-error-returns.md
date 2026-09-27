# Errors in conditional returns

A branch of a returned `if` or `match` expression follows the same error rules
as a direct return. An `IError` value such as `IError(io.Eof{})`
propagates failure when the function returns a different payload type, such as
`!int`. When the function returns `!IError`, an ordinary error value can itself
be the successful payload.
