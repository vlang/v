# Recovering concrete error pointers

A concrete error boxed as `IError` can be recovered with `err as &module.ErrorType`.
The pointer refers to the boxed error object. This works for imported error types and
inside an `err is module.ErrorType` branch as well as outside it.

Casting back to a concrete pointer preserves the object's identity. Returning that pointer
as `IError` preserves the error's concrete type and fields.
