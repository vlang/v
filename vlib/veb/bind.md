# Request binding

`ctx.bind[T]()` decodes a JSON body when `Content-Type` is `application/json`.
The media type is case-insensitive, and parameters such as `charset=utf-8` are ignored.
Other media types fill scalar `string`, `int`, and `bool` fields from the query and form,
with form values taking precedence. Binding validates `required`, `min`, `max`, and `email`
attributes and returns an error when decoding or validation fails.
