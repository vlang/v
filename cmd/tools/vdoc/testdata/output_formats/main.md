# module main


## Contents
- [Constants](#Constants)
- [abc](#abc)
- [def](#def)
- [xyz](#xyz)
- [MyXMLDocument.abc](#MyXMLDocument.abc)
- [MyXMLDocument.from_file](#MyXMLDocument.from_file)
- [MyXMLDocument.from_text](#MyXMLDocument.from_text)
- [MyXMLDocument](#MyXMLDocument)
  - [instance_from_file](#instance_from_file)
  - [instance_from_text](#instance_from_text)
  - [instance_abc](#instance_abc)
  - [instance_void](#instance_void)
  - [instance_int](#instance_int)
  - [instance_result](#instance_result)
  - [instance_option](#instance_option)

## Constants
```v
const omega = 3 // should be first
```

[[Return to contents]](#Contents)

```v
const alpha = 5 // should be in the middle
```

[[Return to contents]](#Contents)

```v
const beta = 2 // should be at the end
```

[[Return to contents]](#Contents)

## abc
```v
fn abc()
```

abc - should be last

[[Return to contents]](#Contents)

## def
```v
fn def()
```

def - should be first

[[Return to contents]](#Contents)

## xyz
```v
fn xyz()
```

xyz - should be in the middle a small script <script>console.log('hello');</script> bold text <b>bold</b> end underlined text <u>underline</u> end a link [main v repo](https://github.com/vlang/v)

[[Return to contents]](#Contents)

## MyXMLDocument.abc
```v
fn MyXMLDocument.abc(text string) ?(string, int)
```

MyXMLDocument.abc does something too... I just do not know what.

[[Return to contents]](#Contents)

## MyXMLDocument.from_file
```v
fn MyXMLDocument.from_file(path string) !MyXMLDocument
```

MyXMLDocument.from_text processes the file path, and returns an error

[[Return to contents]](#Contents)

## MyXMLDocument.from_text
```v
fn MyXMLDocument.from_text(text string) ?MyXMLDocument
```

MyXMLDocument.from_text processes text and produces none

[[Return to contents]](#Contents)

## MyXMLDocument
```v
struct MyXMLDocument {
	path string
}
```

MyXMLDocument is here just to test the different combinations of methods/output types

[[Return to contents]](#Contents)

## instance_from_file
```v
fn (x &MyXMLDocument) instance_from_file(path string) !MyXMLDocument
```

instance_from_file does stuff with path

[[Return to contents]](#Contents)

## instance_from_text
```v
fn (x &MyXMLDocument) instance_from_text(text string) ?MyXMLDocument
```

instance_from_text does stuff with text

[[Return to contents]](#Contents)

## instance_abc
```v
fn (x &MyXMLDocument) instance_abc(text string) ?(string, int)
```

instance_abc does stuff too

[[Return to contents]](#Contents)

## instance_void
```v
fn (x &MyXMLDocument) instance_void()
```

instance_void does stuff too

[[Return to contents]](#Contents)

## instance_int
```v
fn (x &MyXMLDocument) instance_int() int
```

instance_int does stuff too

[[Return to contents]](#Contents)

## instance_result
```v
fn (x &MyXMLDocument) instance_result() !
```

instance_error does stuff too

[[Return to contents]](#Contents)

## instance_option
```v
fn (x &MyXMLDocument) instance_option() ?
```

instance_option does stuff too

[[Return to contents]](#Contents)

#### Powered by vdoc.
