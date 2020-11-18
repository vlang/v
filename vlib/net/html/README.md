# V HTML

A HTML parser made in V.

## Usage

If the description below isn't enought, please look at the test files.

### Parser

Responsible for read HTML in full strings or splited string and returns all Tag objets of
it HTML or return a DocumentObjectModel, that will try to find how the HTML Tree is.

#### split_parse(data string)
This functions is the main function called by parse method to fragment parse your HTML.

#### parse_html(data string, is_file bool)
This function is called passing a filename or a complete html data string to it.

#### add_code_tag(name string)
This function is used to add a tag for the parser ignore it's content. 
For example, if you have an html or XML with a custom tag, like `<script>`, using this function, 
like `add_code_tag('script')` will make all `script` tags content be jumped, 
so you still have its content, but will not confuse the parser with it's `>` or `<`.

#### finalize()
When using **split_parse** method, you must call this function to ends the parse completely.

#### get_tags() []Tag_ptr
This functions returns a array with all tags and it's content.

#### get_dom() DocumentObjectModel
Returns the DocumentObjectModel for current parsed tags.

### WARNING
If you want to reuse parser object to parse another HTML, call `initialize_all()` function first.

### DocumentObjectModel

A DOM object that will make easier to access some tags and search it.

#### get_by_attribute_value(name string, value string) []Tag_ptr
This function retuns a Tag array with all tags in document 
that have a attribute with given name and given value.

#### get_by_tag(name string) []Tag_ptr
This function retuns a Tag array with all tags in document that have a name with the given value.

#### get_by_attribute(name string) []Tag_ptr
This function retuns a Tag array with all tags in document that have a attribute with given name.

#### get_root() Tag_ptr
This function returns the root Tag.

#### get_all_tags() []Tag_ptr
This function returns all important tags, removing close tags.

### Tag

An object that holds tags information, such as `name`, `attributes`, `children`.

#### get_children() []Tag_ptr
Returns all children as an array.

#### get_parent() &Tag
Returns the parent of current tag.

#### get_name() string
Returns tag name.

#### get_content() string
Returns tag content.

#### get_attributes() map[string]string
Returns all attributes and it value.

#### text() string
Returns the content of the tag and all tags inside it. 
Also, any `<br>` tag will be converted into `\n`.

## Some questions that can appear

### Q: Why in parser have a `builder_str() string` method that returns only the lexeme string?
    
A: Because in early stages of the project, `strings.Builder` are used, 
but for some bug existing somewhere, it was necessary to use `string` directly. 
Later, it's planned to use `strings.Builder` again.

### Q: Why have a `compare_string(a string, b string) bool` method?

A: For some reason when using != and == in strings directly, it is not working. 
So this method is a workaround.

### Q: Will be something like `XPath`?

A: Like XPath yes. Exactly equal to it, no.

## Roadmap
- [x] Parser
  - [x] `<!-- Comments -->` detection
  - [x] `Open Generic tags` detection
  - [x] `Close Generic tags` detection
  - [x] `verify string` detection
  - [x] `tag attributes` detection
  - [x] `attributes values` detection
  - [x] `tag text` (on tag it is declared as content, maybe change for text in the future)
  - [x] `text file for parse` support (open local files for parsing)
  - [x] `open_code` verification
- [x] DocumentObjectModel
  - [x] push elements that have a close tag into stack
  - [x] remove elements from stack
  - [x] ~~create a new document root if have some syntax error (deleted)~~
  - [x] search tags in `DOM` by attributes
  - [x] search tags in `DOM` by tag type
  - [x] finish dom test

## License
[GPL3](LICENSE)
