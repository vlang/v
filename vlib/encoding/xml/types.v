module xml

pub type XMLNodeContents = XMLCData | XMLComment | XMLNode | string

pub struct XMLCData {
pub:
	text string @[required]
}

pub struct XMLComment {
pub:
	text string @[required]
}

// XMLNode represents a single XML node. It contains the node name,
// a map of attributes, and a list of children. The children can be
// other XML nodes, CDATA, plain text, or comments.
pub struct XMLNode {
pub:
	name       string @[required]
	attributes map[string]string
	children   []XMLNodeContents
}

// XMLDocument is the struct that represents a single XML document.
// It contains the prolog and the single root node. The prolog struct
// is embedded into the XMLDocument struct, so that the prolog fields
// are accessible directly from the this struct.
// Public prolog fields include version, enccoding, comments preceding
// the root node, and the document type definition.
pub struct XMLDocument {
	Prolog
pub:
	root XMLNode @[required]
}

pub type DTDListItem = DTDElement | DTDEntity

pub struct DTDEntity {
pub:
	name  string @[required]
	value string @[required]
}

pub struct DTDElement {
pub:
	name       string   @[required]
	definition []string @[required]
}

pub struct DocumentTypeDefinition {
pub:
	name string
	list []DTDListItem
}

pub struct DocumentType {
pub:
	name string @[required]
	dtd  DTDInfo
}

type DTDInfo = DocumentTypeDefinition | string

struct Prolog {
	parsed_reverse_entities map[string]string = default_entities_reverse.clone()
pub:
	version  string       = '1.0'
	encoding string       = 'UTF-8'
	doctype  DocumentType = DocumentType{
		name: ''
		dtd:  ''
	}
	comments []XMLComment
}
