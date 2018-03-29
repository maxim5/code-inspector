//
//	data/schema.go
//	goyaml
//
//	Created by Ross Light on 2010-07-03.
//

package data

import (
	"bitbucket.org/zombiezen/goray/yaml/parser"
	"errors"
	"regexp"
)

// Canonical YAML tags
const (
	StringTag   = parser.DefaultPrefix + "str"
	SequenceTag = parser.DefaultPrefix + "seq"
	MappingTag  = parser.DefaultPrefix + "map"

	NullTag  = parser.DefaultPrefix + "null"
	BoolTag  = parser.DefaultPrefix + "bool"
	IntTag   = parser.DefaultPrefix + "int"
	FloatTag = parser.DefaultPrefix + "float"
)

// A Schema determines the tag for a node without an explicit tag.
type Schema parser.Schema

// SchemaFunc is a function type that fulfills the Schema interface.
type SchemaFunc func(node parser.Node, userData interface{}) (tag string, err error)

func (f SchemaFunc) Resolve(n parser.Node, userData interface{}) (string, error) {
	return f(n, userData)
}

// Schemas from the YAML 1.2 specification
var (
	FailsafeSchema Schema = SchemaFunc(failsafeSchema)
	CoreSchema            = SchemaFunc(coreSchema)
)

func failsafeSchema(node parser.Node, userData interface{}) (tag string, err error) {
	switch node.(type) {
	case *parser.Scalar:
		tag = StringTag
	case *parser.Sequence:
		tag = SequenceTag
	case *parser.Mapping:
		tag = MappingTag
	default:
		err = errors.New("Unrecognized node given to failsafe schema")
	}
	return
}

var (
	csNullPat  = regexp.MustCompile(`^(null|Null|NULL|~)$`)
	csBoolPat  = regexp.MustCompile(`^(true|True|TRUE)$|^(false|False|FALSE)$`)
	csDecPat   = regexp.MustCompile(`^([\-+]?)([0-9]+)$`)
	csOctPat   = regexp.MustCompile(`^0o([0-7]+)$`)
	csHexPat   = regexp.MustCompile(`^0x([0-9a-fA-F]+)$`)
	csFloatPat = regexp.MustCompile(`^([\-+]?)(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][\-+]?[0-9]+)?$`)
	csInfPat   = regexp.MustCompile(`^([\-+]?)\.(inf|Inf|INF)$`)
	csNanPat   = regexp.MustCompile(`^\.(nan|NaN|NAN)$`)
)

func coreSchema(node parser.Node, userData interface{}) (tag string, err error) {
	if scalar, ok := node.(*parser.Scalar); ok {
		s := scalar.String()
		switch {
		case csNullPat.MatchString(s):
			return NullTag, nil
		case csBoolPat.MatchString(s):
			return BoolTag, nil
		case csDecPat.MatchString(s) || csOctPat.MatchString(s) || csHexPat.MatchString(s):
			return IntTag, nil
		case csFloatPat.MatchString(s) || csInfPat.MatchString(s) || csNanPat.MatchString(s):
			return FloatTag, nil
		default:
			return StringTag, nil
		}
	}

	if _, ok := node.(*parser.Empty); ok {
		return NullTag, nil
	}

	return failsafeSchema(node, userData)
}
