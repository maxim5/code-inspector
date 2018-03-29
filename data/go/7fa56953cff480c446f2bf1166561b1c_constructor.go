//
//	data/constructor.go
//	goyaml
//
//	Created by Ross Light on 2010-07-04.
//

package data

import (
	"bitbucket.org/zombiezen/goray/yaml/parser"
	"errors"
	"math"
	"strconv"
)

// A Constructor converts a Node into a Go data structure.
type Constructor parser.Constructor

// ConstructorFunc is a function that implements the Constructor interface.
type ConstructorFunc func(parser.Node, interface{}) (interface{}, error)

func (f ConstructorFunc) Construct(n parser.Node, userData interface{}) (interface{}, error) {
	return f(n, userData)
}

// ConstructorMap uses constructors associated with string tags to construct a value.
type ConstructorMap map[string]Constructor

func (m ConstructorMap) Construct(n parser.Node, userData interface{}) (data interface{}, err error) {
	if c, ok := m[n.Tag()]; ok {
		return c.Construct(n, userData)
	}
	err = errors.New("Constructor has no rule for " + n.Tag())
	return
}

// DefaultConstructor constructs all of the core data types.
var DefaultConstructor Constructor = ConstructorMap{
	StringTag:   ConstructorFunc(constructStr),
	SequenceTag: ConstructorFunc(constructSeq),
	MappingTag:  ConstructorFunc(constructMap),
	NullTag:     ConstructorFunc(constructNull),
	BoolTag:     ConstructorFunc(constructBool),
	IntTag:      ConstructorFunc(constructInt),
	FloatTag:    ConstructorFunc(constructFloat),
}

func constructStr(n parser.Node, userData interface{}) (data interface{}, err error) {
	node, ok := n.(*parser.Scalar)
	if !ok {
		err = errors.New("Non-scalar given to string")
		return
	}
	data = node.String()
	return
}

func constructSeq(n parser.Node, userData interface{}) (data interface{}, err error) {
	node, ok := n.(*parser.Sequence)
	if !ok {
		err = errors.New("Non-sequence given to sequence")
		return
	}
	data = node.Slice()
	return
}

func constructMap(n parser.Node, userData interface{}) (data interface{}, err error) {
	node, ok := n.(*parser.Mapping)
	if !ok {
		err = errors.New("Non-mapping given to map")
		return
	}
	data = node.Map()
	return
}

func constructNull(n parser.Node, userData interface{}) (data interface{}, err error) {
	_, isScalar := n.(*parser.Scalar)
	_, isEmpty := n.(*parser.Empty)
	if !isScalar && !isEmpty {
		err = errors.New("Non-scalar tagged as null")
		return
	}
	return nil, nil
}

func constructBool(n parser.Node, userData interface{}) (data interface{}, err error) {
	var s string

	if scalar, ok := n.(*parser.Scalar); ok {
		s = scalar.Value
	} else {
		err = errors.New("Non-scalar tagged as bool")
		return
	}
	groups := csBoolPat.FindStringSubmatch(s)

	if len(groups) > 0 {
		if groups[1] != "" {
			return true, nil
		} else if groups[2] != "" {
			return false, nil
		}
	}

	err = errors.New("Value is an invalid boolean: " + s)
	return
}

func constructInt(n parser.Node, userData interface{}) (data interface{}, err error) {
	var s string

	if scalar, ok := n.(*parser.Scalar); ok {
		s = scalar.Value
	} else {
		err = errors.New("Non-scalar tagged as int")
		return
	}

	if csDecPat.MatchString(s) {
		return strconv.ParseInt(s, 10, 64)
	} else if groups := csHexPat.FindStringSubmatch(s); len(groups) > 0 {
		return strconv.ParseUint(groups[1], 16, 64)
	} else if groups := csOctPat.FindStringSubmatch(s); len(groups) > 0 {
		return strconv.ParseUint(groups[1], 8, 64)
	}

	err = errors.New("Value is an invalid int: " + s)
	return
}

func constructFloat(n parser.Node, userData interface{}) (data interface{}, err error) {
	var s string

	if scalar, ok := n.(*parser.Scalar); ok {
		s = scalar.Value
	} else {
		err = errors.New("Non-scalar tagged as float")
		return
	}

	switch {
	case csFloatPat.MatchString(s):
		return strconv.ParseFloat(s, 64)
	case csInfPat.MatchString(s):
		sign := 1
		if s[0] == '-' {
			sign = -1
		}
		return math.Inf(sign), nil
	case csNanPat.MatchString(s):
		return math.NaN(), nil
	}

	err = errors.New("Value is an invalid float: " + s)
	return
}
