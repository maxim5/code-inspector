//
//	data/values.go
//	goyaml
//
//	Created by Ross Light on 2010-07-04.
//

/*
	The data package has various ways to work with generic data.  It was
	designed for YAML data, but many of the same functions are useful for
	generic mappings and such.
*/
package data

import (
	"reflect"
)

// Sequence is an ordered collection of values.
type Sequence []interface{}

// Map is an unordered collection of values associated with key values.
type Map map[interface{}]interface{}

// AsBool converts an untyped value to a boolean.
func AsBool(data interface{}) (b bool, ok bool) {
	b, ok = data.(bool)
	return
}

// AsFloat converts an untyped value to a floating-point number.
func AsFloat(data interface{}) (f float64, ok bool) {
	val := reflect.ValueOf(data)
	ok = true

	switch val.Kind() {
	case reflect.Float32, reflect.Float64:
		f = val.Float()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		f = float64(val.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		f = float64(val.Uint())
	default:
		ok = false
	}
	return
}

// AsUint converts an untyped value to an unsigned integer.
func AsUint(data interface{}) (i uint, ok bool) {
	i64, ok := AsUint64(data)
	if ok {
		i = uint(i64)
	}
	return
}

// AsUint64 converts an untyped value to a uint64.
func AsUint64(data interface{}) (i uint64, ok bool) {
	val := reflect.ValueOf(data)
	ok = true

	switch val.Kind() {
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		i = val.Uint()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if val.Int() >= 0 {
			i = uint64(val.Int())
		} else {
			ok = false
		}
	default:
		ok = false
	}
	return
}

// AsInt converts an untyped value to a signed integer.
func AsInt(data interface{}) (i int, ok bool) {
	i64, ok := AsInt64(data)
	if ok {
		i = int(i64)
	}
	return
}

// AsInt64 converts an untyped value to a int64.
func AsInt64(data interface{}) (i int64, ok bool) {
	val := reflect.ValueOf(data)
	ok = true

	switch val.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		i = val.Int()
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		i = int64(val.Uint())
	default:
		ok = false
	}
	return
}

// AsSequence converts an untyped value to a sequence of values.
func AsSequence(data interface{}) (seq Sequence, ok bool) {
	switch s := data.(type) {
	case []interface{}:
		seq, ok = Sequence(s), true
	case Sequence:
		seq, ok = s, true
	}
	return
}

// AsMap converts an untyped value to a map of values.
func AsMap(data interface{}) (m Map, ok bool) {
	switch d := data.(type) {
	case map[interface{}]interface{}:
		m, ok = Map(d), true
	case Map:
		m, ok = d, true
	}
	return
}

// HasKeys returns whether a given map contains all of the keys given.
func (m Map) HasKeys(keys ...interface{}) bool {
	for _, k := range keys {
		if _, found := m[k]; !found {
			return false
		}
	}
	return true
}

// CopyMap creates a shallow copy of a map.
func (m Map) Copy() (clone Map) {
	clone = make(Map, len(m))
	for k, v := range m {
		clone[k] = v
	}
	return
}

// SetDefault adds a new key to a map if the key isn't already present, and
// returns the latest value for the key.
func (m Map) SetDefault(k, d interface{}) (v interface{}) {
	v, ok := m[k]
	if !ok {
		m[k] = d
		v = d
	}
	return
}
