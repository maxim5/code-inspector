package data

import (
	// "fmt"
	"github.com/mrmorphic/goss"
	"github.com/mrmorphic/goss/convert"
	"reflect"
	"strconv"
)

func Eval(context interface{}, name string, args ...interface{}) interface{} {
	return NewDefaultLocater(context).Get(name, args...)
}

// Given a context, set the property 'name' to the specified value. Property should be
// a struct, *struct or map, otherwise it will panic.
func Set(context interface{}, name string, value interface{}) {
	// fmt.Printf("\nSet %s.%s in %s\n", context, name, value)
	ctx := reflect.ValueOf(context)
	if ctx.Kind() == reflect.Ptr {
		ctx = ctx.Elem()
	}

	v := reflect.ValueOf(value)

	switch ctx.Kind() {
	case reflect.Struct:
		// get the field and its type
		f := ctx.FieldByName(name)
		// we only try to set the field if the struct defines it.
		if !IsZeroOfUnderlyingType(f) {
			t := f.Type()

			// ensure that the type is assignable to the field
			vi := convertToType(v.Interface(), t)

			// now set it
			f.Set(reflect.ValueOf(vi))
		}
	case reflect.Map:
		ctx.SetMapIndex(reflect.ValueOf(name), v)
	default:
		panic("data.Set only supports contexts that are structs or maps")
	}
}

// defaultLocator is an implementation of DataLocator, with specific behaviours that make it useful in
// the context of SilverStripe. Specifically:
//  * given a name and optional args, will locate a function or property that will meet that.
//  * if context is a map with string key, it will return properties
//  * if a matching symbol can't be found in the context, it will use a property or function called
//    _fallback if it exists, and try to interpret that instead. This allows for the SS-type behaviour
//    of passing a controller
//  * if the context value implements DataLocator, it will delegate to that.
type DefaultLocater struct {
	context interface{}
}

func NewDefaultLocater(context interface{}) goss.Evaluater {
	return &DefaultLocater{context}
}

func (d *DefaultLocater) Get(name string, args ...interface{}) interface{} {
	// fmt.Printf("\nLocate %s (%s) in %s\n", name, args, d.context)

	// Get the Value of context, and dereference if the type is a pointer
	ctx := reflect.ValueOf(d.context)
	ctxOrig := ctx
	if ctx.Kind() == reflect.Ptr {
		ctx = ctx.Elem()
	}

	var value reflect.Value

	// Get the Value associated with the name, which depends on what kind of item the
	// context is.
	switch {
	case ctx.Kind() == reflect.Map:
		value = ctx.MapIndex(reflect.ValueOf(name))
	case ctx.Kind() == reflect.Struct:
		// test first for a function of that name. The catch is that if the original item was a pointer to
		// a struct, the method exists on *struct, not the struct itself, so we need to look at the original
		// Value, not the Elem.
		value = ctxOrig.MethodByName(name)
		if IsZeroOfUnderlyingType(value) {
			// then try Get<name>, which lets a model override a property with a getter.
			value = ctxOrig.MethodByName("Get" + name)
			if IsZeroOfUnderlyingType(value) {
				// if no function, test for struct field of that name. @todo lowercase hidden?
				value = ctx.FieldByName(name)
			}
		}
	}

	// Now we have the value, work out what to do with it. There are two special cases; value couldn't
	// be determined so try _fallback; the value's kind is a function, so call it with args
	switch {
	case IsZeroOfUnderlyingType(value):
		// see if there is a _fallback
		if name != "Fallback" {
			fallback := d.Get("Fallback")
			// fmt.Printf("fallback is %s\n", fallback)
			if fallback != nil {
				return Eval(fallback, name, args...)
			}
		}
		// we couldn't work it out, just return nil with no error.
		return nil
	case value.Kind() == reflect.Func:
		result := value.Call(getConvertedParams(value, args))

		// we ignore any other values returned.
		return result[0].Interface()
	}

	return value.Interface()
}

// Given a method Value and a collection of arguments to the method, return a slice
// of Value objects for those args. Also handle certain cases of automatic type
// conversion that are normal in a SilverStripe site (because of PHP type system.)
func getConvertedParams(method reflect.Value, args []interface{}) []reflect.Value {
	methodType := method.Type()

	paramIndex := 0
	nParams := methodType.NumIn()

	a := make([]reflect.Value, 0)
	for _, x := range args {
		// get the param definition, if we haven't exhausted them already
		var param reflect.Type
		hasParam := false
		if paramIndex < nParams {
			param = methodType.In(paramIndex)
			hasParam = true
		}

		// determine if conversion is required
		if hasParam {
			x = convertToType(x, param)
		}

		// finally add the value to the list of parameters
		a = append(a, reflect.ValueOf(x))
		paramIndex++
	}

	return a
}

func convertToType(x interface{}, t reflect.Type) interface{} {
	// fmt.Printf("\nconvertTo: %s to %s\n", x, t.Name())
	argType := reflect.TypeOf(x)
	if argType.AssignableTo(t) {
		// if assignable, it's OK
		return x
	}

	if argType.ConvertibleTo(t) {
		// if convertible by runtime, convert it
		v := reflect.ValueOf(x)
		v.Convert(t)
		return v.Interface()
	}

	// if x is a string and t is an int, convert
	if argType.Kind() == reflect.String && t.Kind() == reflect.Int {
		i, e := strconv.Atoi(x.(string))
		if e != nil {
			i = 0
		}
		return i
	}
	// General case, just return what we got
	return x
}

// Return string representation of the field
func (d *DefaultLocater) GetStr(fieldName string, args ...interface{}) string {
	return convert.AsString(d.Get(fieldName))
}

func (d *DefaultLocater) GetInt(fieldName string, args ...interface{}) (int, error) {
	return convert.AsInt(d.Get(fieldName))
}

func IsZeroOfUnderlyingType(x interface{}) bool {
	return x != nil && x == reflect.Zero(reflect.TypeOf(x)).Interface()
}
