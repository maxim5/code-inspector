// Package humanize formats large numbers into human readable small numbers.
//
// For example:
//   Using the default base 2:
//      9223372036854775807 as one of 9007199254740992k, 8796093022208M,
//                                    8589934592G, 8388608T, 8192P,
//                                    8.0E, or 8E
//
//   With SI prefixes:
//      9223372036854775807 as one of 9007199254740992Ki, 8796093022208Mi,
//                                    8589934592Gi, 8388608Ti, 8192Pi,
//                                    8.0Ei, or 8Ei
//
//   Using base 10 prefixes:
//      9223372036854775807 as one of 9223372036854776K, 9223372036855M,
//                                    9223372037G, 9223372T, 9223P,
//                                    9.2E, or 9E
package humanize

// Base 2 prefix values (e.g. Mega = 1024*1024)
const (
	_        Int64 = 1 << (10 * iota)
	Kilo           // k or Ki (Note, K is used with Divisor1000)
	Mega           // M or Mi
	Giga           // G or Gi
	Tera           // T or Ti
	Peta           // P or Pi
	Exa            // E or Ei
	MaxScale = iota
	// These overflow int64
	//Zetta
	//Yotta
)

// Flags specify the formating behaviour.
type Flags int

// Flag values.
const (
	Decimal     Flags = 1 << iota // If the final result is less than 10, display it using one decimal point.
	NoSpace                       // Do not put a space between number and the prefix.
	Bytes                         // Use ‘B’ (bytes) as prefix if the original result does not have a prefix.
	Divisor1000                   // Divide by 1000 instead of 1024.
	SIPrefixes                    // Use Ki, Mi, Gi, etc when dividing by 1024

	AlwaysSign  // Always include the sign ('+' or '-')
	SpaceSign   // Leave a space for elided sign
	ZeroPad     // Pad with leading zeros rather than spaces
	LeftJustify // Pad with spaces on the right rather than the left
)

var flagNames = [...]string{"Decimal", "NoSpace", "Bytes", "Divisor1000", "SIPrefixes",
	"AlwaysSign", "SpaceSign", "ZeroPad", "LeftJustify"}

// Defaults used by Itoa, Int64.String, and Int64.Format.
var (
	DefaultMinLen = 0
	DefaultMaxLen = 4
	DefaultFlags  = NoSpace | Decimal
)

// Itoa is shorthand for Format(i, DefaultMinLen, DefaultMaxLen, DefaultFlags)
func Itoa(i int64) string {
	return Format(i, DefaultMinLen, DefaultMaxLen, DefaultFlags)
}

// Format formats a value into a human readable form.
//
// The value is divided and rounded until it will fit into into a string of
// the specified max length.
// If the number has been divided, a space and an appropriate unit prefix
// is also added (which also must fit into the specified max length).
// The NoSpace flag omits the space before the prefix (if any).
// If the resulting string is shorter than the min length specified, it will
// be padded.
//
// By default, the traditional computer science conventions of dividing
// by 1024 and using the prefixes k, M, G, etc is used
// rather than the SI (and IEE/IEC) power of two convention (Ki, Mi, Gi, etc)
// or the power of ten notion (K, M, G, etc).
// The SIPrefixes and Divisor1000 flags change this behaviour.
//
// The Decimal flag causes values <= 9.9 (after dividing and rounding) to be formated
// with a decimal and a single fractional digit (e.g. "1.2 G" instead of "1 G" when
// "1200 M" is too wide).
//
// The maxlen argument must be at least 3 for positive values, and at least 4 for
// negative values. When SIPrefixes is specified the minimums are 4 and 5.
// Format will panic when maxlen is not adequate.
//
// The traditional (default) prefixes:
//   Scale  Prefix  Description  Multiplier           Multiplier (Divisor1000)
//   0       /B¹                 1                    1
//   1      k/K²    kilo         1024                 1000
//   2      M       mega         1048576              1000000
//   3      G       giga         1073741824           1000000000
//   4      T       tera         1099511627776        1000000000000
//   5      P       peta         1125899906842624     1000000000000000
//   6      E       exa          1152921504606846976  1000000000000000000
//
//   ¹ B is used with the Bytes flag
//   ² K is used with the Divisor1000 flag
//
// The SI (and IEE/IEC) power of two prefixes:
//   Scale  Prefix  Description  Multiplier
//   0       /B³                 1
//   1      Ki      kibi         1024
//   2      Mi      mebi         1048576
//   3      Gi      gibi         1073741824
//   4      Ti      tebi         1099511627776
//   5      Pi      pebi         1125899906842624
//   6      Ei      exbi         1152921504606846976
//
//   ³ B is used with the Bytes flag
func Format(value int64, minlen, maxlen int, flags Flags) string {
	str, _ := format(value, minlen, maxlen, autoScale, flags)
	return str
}

// Scale returns the scale that would be used to format the value
// using Format(value, 0, maxlen, flags).
//
// One use of Scale is to store the maximum returned scale when called with
// a sequence of values and then call FormatToScale with that maxiumum for
// each value to have a column of values formatted to the same scale.
func Scale(value int64, maxlen int, flags Flags) uint {
	_, sc := format(value, 0, maxlen, getScale, flags)
	return sc
}

// FormatToScale formats a value into a human readable form using a specific
// scale and prefix.
//
// The prefixes and flags are as specified with the Format function.
func FormatToScale(value int64, minlen int, scale uint, flags Flags) string {
	if scale > uint(MaxScale) {
		panic("humanize.FormatToScale: bad scale")
	}
	str, _ := format(value, minlen, 1e5, int(scale), flags)
	return str
}
