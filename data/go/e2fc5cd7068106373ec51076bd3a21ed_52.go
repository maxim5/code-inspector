/* interface类型是方法的组合.
A value of interface type can hold any value that implements those methods.
*/
package main

import (
	"fmt"
	"math"
)

type Abser interface {
	Abs() float64
}

func main() {
	var a Abser
	f := MyFloat(-math.Sqrt2)
	p := Vertex{3, 4}

	a = f  // a MyFloat implements Abser
	a = &p // a *Vertex implements Abser
//	a = p  // a Vertex, does NOT
	       // implement Abser

	fmt.Println(a.Abs())
}

type MyFloat float64

func (f MyFloat) Abs() float64 {
	if f < 0 {
		return float64(-f)
	}
	return float64(f)
}

type Vertex struct {
	X, Y float64
}

func (p *Vertex) Abs() float64 {
	return math.Sqrt(p.X*p.X + p.Y*p.Y)
}

