// Huisman-Olff-Fresco coenocline modeller. 
// Huisman, J., Olff, H. & Fresco, L.F.M. (1993) A hierarchical set of models for species response analysis. Journal of Vegetation Science, 4, 37-46. 
package main

import (
	. "code.google.com/p/go-eco/eco/aux"
	"flag"
	"fmt"
	"math"
	"math/rand"
)

// HOF response function 
func hofSRF(which int, a, b, c, d, m, x float64) (y float64) {
	switch which {
	case 1: // model I
		y = m / (1 + math.Exp(a))
	case 2: // model II
		y = m / (1 + math.Exp(a+b*x))
	case 3: // model III
		y = m / ((1 + math.Exp(a+b*x)) * (1 + math.Exp(c)))
	case 4: // model IV
		y = m / ((1 + math.Exp(a+b*x)) * (1 + math.Exp(c-b*x)))
	case 5: // model V
		y = m / ((1 + math.Exp(a+b*x)) * (1 + math.Exp(c-d*x)))
	}
	return
}

// Coenocline modeller
func Coenocline(nSpec, nSamp, srfModel int, abumax float64) (out *Matrix) {
	out = NewMatrix(nSamp, nSpec)

	// regular spacing of samples on [0, 1] gradient
	points := make([]float64, nSamp)
	for i := 0; i < nSamp; i++ {
		points[i] = float64(i) / float64(nSamp-1)
	}

	for j := 0; j < nSpec; j++ {

		z := 10.0
		a := z * rand.Float64()
		b := z * rand.Float64()
		c := z * rand.Float64()
		d := z * rand.Float64()
		m := abumax * rand.Float64()
		//		m := abumax

		for i := 0; i < nSamp; i++ { // generate SRF values at every sampling point
			x := (points[i] - 0.5) * 12 // scaling [0, 1] to [-6, +6], a typical interval of HOF response
			y := hofSRF(srfModel, a, b, c, d, m, x)
			if y < 0 {
				y = 0
			}
			out.Set(i, j, y)
		}

		max := -1e20 // find maximum
		for i := 0; i < nSamp; i++ {
			if out.Get(i, j) > max {
				max = out.Get(i, j)
			}
		}
		for i := 0; i < nSamp; i++ { //  and recalculate so that maximum has the desired value
			y := m * out.Get(i, j) / max
			out.Set(i, j, y)
		}

		// shift the curve so that max is at desired position
	}
	return
}

func main() {
	help := flag.Bool("h", false, "Coenocline modeller\nUsage: coenocline -m [1|2|3|4|5] [-xyma]")
	nSpec := flag.Int("x", 20, "number of species")
	nSamp := flag.Int("y", 30, "number of samples")
	srfModel := flag.Int("m", 5, "HOF model: 1 - 5")
	abumax := flag.Float64("a", 100.0, "maximum abundance")

	flag.Parse()

	if *help {
		flag.PrintDefaults()
	} else {
		mtx := Coenocline(*nSpec, *nSamp, *srfModel, *abumax)
		for i := 0; i < *nSamp; i++ {
			for j := 0; j < *nSpec; j++ {
				fmt.Print(mtx.Get(i, j), ",")
			}
			fmt.Println()
		}
	}
}
