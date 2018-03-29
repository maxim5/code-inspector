// Copyright 2012 - 2013 The Eco Authors. All rights reserved. See the LICENSE file.

package sim

// Simple matching coefficient similarity matrix. 
// Sokal RR, Michener CD 1958 A statistical method for evaluating systematic relationship. University of Kansas Science Bulletin, 38:1409-1438. 
// Legendre & Legendre 1998: 255, eq. 7.1 (S1 index). 

import (
	"code.google.com/p/go-eco/eco/aux"
)

// SimpleMatchingBool_S returns a Simple matching coefficient (also called Sokal - Michener) similarity matrix for boolean data. 
func SimpleMatchingBool_S(data *aux.Matrix) *aux.Matrix {
	var (
		a, b, c, d float64 // these are actually counts, but float64 simplifies the formulas
	)

	rows := data.R
	out := aux.NewMatrix(rows, rows)
	for i := 0; i < rows; i++ {
		for j := i; j < rows; j++ {
			a, b, c, d = aux.GetABCD(data, i, j)
			v := (a + d) / (a + b + c + d)
			out.Set(i, j, v)
			out.Set(j, i, v)
		}
	}
	return out
}
