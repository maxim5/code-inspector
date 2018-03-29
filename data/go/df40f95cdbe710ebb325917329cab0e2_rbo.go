/* 
    Implementation of the basic RBO functions.

    Copyright (C) 2014  Marcin Kik

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.


   The author can be reached at mki1967@gmail.com

*/

package rbo

// the number represented by the reversal of the k least significant bits of binary representation of x
func RevBits(k uint8, x uint64) (y uint64) {
	y = (x & 1)
	for i := uint8(1); i < k; i++ {
		y = y << 1
		x = x >> 1
		y = (y | (x & 1))
	}
	return

}

// Returns (t+min{d>0 : r1<= RevBits( (t+d)mod 2^k   ) <=r2}) mod 2^k,
// which is the next wake-up time slot modulo 2^k after t for the RBO receiver if the values of lb and ub are r1 and r2, respectively.
// We assume 0<=r1<=r2< 2^k.
// This is efficient implementation of this function
func NSI(k uint8, t uint64, r1 uint64, r2 uint64) uint64 {
	var twoToK uint64 = (1 << k)     // 2^k
	var modMaskK uint64 = twoToK - 1 // 2^k-1
	var t1, x1, x2, stepDivMask uint64
	var twoToL uint64 = 1
	var stepLMinusOne uint64 = modMaskK
	var tNext uint64 = ((t + 1) & modMaskK)

	for {
		t1 = tNext
		for twoToL < twoToK && (t1&twoToL) == 0 {
			twoToL = twoToL << 1
			stepLMinusOne = stepLMinusOne >> 1
		}
		tNext = ((t1 + twoToL) & modMaskK)
		stepDivMask = ((^stepLMinusOne) & modMaskK)
		x1 = RevBits(k, t1)
		x2 = (x1 | stepDivMask)
		if r1 <= x2 && x1 <= r2 && ((r1-x1+stepLMinusOne)&stepDivMask) <= ((r2-x1)&stepDivMask) {
			break
		}
	}
	var s uint64 = (twoToK >> 1) // 2^(k-1)
	for x1 < r1 || x1 > r2 {

		if x1 < r1 {
			x1 = x1 + s
		} else {
			x1 = x1 - s
		}
		s = s >> 1 // s = s / 2
	}
	return RevBits(k, x1)
}

