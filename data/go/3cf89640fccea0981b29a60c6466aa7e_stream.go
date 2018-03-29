//  This file is part of MuMax, a high-performance micromagnetic simulator.
//  Copyright 2011  Arne Vansteenkiste and Ben Van de Wiele.
//  Use of this source code is governed by the GNU General Public License version 3
//  (as published by the Free Software Foundation) that can be found in the license.txt file.
//  Note that you are welcome to modify this code under the condition that you do not remove any 
//  copyright notices and prominently state that you modified it, giving a relevant date.

package gpu

// This file implements CUDA streams for multi-GPU use.
// Author: Arne Vansteenkiste

import (
	//. "mumax/common"
	cu "cuda/driver"
	cuda "cuda/runtime"
)

type Stream []cu.Stream

// Creates a new multi-GPU stream. Its use is similar as cu.Stream,
// but operates on all GPUs at the same time.
func NewStream() Stream {
	NDev := NDevice()
	str := make([]cu.Stream, NDev)
	for i := range str {
		cuda.SetDevice(_useDevice[i])
		str[i] = cu.StreamCreate()
	}
	return Stream(str)
}

// Destroys the multi-GPU stream.
func (s Stream) Destroy() {
	for i := range s {
		cuda.SetDevice(_useDevice[i])
		cu.StreamDestroy(&s[i])
	}
}

// Synchronizes with all underlying GPU-streams
func (s Stream) Sync() {
	for i := range s {
		cuda.SetDevice(_useDevice[i])
		s[i].Synchronize()
	}
}

// Returns true if all underlying GPU streams have completed.
func (s Stream) Ready() (ready bool) {
	for i := range s {
		cuda.SetDevice(_useDevice[i])
		if s[i].Query() != cu.SUCCESS {
			return false
		}
	}
	return true
}
