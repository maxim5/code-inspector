// Copyright 2011 Arne Vansteenkiste (barnex@gmail.com).  All rights reserved.
// Use of this source code is governed by a freeBSD
// license that can be found in the LICENSE.txt file.

package runtime

// This file implements CUDA streams

//#include <cuda_runtime.h>
import "C"
import "unsafe"

import ()

type Stream uintptr

// The default stream
const STREAM0 Stream = 0

// Creates an asynchronous stream
func StreamCreate() Stream {
	var stream C.cudaStream_t
	err := Error(C.cudaStreamCreate(&stream))
	if err != Success {
		panic(err)
	}
	return Stream(unsafe.Pointer(stream))
}

// Destroys an asynchronous stream
func StreamDestroy(stream Stream) {
	err := Error(C.cudaStreamDestroy(C.cudaStream_t(unsafe.Pointer(stream))))
	if err != Success {
		panic(err)
	}
}

// Destroys an asynchronous stream
func (s *Stream) Destroy() {
	StreamDestroy(*s)
	*s = 0
}

// Returns Success if all operations have completed, ErrorNotReady otherwise
func StreamQuery(stream Stream) Error {
	return Error(C.cudaStreamQuery(C.cudaStream_t(unsafe.Pointer(stream))))
}

// Returns Success if all operations have completed, ErrorNotReady otherwise
func (s Stream) Query() Error {
	return StreamQuery(s)
}

// Blocks until the stream has completed.
func StreamSynchronize(stream Stream) {
	err := Error(C.cudaStreamSynchronize(C.cudaStream_t(unsafe.Pointer(stream))))
	if err != Success {
		panic(err)
	}
}

// Blocks until the stream has completed.
func (s Stream) Synchronize() {
	err := Error(C.cudaStreamSynchronize(C.cudaStream_t(unsafe.Pointer(s))))
	if err != Success {
		panic(err)
	}
}
