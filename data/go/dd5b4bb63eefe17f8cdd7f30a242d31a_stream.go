// Copyright 2011 Arne Vansteenkiste (barnex@gmail.com).  All rights reserved.
// Use of this source code is governed by a freeBSD
// license that can be found in the LICENSE.txt file.

package driver

// This file implements CUDA streams

//#include <cuda.h>
import "C"
import "unsafe"

import ()

type Stream uintptr

// The default stream
const STREAM0 Stream = 0

// Creates an asynchronous stream
func StreamCreate() Stream {
	var stream C.CUstream
	err := Result(C.cuStreamCreate(&stream, C.uint(0))) // flags has to be zero
	if err != SUCCESS {
		panic(err)
	}
	return Stream(unsafe.Pointer(stream))
}

// Destroys the asynchronous stream
func (stream *Stream) Destroy() {
	str := *stream
	*stream = Stream(uintptr(0))
	err := Result(C.cuStreamDestroy(C.CUstream(unsafe.Pointer(str))))
	if err != SUCCESS {
		panic(err)
	}
}

// Destroys an asynchronous stream
func StreamDestroy(stream *Stream) {
	stream.Destroy()
}

// Blocks until the stream has completed.
func (stream Stream) Synchronize() {
	err := Result(C.cuStreamSynchronize(C.CUstream(unsafe.Pointer(stream))))
	if err != SUCCESS {
		panic(err)
	}
}

// Returns Success if all operations have completed, ErrorNotReady otherwise
func (stream Stream) Query() Result {
	return Result(C.cuStreamQuery(C.CUstream(unsafe.Pointer(stream))))
}

// Returns Success if all operations have completed, ErrorNotReady otherwise
func StreamQuery(stream Stream) Result {
	return stream.Query()
}

// Blocks until the stream has completed.
func StreamSynchronize(stream Stream) {
	stream.Synchronize()
}
