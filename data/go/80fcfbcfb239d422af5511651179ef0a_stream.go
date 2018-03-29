package xcog

// Stream is a conduit/channel interface between structures that process sense.
// A stream may be buffered, and may filter or amend the sense it conveys,
// but should always comply with FIFO (i.e. always convey sense in the order
// in which it is received).
type Stream interface {
	Inchan() chan<- Sense
	Intake(overflow Stream, s ...Sense)
}

// SimpleStream functions as a channel between the mantles and the surface
// of a mind, or between surface pools.
type SimpleStream struct {
	contents chan Sense
	outflow  chan<- Sense
}

func NewStream(capacity uint, endpoint Pool) *SimpleStream {
	if endpoint == nil {
		panic("NewStream cannot accept nil endpoint.")
	}
	stream := &SimpleStream{
		make(chan Sense, capacity),
		endpoint.Inchan(),
	}
	go stream.autoFlowRoutine()
	return stream
}

// Inchan returns the receive-only intake channel of the stream.
func (stream *SimpleStream) Inchan() chan<- Sense {
	return stream.contents
}

// Intake receives the given sense into the head of the stream.
// If overflow is nil, then the sense will be discarded if the stream is full;
// otherwise the method will attempt to send the sense to overflow instead,
// or discard the sense if overflow does not immediately accept it.
func (stream *SimpleStream) Intake(overflow Stream, s ...Sense) {
	if s == nil || len(s) == 0 {
		return
	}
	for _, c := range s {
		// Attempt to accept into the head of the stream
		select {
		case stream.contents <- c:
			// Accepted
		default:
			// Stream is full
			// Attempt to send to overflow if defined
			if overflow != nil {
				overflow.Intake(nil, c)
			}
			// TODO Consider: Instead of default immediately,
			// wait for the next tick (allow processing time).
		}
	}
}

// Content accepted into the stream is guaranteed to be sent to outflow,
// unless a send on the channel blocks forever (or until program termination!).
func (stream *SimpleStream) autoFlowRoutine() {
	for c := range stream.contents {
		stream.outflow <- c
	}
}
