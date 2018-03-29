// DO NOT CHANGE THIS FILE

// Definition of API for LSP 2012 This file defines all of the public
// accessible values and functions The actual implementation functions
// are included in different files, as is documented below In
// addition, the API requires definitions of several struct's, but
// their actual definitions are included in the implementation files

package lsp12

// Define operational parameters for LSP client or server
// Parameter structure used when initializing either a client or a server
type LspParams struct {
	// How many epochs can transpire before declaring connection lost
	// When 0, use default value (5)
	EpochLimit int
	// How many milliseconds between epochs
	// When 0, use default value (2000)
	EpochMilliseconds int
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Part A: Packets.
// Implementation file: lsp12-packet.go

// Defines the byte-level format for LSP packets

// Classify different message types
const (
	MsgCONNECT = iota   // Connection request from client
	MsgDATA             // Data 
	MsgACK              // Acknowledge connection request, data, or close
	MsgINVALID          // Invalid message
)

// Program representation of message contained within packet
// Stored in struct in way that can encode as packet using JSON.
type LspMessage struct {
	Type byte      // One of the above-listed values
	ConnId uint16  // Connection ID
	SeqNum byte    // Sequence number (wraps around)
	Payload []byte // Messsage payload (nil for Connect or Ack messages)
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Part B: LSP Client functionality
// Implementation file: lsp12-client.go
//
// Implements a high-level client abstraction that provides a
// convenient programming model for writing client applications

type LspClient struct {
	iLspClient  // Private fields
}

// Initiate a connection to host and set up application client
// Call returns only after connection established
func NewLspClient(hostport string, params *LspParams) (*LspClient, error) {
	return iNewLspClient(hostport, params)
}

// Return the Connection ID for a client
func (cli *LspClient) ConnId() uint16 {
	return cli.iConnId()
}

// Read message from server.  Non-nil error indicates that connection
// to server is permanently lost
// Call blocks until value available to read, or network disconnected
func (cli *LspClient) Read() ([]byte, error) {
	return cli.iRead()
}

// Write message to server.  Non-nil error indicates that connection
// to server is permanently lost.
// Call does not block
func (cli *LspClient) Write(payload []byte) error {
	return cli.iWrite(payload)
}

// Signal to server that no further interactions from client are required.
// Call blocks until all pending messages to server have been sent,
// or network connection lost
// Application should not attempt call to Read, Write, or Close after calling Close
func (cli *LspClient) Close() {
	cli.iClose()
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Part C: LSP server functionality
// Implementation file: lsp12-server.go
//
// Implements a high-level server abstraction that provides a convenient programming
// model for writing server applications

type LspServer struct {
	iLspServer // Private fields
}

// Set up an application server on specified port.
// Call returns once server ready to accept connection requests
func NewLspServer(port int, params *LspParams) (*LspServer, error) {
	return iNewLspServer(port, params)
}

// Read next message received by server, return connection ID + contents.
//
// When connection ID > 0 & error non-nil, this indicates that the
// particular connection has terminated.
//
// When connection ID == 0 & error non-nil, then server is no longer
// operational
//
// Call blocks until value available to read, or network disconnected
func (srv *LspServer) Read() (uint16, []byte, error) {
	return srv.iRead()
}

// Write message to specified client.  Non-nil error indicates that
// client is no longer available.
// connId should be > 0.
//
// Any attempt to send message with connID == 0
// will be ignored, with non-nil error value returned.
// Call does not block
func (srv *LspServer) Write(connId uint16, payload []byte) error {
	return srv.iWrite(connId, payload)
}

// Close only specified connection.
// connID should be > 0.
// Call does not block.
// The application should not make any subsequent calls to 
// Write or CloseConn for this connection.
// Read should not return any data from a connection that has been closed.
func (srv *LspServer) CloseConn(connId uint16) {
	srv.iCloseConn(connId)
}

// Close all connections and terminate server
// Call returns after all pending messages to active clients have been sent
// Application should not attempt to call Read, Write, CloseConn, or CloseAll
//  after calling CloseAll
func (srv *LspServer) CloseAll() {
	srv.iCloseAll()
}