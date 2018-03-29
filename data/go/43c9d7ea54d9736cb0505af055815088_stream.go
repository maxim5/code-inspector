// Copyright 2011 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file contains the three layers of processing for the
// communication with the server: transport (where TLS happens), XML
// (where strings are converted to go structures), and Stream (where
// we respond to XMPP events on behalf of the library client), or send
// those events to the client.

package xmpp

import (
	"crypto/md5"
	"crypto/rand"
	"crypto/tls"
	"encoding/base64"
	"encoding/xml"
	"fmt"
	"io"
	"log/syslog"
	"math/big"
	"net"
	"regexp"
	"strings"
	"time"
)

// Callback to handle a stanza with a particular id.
type stanzaHandler struct {
	id string
	// Return true means pass this to the application
	f func(Stanza) bool
}

// BUG(cjyar) Review all these *Client receiver methods. They should
// probably either all be receivers, or none.

func (cl *Client) readTransport(w io.WriteCloser) {
	defer w.Close()
	cl.socket.SetReadTimeout(1e8)
	p := make([]byte, 1024)
	for {
		if cl.socket == nil {
			cl.waitForSocket()
		}
		nr, err := cl.socket.Read(p)
		if nr == 0 {
			if errno, ok := err.(*net.OpError); ok {
				if errno.Timeout() {
					continue
				}
			}
			if Log != nil {
				Log.Println("read: " + err.Error())
			}
			break
		}
		nw, err := w.Write(p[:nr])
		if nw < nr {
			if Log != nil {
				Log.Println("read: " + err.Error())
			}
			break
		}
	}
}

func (cl *Client) writeTransport(r io.Reader) {
	defer cl.socket.Close()
	p := make([]byte, 1024)
	for {
		nr, err := r.Read(p)
		if nr == 0 {
			if Log != nil {
				Log.Println("write: " + err.Error())
			}
			break
		}
		nw, err := cl.socket.Write(p[:nr])
		if nw < nr {
			if Log != nil {
				Log.Println("write: " + err.Error())
			}
			break
		}
	}
}

func readXml(r io.Reader, ch chan<- interface{},
	extStanza map[string]func(*xml.Name) interface{}) {
	if Loglevel >= syslog.LOG_DEBUG {
		pr, pw := io.Pipe()
		go tee(r, pw, "S: ")
		r = pr
	}
	defer close(ch)

	p := xml.NewParser(r)
Loop:
	for {
		// Sniff the next token on the stream.
		t, err := p.Token()
		if t == nil {
			if err != io.EOF {
				if Log != nil {
					Log.Println("read: " + err.Error())
				}
			}
			break
		}
		var se xml.StartElement
		var ok bool
		if se, ok = t.(xml.StartElement); !ok {
			continue
		}

		// Allocate the appropriate structure for this token.
		var obj interface{}
		switch se.Name.Space + " " + se.Name.Local {
		case NsStream + " stream":
			st, err := parseStream(se)
			if err != nil {
				if Log != nil {
					Log.Println("unmarshal stream: " +
						err.Error())
				}
				break Loop
			}
			ch <- st
			continue
		case "stream error", NsStream + " error":
			obj = &streamError{}
		case NsStream + " features":
			obj = &Features{}
		case NsTLS + " proceed", NsTLS + " failure":
			obj = &starttls{}
		case NsSASL + " challenge", NsSASL + " failure",
			NsSASL + " success":
			obj = &auth{}
		case "jabber:client iq":
			obj = &Iq{}
		case "jabber:client message":
			obj = &Message{}
		case "jabber:client presence":
			obj = &Presence{}
		default:
			obj = &Generic{}
			if Log != nil && Loglevel >= syslog.LOG_NOTICE {
				Log.Printf("Ignoring unrecognized: %s %s",
					se.Name.Space, se.Name.Local)
			}
		}

		// Read the complete XML stanza.
		err = p.Unmarshal(obj, &se)
		if err != nil {
			if Log != nil {
				Log.Println("unmarshal: " + err.Error())
			}
			break Loop
		}

		// If it's a Stanza, we try to unmarshal its innerxml
		// into objects of the appropriate respective
		// types. This is specified by our extensions.
		if st, ok := obj.(Stanza); ok {
			err = parseExtended(st, extStanza)
			if err != nil {
				if Log != nil {
					Log.Println("ext unmarshal: " +
						err.Error())
				}
				break Loop
			}
		}

		// Put it on the channel.
		ch <- obj
	}
}

func parseExtended(st Stanza, extStanza map[string]func(*xml.Name) interface{}) error {
	// Now parse the stanza's innerxml to find the string that we
	// can unmarshal this nested element from.
	reader := strings.NewReader(st.innerxml())
	p := xml.NewParser(reader)
	for {
		t, err := p.Token()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		if se, ok := t.(xml.StartElement); ok {
			if con, ok := extStanza[se.Name.Space]; ok {
				// Call the indicated constructor.
				nested := con(&se.Name)

				// Unmarshal the nested element and
				// stuff it back into the stanza.
				err := p.Unmarshal(nested, &se)
				if err != nil {
					return err
				}
				st.addNested(nested)
			}
		}
	}

	return nil
}

func writeXml(w io.Writer, ch <-chan interface{}) {
	if Loglevel >= syslog.LOG_DEBUG {
		pr, pw := io.Pipe()
		go tee(pr, w, "C: ")
		w = pw
	}
	defer func(w io.Writer) {
		if c, ok := w.(io.Closer); ok {
			c.Close()
		}
	}(w)

	for obj := range ch {
		err := xml.Marshal(w, obj)
		if err != nil {
			if Log != nil {
				Log.Println("write: " + err.Error())
			}
			break
		}
	}
}

func (cl *Client) readStream(srvIn <-chan interface{}, cliOut chan<- Stanza) {
	defer close(cliOut)

	handlers := make(map[string]func(Stanza) bool)
Loop:
	for {
		select {
		case h := <-cl.handlers:
			handlers[h.id] = h.f
		case x, ok := <-srvIn:
			if !ok {
				break Loop
			}
			send := false
			switch obj := x.(type) {
			case *stream:
				handleStream(obj)
			case *streamError:
				cl.handleStreamError(obj)
			case *Features:
				cl.handleFeatures(obj)
			case *starttls:
				cl.handleTls(obj)
			case *auth:
				cl.handleSasl(obj)
			default:
				send = true
			}
			if !send {
				continue
			}
			st, ok := x.(Stanza)
			if !ok {
				if Log != nil && Loglevel >= syslog.LOG_WARNING {
					Log.Printf(
						"Unhandled non-stanza: %v", x)
				}
				continue
			}
			if handlers[st.GetId()] != nil {
				f := handlers[st.GetId()]
				handlers[st.GetId()] = nil
				send = f(st)
			}
			if send {
				cliOut <- st
			}
		}
	}
}

// This loop is paused until resource binding is complete. Otherwise
// the app might inject something inappropriate into our negotiations
// with the server. The control channel controls this loop's
// activity.
func writeStream(srvOut chan<- interface{}, cliIn <-chan Stanza,
	control <-chan int) {
	defer close(srvOut)

	var input <-chan Stanza
Loop:
	for {
		select {
		case status := <-control:
			switch status {
			case 0:
				input = nil
			case 1:
				input = cliIn
			case -1:
				break Loop
			}
		case x, ok := <-input:
			if !ok {
				break Loop
			}
			if x == nil {
				if Log != nil && Loglevel >= syslog.LOG_NOTICE {
					Log.Println("Refusing to send" +
						" nil stanza")
				}
				continue
			}
			srvOut <- x
		}
	}
}

// Stanzas from the remote go up through a stack of filters to the
// app. This function manages the filters.
func filterTop(filterOut <-chan <-chan Stanza, filterIn chan<- <-chan Stanza,
	topFilter <-chan Stanza, app chan<- Stanza) {
	defer close(app)
Loop:
	for {
		select {
		case newFilterOut := <-filterOut:
			if newFilterOut == nil {
				if Log != nil && Loglevel >= syslog.LOG_WARNING {
					Log.Println("Received nil filter")
				}
				filterIn <- nil
				continue
			}
			filterIn <- topFilter
			topFilter = newFilterOut

		case data, ok := <-topFilter:
			if !ok {
				break Loop
			}
			app <- data
		}
	}
}

func filterBottom(from <-chan Stanza, to chan<- Stanza) {
	defer close(to)
	for data := range from {
		to <- data
	}
}

func handleStream(ss *stream) {
}

func (cl *Client) handleStreamError(se *streamError) {
	if Log != nil && Loglevel >= syslog.LOG_NOTICE {
		Log.Printf("Received stream error: %v", se)
	}
	close(cl.Out)
}

func (cl *Client) handleFeatures(fe *Features) {
	cl.Features = fe
	if fe.Starttls != nil {
		start := &starttls{XMLName: xml.Name{Space: NsTLS,
			Local: "starttls"}}
		cl.xmlOut <- start
		return
	}

	if len(fe.Mechanisms.Mechanism) > 0 {
		cl.chooseSasl(fe)
		return
	}

	if fe.Bind != nil {
		cl.bind(fe.Bind)
		return
	}
}

// readTransport() is running concurrently. We need to stop it,
// negotiate TLS, then start it again. It calls waitForSocket() in
// its inner loop; see below.
func (cl *Client) handleTls(t *starttls) {
	tcp := cl.socket

	// Set the socket to nil, and wait for the reader routine to
	// signal that it's paused.
	cl.socket = nil
	cl.socketSync.Add(1)
	cl.socketSync.Wait()

	// Negotiate TLS with the server.
	tls := tls.Client(tcp, TLSConfig)

	// Make the TLS connection available to the reader, and wait
	// for it to signal that it's working again.
	cl.socketSync.Add(1)
	cl.socket = tls
	cl.socketSync.Wait()

	// Reset the read timeout on the (underlying) socket so the
	// reader doesn't get woken up unnecessarily.
	tcp.SetReadTimeout(0)

	if Log != nil && Loglevel >= syslog.LOG_INFO {
		Log.Println("TLS negotiation succeeded.")
	}
	cl.Features = nil

	// Now re-send the initial handshake message to start the new
	// session.
	hsOut := &stream{To: cl.Jid.Domain, Version: Version}
	cl.xmlOut <- hsOut
}

// Synchronize with handleTls(). Called from readTransport() when
// cl.socket is nil.
func (cl *Client) waitForSocket() {
	// Signal that we've stopped reading from the socket.
	cl.socketSync.Done()

	// Wait until the socket is available again.
	for cl.socket == nil {
		time.Sleep(1e8)
	}

	// Signal that we're going back to the read loop.
	cl.socketSync.Done()
}

// BUG(cjyar): Doesn't implement TLS/SASL EXTERNAL.
func (cl *Client) chooseSasl(fe *Features) {
	var digestMd5 bool
	for _, m := range fe.Mechanisms.Mechanism {
		switch strings.ToLower(m) {
		case "digest-md5":
			digestMd5 = true
		}
	}

	if digestMd5 {
		auth := &auth{XMLName: xml.Name{Space: NsSASL, Local: "auth"}, Mechanism: "DIGEST-MD5"}
		cl.xmlOut <- auth
	}
}

func (cl *Client) handleSasl(srv *auth) {
	switch strings.ToLower(srv.XMLName.Local) {
	case "challenge":
		b64 := base64.StdEncoding
		str, err := b64.DecodeString(srv.Chardata)
		if err != nil {
			if Log != nil {
				Log.Println("SASL challenge decode: " +
					err.Error())
			}
			return
		}
		srvMap := parseSasl(string(str))

		if cl.saslExpected == "" {
			cl.saslDigest1(srvMap)
		} else {
			cl.saslDigest2(srvMap)
		}
	case "failure":
		if Log != nil && Loglevel >= syslog.LOG_NOTICE {
			Log.Println("SASL authentication failed")
		}
	case "success":
		if Log != nil && Loglevel >= syslog.LOG_INFO {
			Log.Println("Sasl authentication succeeded")
		}
		cl.Features = nil
		ss := &stream{To: cl.Jid.Domain, Version: Version}
		cl.xmlOut <- ss
	}
}

func (cl *Client) saslDigest1(srvMap map[string]string) {
	// Make sure it supports qop=auth
	var hasAuth bool
	for _, qop := range strings.Fields(srvMap["qop"]) {
		if qop == "auth" {
			hasAuth = true
		}
	}
	if !hasAuth {
		if Log != nil {
			Log.Println("Server doesn't support SASL auth")
		}
		return
	}

	// Pick a realm.
	var realm string
	if srvMap["realm"] != "" {
		realm = strings.Fields(srvMap["realm"])[0]
	}

	passwd := cl.password
	nonce := srvMap["nonce"]
	digestUri := "xmpp/" + cl.Jid.Domain
	nonceCount := int32(1)
	nonceCountStr := fmt.Sprintf("%08x", nonceCount)

	// Begin building the response. Username is
	// user@domain or just domain.
	var username string
	if cl.Jid.Node == "" {
		username = cl.Jid.Domain
	} else {
		username = cl.Jid.Node
	}

	// Generate our own nonce from random data.
	randSize := big.NewInt(0)
	randSize.Lsh(big.NewInt(1), 64)
	cnonce, err := rand.Int(rand.Reader, randSize)
	if err != nil {
		if Log != nil {
			Log.Println("SASL rand: " + err.Error())
		}
		return
	}
	cnonceStr := fmt.Sprintf("%016x", cnonce)

	/* Now encode the actual password response, as well as the
	 * expected next challenge from the server. */
	response := saslDigestResponse(username, realm, passwd, nonce,
		cnonceStr, "AUTHENTICATE", digestUri, nonceCountStr)
	next := saslDigestResponse(username, realm, passwd, nonce,
		cnonceStr, "", digestUri, nonceCountStr)
	cl.saslExpected = next

	// Build the map which will be encoded.
	clMap := make(map[string]string)
	clMap["realm"] = `"` + realm + `"`
	clMap["username"] = `"` + username + `"`
	clMap["nonce"] = `"` + nonce + `"`
	clMap["cnonce"] = `"` + cnonceStr + `"`
	clMap["nc"] = nonceCountStr
	clMap["qop"] = "auth"
	clMap["digest-uri"] = `"` + digestUri + `"`
	clMap["response"] = response
	if srvMap["charset"] == "utf-8" {
		clMap["charset"] = "utf-8"
	}

	// Encode the map and send it.
	clStr := packSasl(clMap)
	b64 := base64.StdEncoding
	clObj := &auth{XMLName: xml.Name{Space: NsSASL, Local: "response"}, Chardata: b64.EncodeToString([]byte(clStr))}
	cl.xmlOut <- clObj
}

func (cl *Client) saslDigest2(srvMap map[string]string) {
	if cl.saslExpected == srvMap["rspauth"] {
		clObj := &auth{XMLName: xml.Name{Space: NsSASL, Local: "response"}}
		cl.xmlOut <- clObj
	} else {
		clObj := &auth{XMLName: xml.Name{Space: NsSASL, Local: "failure"}, Any: &Generic{XMLName: xml.Name{Space: NsSASL,
			Local: "abort"}}}
		cl.xmlOut <- clObj
	}
}

// Takes a string like `key1=value1,key2="value2"...` and returns a
// key/value map.
func parseSasl(in string) map[string]string {
	re := regexp.MustCompile(`([^=]+)="?([^",]+)"?,?`)
	strs := re.FindAllStringSubmatch(in, -1)
	m := make(map[string]string)
	for _, pair := range strs {
		key := strings.ToLower(string(pair[1]))
		value := string(pair[2])
		m[key] = value
	}
	return m
}

// Inverse of parseSasl().
func packSasl(m map[string]string) string {
	var terms []string
	for key, value := range m {
		if key == "" || value == "" || value == `""` {
			continue
		}
		terms = append(terms, key+"="+value)
	}
	return strings.Join(terms, ",")
}

// Computes the response string for digest authentication.
func saslDigestResponse(username, realm, passwd, nonce, cnonceStr,
	authenticate, digestUri, nonceCountStr string) string {
	h := func(text string) []byte {
		h := md5.New()
		h.Write([]byte(text))
		return h.Sum(nil)
	}
	hex := func(bytes []byte) string {
		return fmt.Sprintf("%x", bytes)
	}
	kd := func(secret, data string) []byte {
		return h(secret + ":" + data)
	}

	a1 := string(h(username+":"+realm+":"+passwd)) + ":" +
		nonce + ":" + cnonceStr
	a2 := authenticate + ":" + digestUri
	response := hex(kd(hex(h(a1)), nonce+":"+
		nonceCountStr+":"+cnonceStr+":auth:"+
		hex(h(a2))))
	return response
}

// Send a request to bind a resource. RFC 3920, section 7.
func (cl *Client) bind(bindAdv *bindIq) {
	res := cl.Jid.Resource
	bindReq := &bindIq{}
	if res != "" {
		bindReq.Resource = &res
	}
	msg := &Iq{Type: "set", Id: <-Id, Nested: []interface{}{bindReq}}
	f := func(st Stanza) bool {
		if st.GetType() == "error" {
			if Log != nil {
				Log.Println("Resource binding failed")
			}
			return false
		}
		var bindRepl *bindIq
		for _, ele := range st.GetNested() {
			if b, ok := ele.(*bindIq); ok {
				bindRepl = b
				break
			}
		}
		if bindRepl == nil {
			if Log != nil {
				Log.Printf("Bad bind reply: %v", st)
			}
			return false
		}
		jidStr := bindRepl.Jid
		if jidStr == nil || *jidStr == "" {
			if Log != nil {
				Log.Println("Can't bind empty resource")
			}
			return false
		}
		jid := new(JID)
		if err := jid.Set(*jidStr); err != nil {
			if Log != nil {
				Log.Println(err.Error())
			}
			return false
		}
		cl.Jid = *jid
		if Log != nil && Loglevel >= syslog.LOG_INFO {
			Log.Println("Bound resource: " + cl.Jid.String())
		}
		cl.bindDone()
		return false
	}
	cl.HandleStanza(msg.Id, f)
	cl.xmlOut <- msg
}

// Register a callback to handle the next XMPP stanza (iq, message, or
// presence) with a given id. The provided function will not be called
// more than once. If it returns false, the stanza will not be made
// available on the normal Client.In channel. The stanza handler
// must not read from that channel, as deliveries on it cannot proceed
// until the handler returns true or false.
func (cl *Client) HandleStanza(id string, f func(Stanza) bool) {
	h := &stanzaHandler{id: id, f: f}
	cl.handlers <- h
}
