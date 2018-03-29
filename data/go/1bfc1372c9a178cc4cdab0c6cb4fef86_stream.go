// Copyright 2010 Gary Burd
//
// Licensed under the Apache License, Version 2.0 (the "License"): you may
// not use this file except in compliance with the License. You may obtain
// a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.

// Package twitterstream implements the basic functionality for accessing the
// Twitter streaming APIs. See http://dev.twitter.com/pages/streaming_api for
// information on the Twitter streaming APIs.
//
// The following example shows how to handle dropped connections. If it's
// important for the application to see every tweet in the stream, then the
// application should backfill the stream using the Twitter search API after
// each connection attempt.
//
//  waitUntil := time.Now()
//  for {
//      // Rate limit connection attempts to once every 30 seconds.
//      if d := waitUntil.Sub(time.Now()); d > 0 {
//          time.Sleep(d)
//      }
//      waitUntil = time.Now().Add(30 * time.Second)
//
//      ts, err := twitterstream.Open(client, cred, url, params)
//      if err != nil {
//          log.Println("error opening stream: ", err)
//          continue
//      }
//
//      // Loop until stream has a permanent error.
//      for ts.Err() == nil {
//          var t MyTweet
//          if err := ts.UnmarshalNext(&t); err != nil {
//              log.Println("error reading tweet: ", err)
//              continue
//          } 
//          process(&t) 
//      }
//      ts.Close()
//  }
//          
package twitterstream

import (
	"bufio"
	"bytes"
	"crypto/tls"
	"encoding/json"
	"errors"
	"github.com/garyburd/go-oauth/oauth"
	"io/ioutil"
	"net"
	"net/url"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// Stream manages the connection to a Twitter streaming endpoint.
type Stream struct {
	chunkRemaining int64
	chunkState     int
	conn           net.Conn
	r              *bufio.Reader
	err            error
}

// HTTPStatusError represents an HTTP error return from the Twitter streaming
// API endpoint.
type HTTPStatusError struct {
	// HTTP status code.
	StatusCode int

	// Response body.
	Message string
}

func (err HTTPStatusError) Error() string {
	return "twitterstream: status=" + strconv.Itoa(err.StatusCode) + " " + err.Message
}

var responseLineRegexp = regexp.MustCompile("^HTTP/[0-9.]+ ([0-9]+) ")

// Open opens a new stream.
func Open(oauthClient *oauth.Client, accessToken *oauth.Credentials, urlStr string, params url.Values) (*Stream, error) {
	ts := new(Stream)

	u, err := url.Parse(urlStr)
	if err != nil {
		return nil, err
	}

	addr := u.Host
	if strings.LastIndex(addr, ":") <= strings.LastIndex(addr, "]") {
		if u.Scheme == "http" {
			addr = addr + ":80"
		} else {
			addr = addr + ":443"
		}
	}

	if u.Scheme == "http" {
		ts.conn, err = net.Dial("tcp", addr)
		if err != nil {
			return nil, err
		}
	} else {
		ts.conn, err = tls.Dial("tcp", addr, nil)
		if err != nil {
			return nil, err
		}
		if err = ts.conn.(*tls.Conn).VerifyHostname(addr[:strings.LastIndex(addr, ":")]); err != nil {
			return nil, ts.fatal(err)
		}
	}

	// Setup request body.
	pcopy := url.Values{}
	for key, values := range params {
		pcopy[key] = values
	}
	oauthClient.SignParam(accessToken, "POST", urlStr, pcopy)
	body := pcopy.Encode()

	var req bytes.Buffer
	req.WriteString("POST ")
	req.WriteString(u.RequestURI())
	req.WriteString(" HTTP/1.1")
	req.WriteString("\r\nHost: ")
	req.WriteString(u.Host)
	req.WriteString("\r\nContent-Type: application/x-www-form-urlencoded")
	req.WriteString("\r\nContent-Length: ")
	req.WriteString(strconv.Itoa(len(body)))
	req.WriteString("\r\n\r\n")
	req.WriteString(body)
	_, err = ts.conn.Write(req.Bytes())
	if err != nil {
		return nil, ts.fatal(err)
	}

	// Must connect in 60 seconds.
	err = ts.conn.SetReadDeadline(time.Now().Add(60 * time.Second))
	if err != nil {
		return nil, ts.fatal(err)
	}

	ts.r = bufio.NewReaderSize(ts.conn, 8192)
	p, err := ts.r.ReadSlice('\n')
	if err != nil {
		return nil, ts.fatal(err)
	}

	m := responseLineRegexp.FindSubmatch(p)
	if m == nil {
		return nil, ts.fatal(errors.New("twitterstream: bad http response line"))
	}

	// Skip headers
	for {
		p, err = ts.r.ReadSlice('\n')
		if err != nil {
			return nil, ts.fatal(err)
		}
		if len(p) <= 2 {
			break
		}
	}

	statusCode, _ := strconv.Atoi(string(m[1]))
	if statusCode != 200 {
		p, _ := ioutil.ReadAll(ts.r)
		return nil, HTTPStatusError{statusCode, string(p)}
	}

	ts.chunkState = stateStart
	return ts, nil
}

func (ts *Stream) fatal(err error) error {
	if ts.conn != nil {
		ts.conn.Close()
	}
	if ts.err == nil {
		ts.err = err
	}
	return err
}

// Close releases the resources used by the stream.
func (ts *Stream) Close() error {
	if ts.err != nil {
		return ts.err
	}
	return ts.conn.Close()
}

// Err returns a non-nil value if the stream has a permanent error.
func (ts *Stream) Err() error {
	return ts.err
}

const (
	stateStart = iota
	stateEnd
	stateNormal
)

// Next returns the next line from the stream. The returned slice is
// overwritten by the next call to Next.
func (ts *Stream) Next() ([]byte, error) {
	if ts.err != nil {
		return nil, ts.err
	}
	for {
		// Twitter sends at least one ine of text every 30 seconds.
		err := ts.conn.SetReadDeadline(time.Now().Add(60 * time.Second))
		if err != nil {
			return nil, ts.fatal(err)
		}

		p, err := ts.r.ReadSlice('\n')
		if err != nil {
			return nil, ts.fatal(err)
		}

		switch ts.chunkState {
		case stateStart:
			ts.chunkRemaining, err = strconv.ParseInt(string(p[:len(p)-2]), 16, 64)
			switch {
			case err != nil:
				return nil, ts.fatal(errors.New("error parsing chunk size"))
			case ts.chunkRemaining == 0:
				return nil, ts.fatal(errors.New("end of chunked stream"))
			}
			ts.chunkState = stateNormal
			continue
		case stateEnd:
			ts.chunkState = stateStart
			continue
		case stateNormal:
			ts.chunkRemaining = ts.chunkRemaining - int64(len(p))
			if ts.chunkRemaining == 0 {
				ts.chunkState = stateEnd
			}
		}

		if len(p) <= 2 {
			continue // ignore keepalive line
		}

		return p, nil
	}
	panic("should not get here")
}

// UnmarshalNext reads the next line of from the stream and decodes the line as
// JSON to data. This is a convenience function for streams with homogeneous
// entity types. 
func (ts *Stream) UnmarshalNext(data interface{}) error {
	p, err := ts.Next()
	if err != nil {
		return err
	}
	return json.Unmarshal(p, data)
}
