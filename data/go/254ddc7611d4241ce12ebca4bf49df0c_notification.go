// Copyright 2011 Nicolas Paton. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package apn

import (
	"encoding/hex"
	"encoding/binary"
	"io"
	"os"
	"json"
	"bytes"
	"time"
)


type Notification struct {
	DeviceToken string                 // 64 byte device token string
	Expiry      uint32                 // Expiry time in seconds since Unix epoch. (If set to 0, will set epiry to: now + 1 year)
	Payload     map[string]interface{} // Payload should be formated to Apple specs, see Apple documentation and this pkg's sample code.

	Sent  bool     // True if sent, amazing
	Error os.Error // Error is only assigned when the notification is sent back on the Q Error chan.

	queue      *Queue // Parent queue
	identifier uint32 // The identifier used to find on erronerous notifications
	tries      int    // Number of times tried to be sent
}

const NUMBER_OF_RETRIES = 2

// You should expect to receive these errors on the notification sent back on the 'queue.Error' channel
var (
	PayloadTooLargeError     = os.NewError("Payload size exceeds limit (255 bytes)")
	BadDeviceToken           = os.NewError("The given device token is not a valid device token")
	PayloadJSONEncodingError = os.NewError("Notification's payload cannot be encoded to JSON, it must not respect the json pkg input format")
	RefusedByApple           = os.NewError("After several tries, the notification can't find it's way to Apple. (Can be a connection error!)")
)

// 64 byte device token string. payload should be formated to Apple specs, see documentation. Expiry time in seconds since Unix epoch (0 for 1 year).
func NewNotification(token string, payload map[string]interface{}, expiry uint32) *Notification {

	if expiry == 0 {
		expiry = uint32(time.Seconds() + (60 * 60 * 24 * 360))
	}

	return &Notification{
		Payload:     payload,
		DeviceToken: token,
		Expiry:      expiry,
	}
}

func (n *Notification) validateNotification() (err os.Error) {
	if len(n.DeviceToken) != 64 {
		err = BadDeviceToken
	}

	pload, err := n.jsonPayload()
	if err != nil {
		err = PayloadJSONEncodingError
	}

	if len(pload) > 255 {
		err = PayloadTooLargeError
	}

	_, err = hex.DecodeString(n.DeviceToken)
	if err != nil {
		err = BadDeviceToken
	}

	n.Error = err

	return
}

func (n *Notification) shouldRetry() bool {
	return n.tries < NUMBER_OF_RETRIES && n.Error == nil
}

func (n *Notification) jsonPayload() (string, os.Error) {
	bytes, err := json.Marshal(n.Payload)
	return hex.EncodeToString(bytes), err
}


func (n *Notification) writeTo(writer io.Writer) (written int, err os.Error) {
	payload, err := n.jsonPayload()

	// These error checks can be redundant with a previous validateNotification() call
	if err != nil {
		err = PayloadJSONEncodingError
		return
	}

	if len(payload) > 255 {
		err = PayloadTooLargeError
		return
	}

	deviceToken, err := hex.DecodeString(n.DeviceToken)
	if err != nil {
		err = BadDeviceToken
		return
	}

	// Buffer in which to stuff the full payload
	buffer := bytes.NewBuffer([]byte{})

	// Command, by spec, always send 1 for now, only one command available: "send".
	binary.Write(buffer, binary.BigEndian, uint8(1))

	// Identifier, used to find an erroneous notification when an error message is sent back from Apple 
	binary.Write(buffer, binary.BigEndian, uint32(n.identifier))

	// Expiry.
	binary.Write(buffer, binary.BigEndian, uint32(n.Expiry))

	// Device token. In hexadecimal format.
	binary.Write(buffer, binary.BigEndian, uint16(len(deviceToken)))
	binary.Write(buffer, binary.BigEndian, deviceToken)

	// Actual payload
	binary.Write(buffer, binary.BigEndian, uint16(len(payload)))
	binary.Write(buffer, binary.BigEndian, payload)

	// Write the bytes
	payloadBytes := buffer.Bytes()
	written, err = writer.Write(payloadBytes)
	return
}
