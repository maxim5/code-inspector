// Copyright 2010  The "Go-Hasher" Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package hasher

import (
	"bytes"
	"crypto/rand"
	"errors"
)

// === Read

// Creates an hasher from schema in binary format.
func ReadFromBinary(format []byte) (*hasher, error) {
	_hasher := new(hasher)

	// Get the header to get its fields.
	data := bytes.NewBuffer(format)
	header := data.Next(_HEADER_SIZE)

	_hasher.header.idHash = header[0]
	_hasher.header.saltSize = header[1]

	// Get the hash function.
	_hashType, ok := valueToHash[header[0]]
	if !ok {
		return nil, errors.New("wrong identifier of cryptographic hash function")
	}
	_hasher.Hash = &_hashType

	// Size
	binarySize := len(format)
	_hasher.size = getSizes(binarySize)

	return _hasher, nil
}

// === Get

// Returns an hash for `str` in binary format.
func (h *hasher) GetBinary(str string) ([]byte, error) {
	format := new(bytes.Buffer)

	// Create the random salt.
	salt := make([]byte, h.header.saltSize)
	if _, err := rand.Read(salt); err != nil {
		return nil, err
	}

	// Get the digest.
	digest := h.getDigest(salt, str)

	// Create the output format.
	_ = format.WriteByte(h.header.idHash)   // Hash function identifier.
	_ = format.WriteByte(h.header.saltSize) // Salt size.
	_, _ = format.Write(salt)                  // Salt.
	_, _ = format.Write(digest)                // Message digest.

	return format.Bytes(), nil
}

// Returns the size for output in binary.
func (h *hasher) BinarySize() int {
	return int(h.size.binary)
}

// === Test

// Checks an hash.
//
// Creates an hash using `str` and checks if is the same than got from `format`.
func (h *hasher) IsValidBinary(str string, format []byte) (bool, error) {
	if len(format) != h.BinarySize() {
		return false, errors.New("size of binary format not valid")
	}

	// Get the binary format.
	data := bytes.NewBuffer(format)

	// Skip the header.
	data.Next(_HEADER_SIZE)

	// Get the fields.
	formatSalt := data.Next(int(h.header.saltSize))
	formatDigest := data.Next(h.Hash.Size())

	// Create the digest using the salt given on the binary format.
	digest := h.getDigest(formatSalt, str)

	return bytes.Equal(digest, formatDigest), nil
}
