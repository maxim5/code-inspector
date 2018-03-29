// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package packet

import (
	"github.com/cmars/go.crypto/openpgp/errors"
	"crypto"
	"crypto/md5"
	"crypto/rsa"
	"encoding/binary"
	"fmt"
	"hash"
	"io"
	"math/big"
	"strconv"
	"time"
)

// PublicKeyV3 represents older version 3 public keys. These keys are insecure
// should not be used for encrypting new messages. They are supported here only for
// validating old signatures or parsing old key material.
// See RFC 4880, section 5.5.2.
type PublicKeyV3 struct {
	CreationTime time.Time
	DaysToExpire uint16
	PubKeyAlgo   PublicKeyAlgorithm
	PublicKey    *rsa.PublicKey
	Fingerprint  [16]byte
	KeyId        uint64
	IsSubkey     bool

	n, e parsedMPI
}

// newRSAPublicKeyV3 returns a PublicKey that wraps the given rsa.PublicKey.
// Included here for testing purposes only. RFC 4880, section 5.5.2:
// "an implementation MUST NOT generate a V3 key, but MAY accept it."
func newRSAPublicKeyV3(creationTime time.Time, pub *rsa.PublicKey) *PublicKeyV3 {
	pk := &PublicKeyV3{
		CreationTime: creationTime,
		PublicKey:    pub,
		n:            fromBig(pub.N),
		e:            fromBig(big.NewInt(int64(pub.E))),
	}

	pk.setFingerPrintAndKeyId()
	return pk
}

func (pk *PublicKeyV3) parse(r io.Reader) (err error) {
	// RFC 4880, section 5.5.2
	var buf [8]byte
	_, err = readFull(r, buf[:])
	if err != nil {
		return
	}
	if buf[0] < 2 || buf[0] > 3 {
		return errors.UnsupportedError("public key version")
	}
	pk.CreationTime = time.Unix(int64(uint32(buf[1])<<24|uint32(buf[2])<<16|uint32(buf[3])<<8|uint32(buf[4])), 0)
	pk.DaysToExpire = binary.BigEndian.Uint16(buf[5:7])
	pk.PubKeyAlgo = PublicKeyAlgorithm(buf[7])
	switch pk.PubKeyAlgo {
	case PubKeyAlgoRSA, PubKeyAlgoRSAEncryptOnly, PubKeyAlgoRSASignOnly:
		err = pk.parseRSA(r)
	default:
		err = errors.UnsupportedError("public key type: " + strconv.Itoa(int(pk.PubKeyAlgo)))
	}
	if err != nil {
		return
	}

	pk.setFingerPrintAndKeyId()
	return
}

func (pk *PublicKeyV3) setFingerPrintAndKeyId() {
	// RFC 4880, section 12.2
	fingerPrint := md5.New()
	fingerPrint.Write(pk.n.bytes)
	fingerPrint.Write(pk.e.bytes)
	copy(pk.Fingerprint[:], fingerPrint.Sum(nil))
	pk.KeyId = binary.BigEndian.Uint64(pk.n.bytes[len(pk.n.bytes)-8 : len(pk.n.bytes)])
}

// parseRSA parses RSA public key material from the given Reader. See RFC 4880,
// section 5.5.2.
func (pk *PublicKeyV3) parseRSA(r io.Reader) (err error) {
	pk.n.bytes, pk.n.bitLength, err = readMPI(r)
	if err != nil {
		return
	}
	pk.e.bytes, pk.e.bitLength, err = readMPI(r)
	if err != nil {
		return
	}

	if len(pk.e.bytes) > 3 {
		err = errors.UnsupportedError("large public exponent")
		return
	}
	rsa := &rsa.PublicKey{
		N: new(big.Int).SetBytes(pk.n.bytes),
		E: 0,
	}
	for i := 0; i < len(pk.e.bytes); i++ {
		rsa.E <<= 8
		rsa.E |= int(pk.e.bytes[i])
	}
	pk.PublicKey = rsa
	return
}

// SerializeSignaturePrefix writes the prefix for this public key to the given Writer.
// The prefix is used when calculating a signature over this public key. See
// RFC 4880, section 5.2.4.
func (pk *PublicKeyV3) SerializeSignaturePrefix(h hash.Hash) {
	var pLength uint16
	switch pk.PubKeyAlgo {
	case PubKeyAlgoRSA, PubKeyAlgoRSAEncryptOnly, PubKeyAlgoRSASignOnly:
		pLength += 2 + uint16(len(pk.n.bytes))
		pLength += 2 + uint16(len(pk.e.bytes))
	default:
		panic("unknown public key algorithm")
	}
	pLength += 6
	h.Write([]byte{0x99, byte(pLength >> 8), byte(pLength)})
	return
}

func (pk *PublicKeyV3) Serialize(w io.Writer) (err error) {
	length := 8 // 8 byte header

	switch pk.PubKeyAlgo {
	case PubKeyAlgoRSA, PubKeyAlgoRSAEncryptOnly, PubKeyAlgoRSASignOnly:
		length += 2 + len(pk.n.bytes)
		length += 2 + len(pk.e.bytes)
	default:
		panic("unknown public key algorithm")
	}

	packetType := packetTypePublicKey
	if pk.IsSubkey {
		packetType = packetTypePublicSubkey
	}
	err = serializeHeader(w, packetType, length)
	if err != nil {
		return
	}
	return pk.serializeWithoutHeaders(w)
}

// serializeWithoutHeaders marshals the PublicKey to w in the form of an
// OpenPGP public key packet, not including the packet header.
func (pk *PublicKeyV3) serializeWithoutHeaders(w io.Writer) (err error) {
	var buf [8]byte
	// Version 3
	buf[0] = 3
	// Creation time
	t := uint32(pk.CreationTime.Unix())
	buf[1] = byte(t >> 24)
	buf[2] = byte(t >> 16)
	buf[3] = byte(t >> 8)
	buf[4] = byte(t)
	// Days to expire
	buf[5] = byte(pk.DaysToExpire >> 8)
	buf[6] = byte(pk.DaysToExpire)
	// Public key algorithm
	buf[7] = byte(pk.PubKeyAlgo)

	_, err = w.Write(buf[:])
	if err != nil {
		return
	}

	switch pk.PubKeyAlgo {
	case PubKeyAlgoRSA, PubKeyAlgoRSAEncryptOnly, PubKeyAlgoRSASignOnly:
		return writeMPIs(w, pk.n, pk.e)
	}
	return errors.InvalidArgumentError("bad public-key algorithm")
}

// CanSign returns true iff this public key can generate signatures
func (pk *PublicKeyV3) CanSign() bool {
	return pk.PubKeyAlgo != PubKeyAlgoRSAEncryptOnly
}

// VerifySignatureV3 returns nil iff sig is a valid signature, made by this
// public key, of the data hashed into signed. signed is mutated by this call.
func (pk *PublicKeyV3) VerifySignatureV3(signed hash.Hash, sig *SignatureV3) (err error) {
	if !pk.CanSign() {
		return errors.InvalidArgumentError("public key cannot generate signatures")
	}

	suffix := make([]byte, 5)
	suffix[0] = byte(sig.SigType)
	binary.BigEndian.PutUint32(suffix[1:], uint32(sig.CreationTime.Unix()))
	signed.Write(suffix)
	hashBytes := signed.Sum(nil)

	if hashBytes[0] != sig.HashTag[0] || hashBytes[1] != sig.HashTag[1] {
		return errors.SignatureError("hash tag doesn't match")
	}

	if pk.PubKeyAlgo != sig.PubKeyAlgo {
		return errors.InvalidArgumentError("public key and signature use different algorithms")
	}

	switch pk.PubKeyAlgo {
	case PubKeyAlgoRSA, PubKeyAlgoRSASignOnly:
		err = rsa.VerifyPKCS1v15(pk.PublicKey, sig.Hash, hashBytes, sig.RSASignature.bytes)
		if err != nil {
			return errors.SignatureError("RSA verification failure")
		}
		return nil
	default:
		panic("shouldn't happen")
	}
	panic("unreachable")
}

// VerifyUserIdSignatureV3 returns nil iff sig is a valid signature, made by this
// public key, of id.
func (pk *PublicKeyV3) VerifyUserIdSignatureV3(id string, sig *SignatureV3) (err error) {
	h, err := userIdSignatureV3Hash(id, pk, sig.Hash)
	if err != nil {
		return err
	}
	return pk.VerifySignatureV3(h, sig)
}

// VerifyKeySignature returns nil iff sig is a valid signature, made by this
// public key, of signed.
func (pk *PublicKeyV3) VerifyKeySignatureV3(signed *PublicKeyV3, sig *SignatureV3) (err error) {
	h, err := keySignatureHash(pk, signed, sig.Hash)
	if err != nil {
		return err
	}
	return pk.VerifySignatureV3(h, sig)
}

// userIdSignatureV3Hash returns a Hash of the message that needs to be signed
// to assert that pk is a valid key for id.
func userIdSignatureV3Hash(id string, pk signingKey, hfn crypto.Hash) (h hash.Hash, err error) {
	h = hfn.New()
	if h == nil {
		return nil, errors.UnsupportedError("hash function")
	}

	// RFC 4880, section 5.2.4
	pk.SerializeSignaturePrefix(h)
	pk.serializeWithoutHeaders(h)

	h.Write([]byte(id))

	return
}

// KeyIdString returns the public key's fingerprint in capital hex
// (e.g. "6C7EE1B8621CC013").
func (pk *PublicKeyV3) KeyIdString() string {
	return fmt.Sprintf("%X", pk.KeyId)
}

// KeyIdShortString returns the short form of public key's fingerprint
// in capital hex, as shown by gpg --list-keys (e.g. "621CC013").
func (pk *PublicKeyV3) KeyIdShortString() string {
	return fmt.Sprintf("%X", pk.KeyId&0xFFFFFFFF)
}

// BitLength returns the bit length for the given public key.
func (pk *PublicKeyV3) BitLength() (bitLength uint16, err error) {
	switch pk.PubKeyAlgo {
	case PubKeyAlgoRSA, PubKeyAlgoRSAEncryptOnly, PubKeyAlgoRSASignOnly:
		bitLength = pk.n.bitLength
	default:
		err = errors.InvalidArgumentError("bad public-key algorithm")
	}
	return
}
