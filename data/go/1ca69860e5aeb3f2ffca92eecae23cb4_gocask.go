/*
   Implementation of the bitcask key-value store from Riak.
   Paper available at: http://downloads.basho.com/papers/bitcask-intro.pdf
   The key-value pairs is stored in a log file, which only writes to the append, so a write never include a disk seek.
   Each record is stored in the following format:

   	|-------------------------------------------------------------------------------------------------------------------|
	|crc (int32) | tstamp (int32) | key length (int32) | value length (int32) | key data ([]byte) | value data ([]byte) |
	|-------------------------------------------------------------------------------------------------------------------|
*/
package gocask

import (
	"errors"
	"os"
	"path"
)

/*
   Main structure for any gocask file.
   Holds the current directory and the active file
*/
type Gocask struct {
	directory  string
	activeFile *GFile
	permission int
	keydir     *Keydir
}

var ErrKeyNotFound = errors.New("Key not found")

/*
   Open the key-value store at the given directory.
   If the directory doesn't exist one will be created.
   Always try to open for read-write, but if someone is already using this directory, open in read-only mode.

   Populate the KeyDir strucutre with the information obtained from the scan of all the data files in the directory.
*/
func NewGocask(directory string) (b *Gocask, err error) {
	err = os.MkdirAll(directory, 0766)
	if err != nil {
		return nil, err
	}

	goCask := new(Gocask)
	var activeFile *os.File

	goCask.directory = directory
	goCask.keydir = NewKeydir()
	activeFile, err = goCask.openActiveFile()

	if err != nil {
		return nil, err
	}
	goCask.activeFile = NewGFile(activeFile)

	err = goCask.populateKeyDir()

	return goCask, nil
}

/*
	Close the key-value store.
*/
func (g *Gocask) Close() error {
	var err error
	if g.activeFile != nil {
		err = g.activeFile.file.Close()
	}

	return err
}

/*
   Get the hands in the active file (the one that receives the changes).
   And load the keydir memory structure.
*/
func (g *Gocask) openActiveFile() (file *os.File, err error) {
	activeFilePath := path.Join(g.directory, "gocask")
	return os.OpenFile(activeFilePath, os.O_CREATE|os.O_APPEND|os.O_RDWR, 0766)
}

/*
	Save the key-value pair in the current file.
*/
func (g *Gocask) Put(key string, value []byte) error {
	if g.keydir == nil {
		return errors.New("key dir is invalid")
	}

	if g.activeFile == nil {
		return errors.New("Active file is not defined")
	}

	return g.keydir.WriteTo(g.activeFile, key, value)
}

/*
	Retreives the value for the given if from the keystore
*/
func (g *Gocask) Get(key string) (value []byte, err error) {
	kde := g.keydir.keys[key]
	if kde == nil {
		err = ErrKeyNotFound
		value = nil
	} else {
		return kde.readValue()
	}

	return
}

/*
   Read the contents at the given directory and load it into the memory.
*/
func (g *Gocask) populateKeyDir() error {
	return g.keydir.Fill(g.activeFile)
}
