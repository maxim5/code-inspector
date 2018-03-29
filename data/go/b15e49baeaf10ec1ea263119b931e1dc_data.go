package data

import (
	"encoding/xml"
	"gongo/preferences"
	_ "gongo/data/field" // Imported only to initialize it first
	"os"
)

// Data is the base type for data structure definition
type Data struct {
	DBs       []DB `xml:"DB"`
	dbDict    map[string]DB
	defaultDB DB
}

var (
	data Data
)

// GetDB returns the named database definition.
func GetDB(name string) DB {
	return data.dbDict[name]
}

// GetDefaultDB returns the definition of the default database
func GetDefaultDB() DB {
	return data.defaultDB
}

func init() {
	data.dbDict = make(map[string]DB)
	file, err := os.Open(preferences.Get("data model"))
	if err != nil {
		preferences.ErrorLog.Panicf("Could not open data model file \"%v\". Error: %v.\n",
			preferences.Get("data model"), err)
	}
	err = xml.NewDecoder(file).Decode(&data)
	if err != nil {
		preferences.ErrorLog.Panicf("Could not parse data model file \"%v\". Error: %v.\n",
			preferences.Get("data model"), err)
	}
	for _, value := range data.DBs {
		data.dbDict[value.Name] = value
		if value.Default {
			data.defaultDB = value
		}
		value.parseModels()
	}
}

// BUG(Leandro): Package data still does not support third-party packages. Should have a
// Register(definition []byte) function.
