package inbox

import (
	"errors"
	"fmt"
	"labix.org/v2/mgo"
	"labix.org/v2/mgo/bson"
	"time"
)

// Data we want to store for each message 
type EmailData struct {
	Domain  string
	Address string
	Name    string
	Uid     uint32
	Date    time.Time
	Size    uint32
	User_Id int
}

type GetEmailResults []GetEmailResult

type GetEmailResult struct {
	Id    string "_id"
	Value *GetEmailValue
}

type GetEmailValue struct {
	Size  int
	Count int
	Uids  []uint32
}

// Interface to writing email data somewhere
type EmailDataWriter interface {
	// Take in email data, write it somewhere
	WriteEmailData(ed EmailData)
}

type EmailDataAPI interface {
	GetEmailData(user_id int, grouping string) *GetEmailResult
	DeleteOldData(user_id int)
	//ModifyEmails(user_id int, action string, uids []uint32)
}

// Test implementation of EmailDataWriter
type TerminalEmailDataWriter bool

// Write EmailData to a terminal
func (tedw TerminalEmailDataWriter) WriteEmailData(ed EmailData) {
	fmt.Printf("%s,%s,%s,%d,%s,%d,%d\n", ed.Domain, ed.Address, ed.Name, ed.Uid, ed.Date.Format("2006-01-02"), ed.Size, ed.User_Id)
}

func (tedw TerminalEmailDataWriter) DeleteOldData(user_id int) {
	// do nothing
}

// Write EmailData to MongoDB
type MongoEmailData struct {
	email       *mgo.Collection
	validGroups map[string]bool
}

// Constructor for MongoEmailData
func NewMongoEmailData(mongo_url string, db string) (med *MongoEmailData) {
	med = new(MongoEmailData)

	// Connect to mongo
	session, err := mgo.Dial(mongo_url)

	if err != nil {
		panic(err)
	}

	// Get "email" collection
	med.email = session.DB(db).C("email")

	// Ensure unique index on uids per user_id
	index := mgo.Index{
		Key:      []string{"user_id", "uid"},
		Unique:   true,
		DropDups: true,
	}

	err = med.email.EnsureIndex(index)

	if err != nil {
		panic(err)
	}

	// Set validGroups in map/reduce functions
	med.validGroups = map[string]bool{"address": true, "domain": true, "name": true}

	return med
}

// Write EmailData to MongoDB
func (med *MongoEmailData) WriteEmailData(ed EmailData) {
	err := med.email.Insert(&ed)

	if err != nil {
		fmt.Println("Unable to insert, skipping: ", err)
	}
}

/*
function (key, values) { var res = {size:0, count:0, uids:[]}; for (var i = 0; i < values.length; i++) { res.count += values[i].count; res.size += values[i].size; res.uids = res.uids.concat(values[i].uids); } return res; }
*/

/*
function () {
    emit(this.address, {size:this.size, count:1, uids:[this.uid]});
}
*/

func (med *MongoEmailData) generateReducer(group string) (string, error) {

	return "function (key, values) { var res = {size:0, count:0, uids:[]}; for (var i = 0; i < values.length; i++) { res.count += values[i].count; res.size += values[i].size; res.uids = res.uids.concat(values[i].uids); } return res; }", nil
}

func (med *MongoEmailData) generateMapper(group string) (mapper string, err error) {

	if med.validGroups[group] {
		mapper = fmt.Sprintf("function () { emit(this.%s, {size:this.size, count:1, uids:[this.uid]}); }", group)
	} else {
		err = errors.New(fmt.Sprintf("Invalid group: %s", group))
	}

	return mapper, err
}

func (med *MongoEmailData) GetEmailData(user_id int, group string) (*GetEmailResults, error) {

	// Create a mapper for this grouping
	mapper, err := med.generateMapper(group)
	if err != nil {
		//	return nil, err
		panic(err)
	}

	// Create a reducer for this grouping
	reducer, err := med.generateReducer(group)
	if err != nil {
		//	return nil, err
		panic(err)
	}

	job := &mgo.MapReduce{
		Map:    mapper,
		Reduce: reducer,
	}
	var result GetEmailResults

	med.email.Find(bson.M{"user_id": user_id}).MapReduce(job, &result)

	return &result, nil
}

func (med *MongoEmailData) DeleteOldData(user_id int) {
	med.email.RemoveAll(bson.M{"user_id": user_id})
}
