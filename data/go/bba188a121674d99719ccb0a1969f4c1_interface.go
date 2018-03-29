// interface.go
//
// Score Interface
//
// This provides the interface that the score interfaces need to conform to.

package main

import (
	o "orchestra"
	"os"
)

var (
	interfaces = make(map[string]func(*TaskRequest) ScoreInterface)
)

type ExecutionEnvironment struct {
	Environment map[string]string
	Arguments   []string
	Files       []*os.File
}

func NewExecutionEnvironment() (ee *ExecutionEnvironment) {
	ee = new(ExecutionEnvironment)
	ee.Environment = make(map[string]string)

	return ee
}

type ScoreInterface interface {
	// prepare gets called up front before the fork.  It should do
	// any/all lifting required.
	//
	// returns false if there are any problems.
	Prepare() bool

	// SetupEnvironment gets called prior to starting the child
	// process.  It should return an ExecutionEnvironment with the
	// bits filled in the way it wants.
	SetupProcess() *ExecutionEnvironment

	// Cleanup is responsible for mopping up the mess, filing any
	// results that need to be stored, etc.  This will be called
	// only from the main thread to ensure that results can be updated
	// safely.
	Cleanup()
}

func HasInterface(ifname string) bool {
	_, exists := interfaces[ifname]

	return exists
}

func RegisterInterface(ifname string, initfunc func(*TaskRequest) ScoreInterface) {
	_, exists := interfaces[ifname]
	if exists {
		o.Assert("Multiple attempts to register %s interface", ifname)
	}
	interfaces[ifname] = initfunc
}

func NewScoreInterface(task *TaskRequest) (iface ScoreInterface) {
	score, exists := Scores[task.Score]
	if !exists {
		return nil
	}
	if !HasInterface(score.Interface) {
		return nil
	}
	ifinit, _ := interfaces[score.Interface]

	iface = ifinit(task)

	return iface
}
