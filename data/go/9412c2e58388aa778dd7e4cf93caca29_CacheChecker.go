package govtc

import (
	"bufio"
	"database/sql"
	"fmt"
	"os"
	_"time"
	"sync"
	"strings"
	_ "github.com/mattn/go-sqlite3"
	"github.com/williballenthin/govt"
)

type CacheChecker struct {
	apiKeys      	[]string
	databasePath    string
	checkLimit      int
	tasksQueue      chan BatchData
	resultsQueue    chan govt.FileReportResults
	workerQueue     chan *VtChecker
	quit 			chan bool
	waitGroup       sync.WaitGroup
	db				*sql.DB
}

const SQL_SELECT_MD5 string = "SELECT id, md5, sha256, positives, total, permalink, responsecode, scans, scandate, updatedate from vthashes where md5 = ?"
const SQL_SELECT_SHA256 string = "SELECT id, md5, sha256, positives, total, permalink, responsecode, scans, scandate, updatedate from vthashes where sha256 = ?"

func NewCacheChecker(apiKeys []string, databasePath string) *CacheChecker {
	c := new(CacheChecker)
	c.apiKeys = apiKeys
	c.databasePath = databasePath
	c.checkLimit = 4
	return c
}

func (cc *CacheChecker) ProcessFile(hashChannel chan *VtRecord,
								    inputFile string,
								    mode int) {
	cc.startDispatcher()
	cc.manage()

	fmt.Println("Opening database: " + cc.databasePath)

	// Open the database containing our VT data
	var err error
	cc.db, err = sql.Open("sqlite3", cc.databasePath)
	defer cc.db.Close()
	if err != nil {
		// Error? callback?
		return
	}

	// Prepare the SQL statements
	stmtMd5, err := cc.db.Prepare(SQL_SELECT_MD5)
	if err != nil {
		fmt.Println("MD5 statement error: " + err.Error())
	}
	defer stmtMd5.Close()

	stmtSha256, err := cc.db.Prepare(SQL_SELECT_SHA256)
	if err != nil {
		fmt.Println("SHA256 statement error: " + err.Error())
	}
	defer stmtSha256.Close()

	file, _ := os.Open(inputFile)
	defer file.Close()
	scanner := bufio.NewScanner(file)
	line := ""
	//batchData := BatchData{}
	tempHashes := make([]string, 0, cc.checkLimit)
	for scanner.Scan() {
		fmt.Println("New line")
		line = strings.TrimSpace(scanner.Text())
		if len(line) == 0 {
			continue
		}

		hashType := GetHashTypeFromLength(line)
		if hashType == HASH_UNKNOWN {
			continue
		}

		vtRecord, err := isHashInDatabase(stmtMd5, stmtSha256, line)
		if err != nil {
			if strings.Contains(err.Error(), "no rows in result set") == false {
				fmt.Println(err.Error())
				continue
			} else {
				fmt.Println("Hash not identified in database")
				if mode == MODE_CACHE || mode == MODE_LIVE {
					fmt.Println("Adding temp hash")
					tempHashes = append(tempHashes, line)
					fmt.Println("got here2")
					if len(tempHashes) >= cc.checkLimit {
						fmt.Println("got here4")
						batchData := BatchData{}
						//batchCheck := new(BatchCheck)
						batchData.Add(tempHashes)
						tempHashes = make([]string, cc.checkLimit)
						cc.waitGroup.Add(1)
						cc.tasksQueue <- batchData
					}

					fmt.Println("got here1")
				} else {
					// The hash wasn't in database so just return a result
					vt := new(VtRecord)
					if hashType == HASH_MD5 {
						vt.Md5 = line
					} else {
						vt.Sha256 = line
					}
					vt.ResponseCode = 0

					// Hash identified? Callback
					hashChannel <-vt
				}
				fmt.Println("got here3")

			}
		} else {
			// Hash identified? Callback
			hashChannel <-vtRecord
		}
	}

	fmt.Println("Completed all checks")

	fmt.Println(len(tempHashes))

	// Have we reached the end and still have a partially filled slice?
	if len(tempHashes) > 0 {
		batchData := BatchData{}
		batchData.Add(tempHashes)
		tempHashes = nil
		cc.waitGroup.Add(1)
		cc.tasksQueue <- batchData
	}

	// Wait for all of the tasks to get a result
	cc.waitGroup.Wait()

	cc.quit <- true
	close(cc.tasksQueue)
	close(cc.resultsQueue)
	close(cc.workerQueue)
}

func (cc *CacheChecker) startDispatcher() {
	cc.workerQueue = make(chan *VtChecker, len(cc.apiKeys))
	cc.tasksQueue = make(chan BatchData)
	cc.resultsQueue = make(chan govt.FileReportResults)
	cc.quit = make(chan bool, 1)

	fmt.Println("Starting workers")

	for index, apikey := range cc.apiKeys {
		fmt.Println(fmt.Sprintf("Starting worker: %d", index + 1))
		cc.workerQueue <- NewVtChecker(index + 1,
									   apikey,
								       cc.databasePath,
									   cc.workerQueue,
									   cc.resultsQueue)
	}
}

//
func (cc *CacheChecker) manage () {
	go func() {
		fmt.Println("**manage.start")
		for {
			fmt.Println("**manage.loop")
			select {
			case task := <-cc.tasksQueue:
				fmt.Println("**task")
				fmt.Println(len(task.Hashes))
				go cc.dispatch(task)
			case result := <-cc.resultsQueue:
				fmt.Println("**result")
				go cc.process(result)

			case <-cc.quit:
				fmt.Println("**quit")
			cc.quit <- true
				return
			}
		}
	}()
}


func (cc *CacheChecker) dispatch(batch BatchData) {
	fmt.Println("Dispatching task:")
	select {
	case worker := <-cc.workerQueue:
		fmt.Println("**task2")
		fmt.Println(len(batch.Hashes))
		go worker.Work(batch)
	case <-cc.quit:
		cc.quit <- true
		return
	}
}

//
func (cc *CacheChecker) process(ftr govt.FileReportResults) {
	fmt.Println("Got results:")
	//fmt.Println(ftr)
	for _, fr := range ftr {
		fmt.Println(fr.Md5)
	}

	//cc.db.Query("SELECT sha256, positives, total, permalink, responsecode, scans, scandate, updatedate FROM vthashes WHERE md5=?", )

	cc.waitGroup.Done()


//	if (report.ResponseCode == 0)
//	{
//		// The hash wasn't in VT
//		Hash temp = new Hash();
//		temp.Md5 = report.Resource;
//		temp.Response = 0;
//		OnHashChecked(temp);
//		continue;
//	}
//
//	var ret = _db.SingleOrDefault<Hash>("WHERE Md5 = @0", report.Md5);
//	if (ret == null)
//	{
//		Hash temp = new Hash();
//		temp.Md5 = report.Md5;
//		temp.Sha256 = report.Sha256;
//		temp.Positive = report.Positives;
//		temp.Total = report.Total;
//		temp.Permalink = report.Permalink;
//		temp.ScanDate = report.ScanDate;
//		temp.Response = (byte)report.ResponseCode;
//		temp.UpdateDate = DateTime.Now;
//		temp.Scans = Functions.GenerateScansString(report);
//
//		_db.Insert(temp);
//		OnHashChecked(temp);
//	}
//	else
//{
//	ret.Positive = report.Positives;
//	ret.Md5 = report.Md5;
//	ret.Sha256 = report.Sha256;
//	ret.Total = report.Total;
//	ret.Permalink = report.Permalink;
//	ret.ScanDate = report.ScanDate;
//	ret.Response = (byte)report.ResponseCode;
//	ret.Scans = Functions.GenerateScansString(report);
//	ret.UpdateDate = DateTime.Now;
//
//	_db.Update(ret);
//	OnHashChecked(ret);
//}
}


//
func isHashInDatabase(stmtMd5 *sql.Stmt,
					  stmtSha265 *sql.Stmt,
					  hash string) (*VtRecord, error) {
	vtRecord := new(VtRecord)
	if len(hash) == 32 {
		err := stmtMd5.QueryRow(hash).Scan(&vtRecord.Id,
										   &vtRecord.Md5,
			                               &vtRecord.Sha256,
										   &vtRecord.Positives,
			                               &vtRecord.Total,
										   &vtRecord.PermaLink,
			                               &vtRecord.ResponseCode,
										   &vtRecord.Scans,
										   &vtRecord.ScanDate,
										   &vtRecord.UpdateDate)
		if err != nil {
			return vtRecord, err
		}
	} else {
		err := stmtSha265.QueryRow(hash).Scan(&vtRecord.Id,
											  &vtRecord.Md5,
											  &vtRecord.Sha256,
											  &vtRecord.Positives,
											  &vtRecord.Total,
											  &vtRecord.PermaLink,
											  &vtRecord.ResponseCode,
											  &vtRecord.Scans,
											  &vtRecord.ScanDate,
											  &vtRecord.UpdateDate)
		if err != nil {
			return vtRecord, err
		}
	}

	return vtRecord, nil
}

func (c CacheChecker) ProcessHash(hash string, mode int) {

}

