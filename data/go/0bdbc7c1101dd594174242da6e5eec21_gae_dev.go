/*
 Development Tool for go app engine development, that presents an easier to use wrapper for 
 App Engine development with GO Runtime, bypassing some pitfalls caused by integration with
 the Python SDK.
 
 It will do the following:
   - Launch Api Server (Python SDK)
     - if nothing is listening on the Python API Unix Socket
   - Launch go app (just like python sdk does) and writes log files to a certain directory
     - if nothing is listening on the Go App Unix Socket
   - Create a proxy to do the following (by default):
     - requests matching / or /_ah/warmup, etc go to GO
     - requests matching /_ah/*, /form* go to Python
     - requests matching static files are served by this "helper" process
     - all others go to GO
   - Watches for changes to directories for my app 
     - If any .go file there changes, it will rebuild and restart the app
     - If any other file changes, it will just restart the app
   - Watches for some files and keeps them in sync.
     - If any source file changes, it will copy it over to the corresponding dest
   - Since API server is single-threaded, we need to ensure that: 
     - a proxy to GoApp does not happen within a call to ApiServer

 Benefits:
 
   - Python is no longer used as a front end.
     - Concurrent Requests supported (Python SDK runs one request at a time).
     - Much Faster requests (Python SDK checks each time for any changed files in dir tree)
   - One executable to run and manage everything.
     - Ctrl-C on the gogaedev will shutdown all child processes (python sdk, goapp, etc)
   - Logging: 
     - Log files are rolled over each time app is restarted, or api server is restarted.
     - Log files can be saved (separate from Python SDK or everything else)
     - All logs can be configured to go to stdout/stderr of gogaedev process
       so you can monitor the all logs from one place. Logs still go to log files as above.
   - Only API requests will exist in python.
     - Python SDK issues (slowness, single threaded, etc) will not affect Go App development.
   - app.yaml is used only by Python SDK for API's, and for production 
     - It is not used by the Go Instance during development.
       No need to worry about keeping things in sync between dev and prod.
       For example, some files skipped during prod should be available in dev (e.g. tests, etc)
 
 Setup of Python SDK:

 With this setup, only requirement from Python SDK:
   - The API Server should be setup to initialize its server socket at startup
     not at first GoApp CGI request. 
 
 To setup the API server accordingly, Two files have to be edited:
   - google/appengine/tools/dev_appserver_main.py
     to create and listen on the Go API Server Socket at startup 
   - google/appengine/tools/dev_appserver.py 
     to remove (comment out) call to execute_go_cgi
 
 Edit google/appengine/tools/dev_appserver_main.py 
 to create and listen on the Go API Server Socket at startup 
 (i.e. before call to http_server.serve_forever).
    #ugorji: add call to setup api hook port
    if appinfo.runtime == 'go':
      import threading, getpass, atexit, asyncore
      import google.appengine.ext.go as go
      from google.appengine.ext.remote_api import handler
      user_port = '%s_%s' % (getpass.getuser(), port)
      go.SOCKET_API = go.SOCKET_API % user_port
      go.SOCKET_HTTP = go.SOCKET_HTTP % user_port
      go.GAB_WORK_DIR = go.gab_work_dir() % user_port
      go.cleanup()
      atexit.register(go.cleanup)
      go.RAPI_HANDLER = handler.ApiCallHandler()
      ds = go.DelegateServer()
      def asynCoreLoop():
        while ds.connected or ds.accepting:
          asyncore.loop(map=ds._map, count=1)
      th = threading.Thread(target=asynCoreLoop)
      th.setDaemon(True)
      th.start()
 
 Edit google/appengine/tools/dev_appserver.py 
 to remove call to execute_go_cgi, by commenting out the whole code block. 
 This way, Python SDK does not try to proxy requests over to the Go App.
  # Ugorji: remove hook for _go_app
  # if handler_path == '_go_app':
  #   from google.appengine.ext.go import execute_go_cgi
  #   return execute_go_cgi(root_path, handler_path, cgi_path,
  #       env, infile, outfile)

 How To Use This Tool:
 
   - User creates a file go-gae-dev-cfg.json, and puts in their app directory
     A complete one looks something like this (below, we show what the defaults are):
     # Within regex, no spaces (I had to put spaces between * and / because it's in a comment)
       {
         "Verbose": false,
         "Succinct": true,
         "UseFSWatch": false,
         "IncludeChildProcLogs": false,
         "GaeSdkDir": "",
         "AppId": "app",
         "AppVersion": "1",
         "GoFilesToIgnore": "abc",
         "WatchPathsToIgnore": "(.* /)?(_go_\\.[0-9]|_obj|.*[#~].*)",
         "WatchDirNamesToSkip": "^(cmds|_obj|\\..+|_.*)$",
         "StaticPaths": "/web(/.*)?|(.*\\.(gif|png|jpg|jpeg|ico|css|js|json))",
         "InitialCheckGoPaths": "/$|(/_ah/(warmup)(/.*)?)",
         "ApiPaths": "/_ah/.*|/form.*",
         "ApiServerHttpURL": "http://localhost:8080",
         "GoServerHttpURL": "http://localhost:8088",
         "ManageGoApp": true,
         "ManageApiServer": true,
         "ProxyAddr": ":8888",
         "ApiParams": [ "--allow_skipped_files", "--skip_sdk_update_check" ],
         "LogDir": "/tmp/gogaedev_USERNAME_logs",
         "StaticFilesDir": ".", 
         "ManageGoAppIntervalSecs": 5,
         "FilesToSync": { },
         "DirsToWatch": [
           "."
         ]  
       }
     A lot of these entries are reasonable/adequate defaults and can be omitted. 
     At a minimum, configure these:
       {
         "GaeSdkDir": "/opt/go-appengine-1.6.0/google_appengine",
         "GoFilesToIgnore": ".* /((main|yaml)/.*|app_(dev|prod)\\.go)"
       }
     To sync some files, e.g. if app_dev.go changes, copy it over to app_for_env.go, use:
       {
         "FilesToSync": {
           "app/server/app_dev.go": "app/server/app_for_env.go"
         }
       }

   - User must add a http tcp listen address to their app. A quick way to do this is, 
     within an init() method in your app, add:
       if appengine.IsDevAppServer() { go http.ListenAndServe(":8888", nil) }
   - Within your app directory, run gogaedev:
       gogaedev
   - Access application as usual, but using the proxy (instead of going through Python SDK). 
     E.g.
     http://localhost:8888/_ah/admin
     http://localhost:8888/
*/
package main

/*
 Issues (non affecting us):
   - filepath.Walk does not follow symbolic links. 
     soln: we use a custom Walk function to allow this.
   - go-app-builder:
     - wants all paths to be relative to appdir 
       soln: it's fine, our custom Walk function allows this
     - builds all packages everytime, making build take long time
   - UNIX domain socket has errors under load
     *776 connect() to unix:/tmp/dev_appserver_ugorji_8080_socket_http failed 
     (11: Resource temporarily unavailable) while connecting to upstream, 
     client: 127.0.0.1, server: _, request: "GET / HTTP/1.1", 
     upstream: "http://unix:/tmp/dev_appserver_ugorji_8080_socket_http:/", 
     host: "localhost:8000"
     - TCP has better error handling
     - Thus, we need to put in a TCP Port in the server, 
       else we can't really stress out the app under load
       See https://groups.google.com/d/msg/golang-nuts/9zb9-Mo2uyA/0Vmz7Vqnz5EJ
   - regexp.MatchString requires ^ at start of string
   - FileSystem Watching currently only supported on Linux (inotify).
     By default, UseFsWatch = false, and we'd walk tree every time interval
     to determine what files changed.
     Note however, that a bug exists with os/inotify which may cause issues. To be safe,
     use the less efficient mode with UseFsWatch=false.
   - If UseFsWatch = true, diff goroutine may update restartApp, rebuildApp variables.
     We synchronize access using a channel.
   - time.Sleep() may use a new OS thread every time. To be safe, use time.After().
     TODO: Change back to time.Sleep after GO 1 is released.

 To make this open source:
   - Bundle the following files into one, resolving all paths as necessary:

 Is it neccessary (to do on the Python SDK to ensure only one socket connection)
   - The API Socket should use listen(1)
     This is because python sdk can only handle one request at a time.
     Concurrent requests (possible via listen(n) n>1) will break it.

*/

import (
	"os"
	"os/user"
	"io"
	"os/inotify"
	"time"
	"http"
	"net"
	"fmt"
	"log"
	"strings"
	"strconv"
	"path/filepath"
	"exec"
	"regexp"
	"json"
	"url"
	"runtime"
	"sync"
)

const (
	//TODO: set to false once Python SDK becomes multi threaded	
	apiServerSingleThreaded = true
)

var (
	noFSWatchExitEarly = os.NewError("noFSWatchExitEarly: rebuildApp = restartApp = true")
	fsWatchNotSupported = fmt.Errorf("Using FS Watch is not supported for: %v", runtime.GOOS)
)

//MainAsProc calls os.Open on the configFile, and uses that to call gaedev.Main().
//If it sees any error, it calls os.Exit(1).
//Else it waits forever on the ErrChan of the Runner, printing out any error it sees.
//Note that this function does not return (unless there was an error at initialization).
func MainAsProc(configFile string) {
	fn := func() (r *Runner, err os.Error) {
		log.SetFlags(log.LstdFlags|log.Lmicroseconds)
		f, err := os.Open(configFile)
		if err != nil { return }
		defer f.Close()
		if r, err = Main(f); err != nil { return }
		return
	}
	r, err := fn()
	if err != nil { 
		//panic(err)
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
		return
	}
	for {
		select {
		case err1 := <- r.ErrChan:
			log.Printf("Error: %v\n", err1)			
		}
	}
	//wait forever
	//waitForeverChan := make(chan bool)
	//<- waitForeverChan
}

//out calls log.Printf internally
func out(format string, args ...interface{}) {
	ln := len(format)
	if ln > 0 && format[ln-1] != '\n' { format = format + "\n" }
	log.Printf(format, args...)
}

//multiError allows us wrap a slice of errors as one
type multiError []os.Error

func (x multiError) String() string {
	return fmt.Sprintf("%v", []os.Error(x))
}

//Config holds the configuration. It can be loaded, or updated from a configuration file.
type Config struct { 
	//GaeSdkDir is the GO GAE SDK Home. Must be set in the config file
	GaeSdkDir string 
	//AppDir is the App Directory. Defaults to ".". This must be run from the app directory.
	AppDir string
	//FilesToSync is a mapping of files to sync. For example, if you have app_dev.go, and app_prod.go,
	//then in development environment, you want app_dev.go to be copied to app.go,
	//and want to exclude the building or app_dev.go and app_prod.go.
	FilesToSync map[string]string
	//DirsToWatch is a slice of Directories to watch. Defaults to ".". We follow sym links.
	DirsToWatch []string
	//GoFilesToIgnore is a Regular Expression string, 
	//showing Go Files to ignore (similar to no_build_files)
	GoFilesToIgnore string
	//WatchPathsToIgnore is a Regular Expression string, showing the paths in the Watch directories 
	//that must be ignored (e.g. .git, .svn, #*, etc)
	WatchPathsToIgnore string
	//WatchDirNamesToSkip is a Regular Expression string, showing the directory names 
	//that must be skipped during a Walk (e.g. _obj, cmds, .XYZ, _XYZ, etc)
	WatchDirNamesToSkip string
	//LogDir is the dir Where should log file for Go App, etc go? Defaults to a dir in tmp.
	LogDir string
	//StaticFilesDir is the base directory from which we serve static files.
	StaticFilesDir string
	
	//AppId is your app id. Set it if you need the appid in your code.
	AppId string
	//App Version is your app version. Set it if you need the app version in your code.
	AppVersion string
	
	//Verbose flag, if true, causes non-crucial notifications to be logged.
	//For no logging, set Verbose=false and Succint=false.
	Verbose bool
	//Succinct flag, if true, causes crucial notifications to be logged.
	//For no logging, set Verbose=false and Succint=false.
	Succinct bool
	
	//StaticPaths is a RegularExpression string, saying what paths should match as static files.
	StaticPaths string
	//InitialCheckGoPaths is a RegularExpression string, saying what paths should be 
	//sent to GO App before other checks.
	InitialCheckGoPaths string
	//ApiPaths is a RegularExpression string, saying what paths should be sent to Python SDK.
	ApiPaths string
	
	//ApiServerHttpURL is a URL that defines where we proxy API requests to
	ApiServerHttpURL string
	//GoServerHttpURL is a URL that defines where we proxy GO requests to.
	GoServerHttpURL  string 
	//ProxyAddr is the address that the reverse proxy listens on
	ProxyAddr string
	
	//ApiParams are the Parameters passed to dev_appserver.py. It does not include the directory
	//of the app.
	ApiParams []string
	
	//ManageGoAppIntervalSecs is how long to wait (in seconds) 
	//between calls to manageGoApp  (rebuildApp, restartApp, etc)
	ManageGoAppIntervalSecs int
	
	//ManageGoApp is a flag, saying whether we should manage Go App (start/stop/rebuildOnChange)
	ManageGoApp bool
	//ManageApiServer is a flag, saying whether we should manage Python SDK (start)
	ManageApiServer bool
	//UseFSWatch is a flag, saying whether we should use FS Watch (e.g. inotify). 
	//Currently, FS Watch supported on linux only (inotify). 
	UseFSWatch bool
	//IncludeChildProcLogs says whether child process logs should be included 
	//in this process stdout/stderr streams. Defaults to false.
	IncludeChildProcLogs bool
	
	//workDir is the directory used to store things for building Go App
	workDir string
	//sockPfx is the path prefix to the unix domain sockets (without _api or _http)
	sockPfx string
	
	// below are *regexp.Regexp corresponding to regexp strings defined above.
	
	goFilesToIgnoreRE *regexp.Regexp
	watchPathsToIgnoreRE *regexp.Regexp
	watchDirNamesToSkipRE *regexp.Regexp

	staticPathsRE *regexp.Regexp 
	initialCheckGoPathsRE *regexp.Regexp 
	apiPathsRE *regexp.Regexp 
	
}

//NewConfig Create a New Config with reasonable defaults. Look at the package
//documentation for the set of defaults.
func NewConfig() (c *Config, err os.Error) {
	c = new(Config)
	c.FilesToSync = make(map[string]string)
	c.DirsToWatch = []string{ "." }
	c.ApiParams = []string{ "--allow_skipped_files", "--skip_sdk_update_check" }
	c.ManageGoAppIntervalSecs = 5
	
	c.WatchPathsToIgnore = `(.*/)?(_go_\.[0-9]|_obj|.*[#~].*)`
	c.WatchDirNamesToSkip = `^(cmds|_obj|\..+|_.*)$`
	c.GoFilesToIgnore = `abc`
	c.AppId = "app"
	c.AppVersion = "1"
	
	c.ManageGoApp = true
	c.ManageApiServer = true
	c.IncludeChildProcLogs = false
	
	c.Succinct = true
	c.Verbose = false
	
	me, err := user.LookupId(os.Getuid())
	if err != nil { return }
	c.LogDir = filepath.Join(os.TempDir(), "gogaedev_" + me.Username + "_logs")
	//c.LogDir = c.workDir //can't share log dir with workdir (gab manages workDir)
	
	if c.AppDir, err = filepath.Abs("."); err != nil { return }
	c.StaticFilesDir = c.AppDir
	c.StaticPaths = `/web(/.*)?|(.*\.(gif|png|jpg|jpeg|ico|css|js|json))`
	c.InitialCheckGoPaths = `/$|/_ah/(warmup)(/.*)?`
	c.ApiPaths = `/_ah/.*|/form.*`
	c.ApiServerHttpURL = "http://localhost:8080"
	c.GoServerHttpURL  = "http://localhost:8088"
	c.ProxyAddr = ":8888"
	
	c.UseFSWatch = false
	//switch runtime.GOOS {
	//case "linux": c.UseFSWatch = true
	//}
	return 
}

//Runner encapsulates a single run of the tool.
type Runner struct {
	c *Config
	goAppProcess *os.Process
	apiProcess *os.Process
	apiProxy *http.ReverseProxy
	goAppProxy *http.ReverseProxy
	
	//client http.Client
	rebuildApp bool
	restartApp bool
	
	//channels used for updating the variables restartApp, rebuildApp by FSWatch GoRoutines.
	//We use a buffer of 1, since the channels are only used to send true in.
	//If one person has already sent true, then others don't have to.
	
	rebuildAppChan chan bool
	restartAppChan chan bool
	
	//if using fswatch, you need to signal to the ManageGoApp goroutine that it can start managing
	fsWatchReadyChan chan bool
	
	//ErrChan is used to send errors obtained within goroutines. So nothing is just lost.
	ErrChan chan os.Error
	
	//during a call to Api Server, do not allow any calls to GoApp
	apiServerMU sync.RWMutex
	
	//manageGoAppMU ensures that request is not sent to app while manageGoApp() is running
	manageGoAppMU sync.RWMutex
	manageGoAppLastRunStartNs int64
	manageGoAppLastRunEndNs int64
}

func NewRunner(c *Config) (r *Runner) {
	r = new(Runner)
	r.c = c
	r.rebuildApp = true
	r.restartApp = true
	r.rebuildAppChan = make(chan bool, 1)
	r.restartAppChan = make(chan bool, 1)
	r.fsWatchReadyChan = make(chan bool)
	r.ErrChan = make(chan os.Error, 8)
	return
}

//Main creates the Config, with updates from the reader parameter, 
//and then sets up the Proxy and starts goroutine(s) to manage the Go App.
func Main(f io.Reader) (r *Runner, err os.Error) {
	var c *Config
	if c, err = NewConfig(); err != nil { return }
	if err = json.NewDecoder(f).Decode(c); err != nil { return }
	if err = c.Validate(); err != nil { return }
	
	r = NewRunner(c)

	if err = r.DoProxy(); err != nil { return }
	
	httpReqFn := func(hw http.ResponseWriter, hr *http.Request) {
		r.DoRequest(hw, hr)
	}
	http.HandleFunc("/", httpReqFn)
		
	if c.ManageGoApp { 
		if c.UseFSWatch {
			switch runtime.GOOS {
			case "linux":
				go r.DoLinuxWatch()
			default:
				err = fsWatchNotSupported
				return 
			}
		} 
		go r.DoManageGoApp()
	}
	go http.ListenAndServe(c.ProxyAddr, nil)
	return
}

//DoProxy Sets up Proxies for goApp and apiServer
func (r *Runner) DoProxy() (err os.Error) {
	c := r.c
	var uri *url.URL
	
	if uri, err = url.Parse(c.GoServerHttpURL); err != nil { return }
	r.goAppProxy = http.NewSingleHostReverseProxy(uri)
	d2 := r.goAppProxy.Director
	r.goAppProxy.Director = func(req *http.Request) {
		d2(req)
		req.Header.Set("X-AppEngine-Inbound-AppId", "dev~" + c.AppId)
		req.Header.Set("X-AppEngine-Inbound-Version-Id", c.AppVersion)
	}
	
	if uri, err = url.Parse(c.ApiServerHttpURL); err != nil { return }
	r.apiProxy = http.NewSingleHostReverseProxy(uri)
	
	return
}

//DoRequest Handles a request, using the RawURL to determine where to route requests to
func (r *Runner) DoRequest(w http.ResponseWriter, req *http.Request) {
	c := r.c
	doGoAppReqFn := func() {
		r.manageGoAppMU.RLock()
		defer r.manageGoAppMU.RUnlock()
		if apiServerSingleThreaded {
			r.apiServerMU.RLock()
			defer r.apiServerMU.RUnlock()
		}
		r.goAppProxy.ServeHTTP(w, req)
	}
	doApiReqFn := func() {
		if apiServerSingleThreaded {
			r.apiServerMU.Lock()
			defer r.apiServerMU.Unlock()
		}
		r.apiProxy.ServeHTTP(w, req)
	}
	//println("################################: ", r.RawURL)
	switch {
	case c.initialCheckGoPathsRE.MatchString(req.RawURL):
		r.vout("Matching GO Initial Check (to GO APP): %v", req.RawURL) 
		doGoAppReqFn()
	case c.apiPathsRE.MatchString(req.RawURL):
		r.vout("Matching API (to Python API Server): %v", req.RawURL) 
		doApiReqFn()
	case c.staticPathsRE.MatchString(req.RawURL):
		r.vout("Matching Static (serving file): %v", req.RawURL) 
		http.ServeFile(w, req, filepath.Join(c.StaticFilesDir, req.RawURL))
	default:
		r.vout("Matching Default (to GO APP): %v", req.RawURL) 
		doGoAppReqFn()
	}
}

//DoManageGoApp runs in a loop, calling manageGoApp every interval.
//If a call to manageGoApp returns an error, send it into the ErrChan.
func (r *Runner) DoManageGoApp() {
	c := r.c
	sleepNs := 1e9 * int64(c.ManageGoAppIntervalSecs)
	//ticker := time.NewTicker(sleepNs)
	if c.UseFSWatch { 
		select {
		case _ = <- r.fsWatchReadyChan: 
		}
	}
	for {
		if err := r.manageGoApp(); err != nil {
			select {
			case r.ErrChan <- err:
			}
		}
		<- time.After(sleepNs)
	}
}

//DoLinuxWatch Handles watching directories, files, etc in linux, and acting appropriately
//ie sending appropriate restartApp, rebuidApp flags down the appropriate channel for 
//manageGoApp to use.
func (r *Runner) DoLinuxWatch() {
	c := r.c
	var err os.Error
	fnErr := func() {
		if err != nil {
			select {
			case r.ErrChan <- err:
			}
			err = nil
		}
	}
	defer fnErr()
	watcher, err := inotify.NewWatcher()
	if err != nil { return }
	watcherd, err := inotify.NewWatcher()
	if err != nil { return }
	for k, _ := range c.FilesToSync {
		err = watcher.AddWatch(k, inotify.IN_CLOSE_WRITE)
		if err != nil { return }
	}
	dwErrors := make([]os.Error, 0, 4)
	for _, k := range c.DirsToWatch {
		Walk(k, true, true, r.makeLinuxWalkFunc(watcherd, &dwErrors))
		if len(dwErrors) > 0 { err = multiError(dwErrors); return }
	}
	select {
	case r.fsWatchReadyChan <- true:
	}
	//manageGoApp() 
	for {
		err = r.doLinuxWatch(watcher, watcherd)
		fnErr()
	}
}

func (r *Runner) doLinuxWatch(watcher *inotify.Watcher, watcherd *inotify.Watcher) (err os.Error) {
	c := r.c
	//L:
	select {
	case ev := <-watcherd.Event:
		//r.vout("Dir Event: %v, Mask: %v\n", ev, ev.Mask) 		
		if c.watchPathsToIgnoreRE.MatchString(ev.Name) { return }
		r.vout("Dir Event: %v, Mask: %v\n", ev, ev.Mask) 			
		if ev.Mask & 
			(inotify.IN_DELETE|inotify.IN_ISDIR) == (inotify.IN_DELETE|inotify.IN_ISDIR) {
			r.vout("Directory deleted (meaning watch ends): %v", ev.Name) 
			if err = watcherd.RemoveWatch(ev.Name); err != nil { return }
			//no need to remove watch for non-existent file. It crashes program.
		} else if ev.Mask & 
			(inotify.IN_CREATE|inotify.IN_ISDIR) == (inotify.IN_CREATE|inotify.IN_ISDIR) {
			dwErrors := make([]os.Error, 0, 4)
			Walk(ev.Name, true, true, r.makeLinuxWalkFunc(watcherd, &dwErrors))
			if len(dwErrors) > 0 { err = multiError(dwErrors); return }
		}
		//r.vout("DoLinuxWatch: Try to do selects")
		select {
		case r.restartAppChan <- true:
			r.vout("DoLinuxWatch: Sending true into restartAppChan")
		default:
		}
		//restartApp = true
		if r.isAppGoFile(ev.Name) {
			select {
			case r.rebuildAppChan <- true:
				r.vout("DoLinuxWatch: Sending true into rebuildAppChan")
			default:
			}			
			//rebuildApp = true
		}
	case ev := <-watcher.Event:
		r.vout("DoLinuxWatch: Sync Event: %v, Mask: %v| To: %v\n", 
			ev, ev.Mask, c.FilesToSync[ev.Name])
		//break L
		if v, ok := c.FilesToSync[ev.Name]; ok {
			if err = r.cp(ev.Name, v); err != nil { return }
			//bb, err := ioutil.ReadFile(ev.Name)
			//if err != nil { return }
			//err = ioutil.WriteFile(v, bb, 0777)
			//if err != nil { return }
		}
	case err = <-watcher.Error:
		out("DoLinuxWatch: Sync Error: %v\n", err)
	case err = <-watcherd.Error:
		out("DoLinuxWatch: Dir Error: %v\n", err)
		//case _ = <-time.After(1e9 * int64(c.ManageGoAppIntervalSecs)):
	}
	return nil
}

//Validate is called after constructing your config. It does some initialization of 
//helper variables (regex'es), does some checks (e.g. is AppDir a real app dir),
//and sets some variables to reasonable defaults based on others.
//This was made an exported function so that it is called explicitly to ensure a clean Config.
func (c *Config) Validate() (err os.Error) {
	if c.Verbose && !c.Succinct { c.Succinct = true }
	if c.UseFSWatch {
		switch runtime.GOOS {
		case "linux":
		default: 
			err = fsWatchNotSupported
			return
		}
	}
	if c.GaeSdkDir == "" { err = os.NewError("No GAE SDK specified"); return }
	if _, err = os.Stat(filepath.Join(c.AppDir, "app.yaml")); err != nil { return }
	
	if c.goFilesToIgnoreRE, err = regexp.Compile(c.GoFilesToIgnore); err != nil { return }
	if c.watchPathsToIgnoreRE, err = regexp.Compile(c.WatchPathsToIgnore); err != nil { return }
	if c.watchDirNamesToSkipRE, err = regexp.Compile(c.WatchDirNamesToSkip); err != nil { return }
	if c.staticPathsRE, err = regexp.Compile(c.StaticPaths); err != nil { return }
	if c.initialCheckGoPathsRE, err = regexp.Compile(c.InitialCheckGoPaths); err != nil { return }
	if c.apiPathsRE, err = regexp.Compile(c.ApiPaths); err != nil { return }

	apiurl, err := url.Parse(c.ApiServerHttpURL)
	if err != nil { return }
	apiPort := "80"
	if colonIdx := strings.LastIndex(apiurl.Host, ":"); colonIdx != -1 { 
		apiPort = apiurl.Host[colonIdx+1:] 
	}
	me, err := user.LookupId(os.Getuid())
	if err != nil { return }
	c.sockPfx = filepath.Join(os.TempDir(), 
		"dev_appserver_" + me.Username + "_" + apiPort + "_socket_")
	//c.workDir = filepath.Join(me.HomeDir, ".cache", 
	c.workDir = filepath.Join(os.TempDir(), 
		"dev_appserver_" + me.Username + "_" + apiPort + "_go_app_work_dir")
	
	if err = os.MkdirAll(c.workDir, 0777); err != nil { return }
	if err = os.MkdirAll(c.LogDir, 0777); err != nil { return }
	
	if c.Verbose { out("Config: %#+v\n", c) }
	return	
}

//manageGoApp is main function that runs to manage the Go App. 
//It will check if a restart/rebuild is necessary, and if so, will proceed to do so.
func (r *Runner) manageGoApp() (err os.Error) {
	c := r.c
	if !c.ManageGoApp { return }
	currTimeNs := time.Nanoseconds()
	if r.manageGoAppLastRunEndNs > 0 && 
		(currTimeNs - r.manageGoAppLastRunEndNs) < 1e9 * int64(c.ManageGoAppIntervalSecs) { return }
	r.manageGoAppMU.Lock()
	defer func() { 
		r.manageGoAppLastRunStartNs = currTimeNs
		r.manageGoAppLastRunEndNs = time.Nanoseconds()
		//if err != nil { r.vout("manageGoApp err: %v", err) }
		r.vout("manageGoApp: TimeTaken: %v \u00B5s (microseconds)", 
			(r.manageGoAppLastRunEndNs - r.manageGoAppLastRunStartNs) / 1e3)
		r.manageGoAppMU.Unlock()
		//r.vout("manageGoApp finish: manageGoAppLastRunStartNs: %v, manageGoAppLastRunEndNs: %v" + 
		//	" rebuildApp: %v, restartApp: %v", 
		//	manageGoAppLastRunStartNs, manageGoAppLastRunEndNs, rebuildApp, restartApp)
	}()
	if c.UseFSWatch {
		select {
		case r.restartApp = <- r.restartAppChan:
			r.vout("manageGoApp: Retrieving %v from restartAppChan", r.restartApp)
		default: 
		}
		select {
		case r.rebuildApp = <- r.rebuildAppChan:
			r.vout("manageGoApp: Retrieving %v from rebuildAppChan", r.rebuildApp)
		default: 
		}
	} else {
		for _, d := range c.DirsToWatch {
			Walk(d, true, true, r.makeNoFSWatchWalkFn())
		}
	}
	r.vout("mgApp(): rebuildApp: %v, restartApp: %v\n", r.rebuildApp, r.restartApp) 
	//if python sdk is down, start it up
	if apiConn, err := net.Dial("unix", c.sockPfx + "api"); err != nil {
		r.vout("Check Api Server Up. Error: %v\n", err) 
		if err = r.startApi(); err != nil { return }
	} else {
		apiConn.Close()
	}
	//only reset flags iff action taken was successful, in case we need to try again
	if r.rebuildApp {
		r.restartApp = true
		err = r.buildApp()
		if err == nil {
			r.rebuildApp = false
		} else {
			r.restartApp = false
			r.sout("Building Error: %v\n", err) 
		}
	}
	if r.restartApp {
		err = r.startApp()
		if err == nil {
			r.restartApp = false
		} else {
			r.sout("Starting Error: %v\n", err) 
		}
	}
	return
}

func (r *Runner) buildApp() (err os.Error) {
	c := r.c
	if err = r.killApp(); err != nil { return }
	r.sout("Building App\n") 
	goFiles := make([]string, 0, 4)
	for _, d := range c.DirsToWatch {
		Walk(d, true, true, r.makeFindGoWalkFunc(&goFiles))
	}
	goArch := os.Getenv("GOARCH")
	if len(goArch) == 0 { goArch = runtime.GOARCH }
	gabArch := "6"
	switch goArch {
	case "386": gabArch = "8"
	case "arm": gabArch = "5"
	}
	args := make([]string, 0, 20 + len(goFiles))
	args = append(args, 
		"-app_base", c.AppDir,
		"-arch", gabArch,
		"-binary_name", "_go_app",
		"-dynamic",
		"-goroot", filepath.Join(c.GaeSdkDir, "goroot"),
		"-unsafe",
		"-work_dir", c.workDir)
	args = append(args, goFiles...)
	gabcmd := filepath.Join(c.GaeSdkDir, "goroot", "bin", "go-app-builder")
	cmd := exec.Command(gabcmd, args...)
	cmd.Dir = c.AppDir
	r.vout("Cmd: %v, Args: %v\n", gabcmd, args) 
	//if err := c.Start(); err != nil { return (err) }
	out, err := cmd.CombinedOutput()
	if err != nil { return fmt.Errorf("Err: %v, Output: %v", err, string(out)) }
	r.vout("Pid: %v\n", cmd.Process.Pid) 
	return
}

func (r *Runner) killApp() (err os.Error) {
	c := r.c
	r.vout("Attempting to Kill App (if it exists)\n") 
	if r.goAppProcess != nil {
		r.sout("App Exists. Killing App\n") 
		if err = r.goAppProcess.Kill(); err != nil { return }
		r.goAppProcess = nil
	}
	//for some reason, things barf if we don't create this directory after each kill
	if err = os.MkdirAll(c.workDir, 0777); err != nil { return }
	return
}

func (r *Runner) startApp() (err os.Error) {
	c := r.c
	if err = r.killApp(); err != nil { return }
	cmd, err := r.startServerProc("Go-App", "goapp.log", 
		"unix", c.sockPfx + "http", 1e8,
		filepath.Join(c.workDir, "_go_app"),
		"-addr_http", "unix:" + c.sockPfx + "http",
		"-addr_api", "unix:" + c.sockPfx + "api")
	if err != nil { return }
	r.goAppProcess = cmd.Process
	return
}

func (r *Runner) startApi() (err os.Error) {
	c := r.c
	if !c.ManageApiServer { return }
	params := []string{ filepath.Join(c.GaeSdkDir, "dev_appserver.py") }
	params = append(params, c.ApiParams...)
	params = append(params, ".")
	cmd, err := r.startServerProc("Api-Server", "apiserver.log", 
		"unix", c.sockPfx + "api", 2e8, 
		"python", params...)
	if err != nil { return }
	r.apiProcess = cmd.Process
	return
}

func (r *Runner) startServerProc(name string, logfile string, netw string, addr string,
	sleepNs int64, cmdName string, cmdParams ...string) (cmd *exec.Cmd, err os.Error) {
	c := r.c
	defer func() { 
		if err != nil { r.vout("Error starting %v: %v\n", name, err) }
	}()
	r.sout("Starting %v\n", name) 
	r.vout("Cmd: %v, Params: %v\n", cmdName, cmdParams) 
	cmd = exec.Command(cmdName, cmdParams...)
	cmd.Dir = c.AppDir
	var flog io.Writer
	logPath := filepath.Join(c.LogDir, logfile)
	//setup different file for the output, backing up if necessary
	if _, err = os.Stat(logPath); err == nil {
		logBak := filepath.Join(c.LogDir, logfile + "." + strconv.Itoa64(time.Seconds()) + ".log")
		if err = r.cp(logPath, logBak); err != nil { return }
	}
	if flog, err = os.Create(logPath); err != nil { return }
	flogout, flogerr := flog, flog
	if c.IncludeChildProcLogs { 
		flogout, flogerr = io.MultiWriter(flog, os.Stdout), io.MultiWriter(flog, os.Stderr)
	}
	cmd.Stdout, cmd.Stderr = flogout, flogerr
	if err = cmd.Start(); err != nil { return }
	
	var conn net.Conn
	//try to make connection to it (give it 4 seconds)
	for i := 0; i < 40; i++ {
		<- time.After(sleepNs) //0.1 seconds
		r.vout("Trying to dial: %v, %v\n", netw, addr) 
		if conn, err = net.Dial(netw, addr); err == nil { conn.Close(); break }
	}
	if err != nil { 
		r.vout("Unable to connect to server after 4 seconds. Killing Process.\n") 
		cmd.Process.Kill()
		return 
	}	
	r.sout("%v Started\n", name) 
	
	return
}

func (r *Runner) isAppGoFile(path string) bool {
	c := r.c
	x := false
	if strings.HasSuffix(path, ".go") && !c.goFilesToIgnoreRE.MatchString(path) {
		x = true
	}
	r.vout("isAppGoFile: Path: %v, :%v\n", path, x) 
	return x
}

//vout logs the message if c.Verbose = true
func (r *Runner) vout(format string, args ...interface{}) {
	c := r.c
	if c.Verbose { out(format, args...) }
}

//sout logs the message if c.Succinct = true
func (r *Runner) sout(format string, args ...interface{}) {
	c := r.c
	if c.Succinct { out(format, args...) }
}

//cp copies from a src path to a dest path.
func (r *Runner) cp(src string, dest string) (err os.Error) {
	//fsrc := os.OpenFile(src, flag int, perm uint32) (file *File, err Error)
	r.vout("Copy: Src: %v, Dest: %v", src, dest)
	var fsrc, fdest *os.File
	if fsrc, err = os.Open(src); err != nil { return }
	if fdest, err = os.Create(dest); err != nil { return }
	if _, err = io.Copy(fdest, fsrc); err != nil { return }
	if err = fsrc.Close(); err != nil { return }	
	if err = fdest.Close(); err != nil { return }	
	return
}

//noFSWatchWalkFn is called only by manageGoApp(). It is the function called to 
//check if a restart/rebuild are necessary, and also to ensure that FilesToSync
//is checked.
func (r *Runner) makeNoFSWatchWalkFn() WalkFunc {
	c := r.c
	fn := func(path string, resolvedPath string, f *os.FileInfo, errx os.Error) os.Error {
		if errx != nil { return errx }
		noFilesToSync := len(c.FilesToSync) == 0
		if noFilesToSync && r.rebuildApp && r.restartApp { return noFSWatchExitEarly }
		if f.IsDirectory() {
			if c.watchDirNamesToSkipRE.MatchString(f.Name) { return WalkSkipDir }
			return nil 
		} else if f.IsRegular() {
			//r.vout("noFSWatchWalkFn: Path: %v", path)
			fnx := func(newPath string, newFileInfo *os.FileInfo) {
				if r.manageGoAppLastRunStartNs > 0 {
					if newFileInfo.Mtime_ns < r.manageGoAppLastRunStartNs { return }
					r.vout("noFSWatchWalkFn: File changed: Path: %v, Mtime_ns: %v, manageGoLastRun: %v", 
						newPath, newFileInfo.Mtime_ns, r.manageGoAppLastRunStartNs)
				}
				r.restartApp = true
				if r.isAppGoFile(newPath) { r.rebuildApp = true } 
			}
			//check if any file changed
			fnx(path, f)
			//check if path matches one of the filesToSync
			if fdest, ok := c.FilesToSync[path]; ok { 
				fdstat, errd := os.Stat(fdest)
				if errd != nil || f.Mtime_ns > fdstat.Mtime_ns {
					r.cp(path, fdest)
					fdstat, errd = os.Stat(fdest)
				}
				fnx(fdest, fdstat)
			}
		}
		return nil
	}
	return fn
}

//makeLinuxWalkFunc returns a WalkFunc to add inotify watches, and collect errors
func (r *Runner) makeLinuxWalkFunc(watcherd *inotify.Watcher, errors *[]os.Error) WalkFunc {
	c := r.c
	fn := func(path string, resolvedPath string, f *os.FileInfo, errx os.Error) os.Error {
		//r.vout("- VisitDir: path: %v: %v: %v\n", path, resolvedPath, f.Name) 
		if errx != nil { return errx }
		if !f.IsDirectory() { return nil }
		if c.watchDirNamesToSkipRE.MatchString(f.Name) { return WalkSkipDir }
		//if !supportedDirectory(path) { return WalkSkipDir }
		r.vout("VisitDir: path: %v: %v\n", path, resolvedPath) 
		//println("ZZZ: ", path)
		flags := inotify.IN_CLOSE_WRITE|inotify.IN_DELETE|inotify.IN_CREATE
		flags = flags|inotify.IN_MOVED_FROM|inotify.IN_MOVED_TO //|inotify.IN_EXCL_UNLINK
		err := watcherd.AddWatch(resolvedPath, flags)
		if err != nil { r.vout("AddWatch Error: %v", err) }
		if err != nil { *errors = append(*errors, err) }
		return nil
	}
	return fn
}

//makeFindGoWalkFunc returns a WalkFunc to find all go files for the app.
func (r *Runner) makeFindGoWalkFunc(goFiles *[]string) WalkFunc {
	c := r.c
	fn := func(path string, resolvedPath string, f *os.FileInfo, errx os.Error) os.Error {
		if errx != nil { return errx }
		if f.IsDirectory() { 
			if c.watchDirNamesToSkipRE.MatchString(f.Name) { return WalkSkipDir }
			//if !supportedDirectory(path) { return WalkSkipDir }
			return nil 
		} else if f.IsRegular() {
			if r.isAppGoFile(path) {
				if strings.HasPrefix(path, c.AppDir) {
					path = path[len(c.AppDir) + 1:]
				} 
				*goFiles = append(*goFiles, path)
			}
		}
		return nil
	}
	return fn
}

