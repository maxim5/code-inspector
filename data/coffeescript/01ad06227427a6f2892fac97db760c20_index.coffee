class KiteHelper extends KDController

  mvIsStarting: false
  
  getReady:->

    new Promise (resolve, reject) =>

      {JVM} = KD.remote.api
      JVM.fetchVmsByContext (err, vms)=>

        console.warn err  if err
        return unless vms

        @_vms = vms
        @_kites = {}

        kiteController = KD.getSingleton 'kiteController'

        for vm in vms
          alias = vm.hostnameAlias
          @_kites[alias] = kiteController
            .getKite "os-#{ vm.region }", alias, 'os'
        
        resolve()

  getKite:->

    new Promise (resolve, reject)=>

      @getReady().then =>
        vm = @_vms.first.hostnameAlias
        {vmController} = KD.singletons

        unless kite = @_kites[vm]
          return reject
            message: "No such kite for #{vm}"
        
        vmController.info vm, (err, vmn, info)=>
          if !@mvIsStarting and info.state is "STOPPED"
            @mvIsStarting = true
            timeout = 10 * 60 * 1000
            kite.options.timeout = timeout
            
            kite.vmOn().then ->
              resolve kite
            .timeout(timeout)
            .catch (err)->
              reject err
          else
            resolve kite

class LogWatcher extends FSWatcher

  fileAdded: (change)->
    {name} = change.file
    [percentage, status] = name.split '-' 
    
    @emit "UpdateProgress", percentage, status
    
class TerminalView extends KDView

  constructor: (options = {}, data) ->
    super options, data     
    
    @addSubView @terminal = new TerminalPane

class FinderView extends KDView

  constructor: (options = {}, data) ->
    super options, data

    vmController = KD.getSingleton "vmController"
    vmController.fetchDefaultVm (err, vm) =>
      
      vm.path = "/home/#{KD.nick()}/PhoneGap"
      
      warn err  if err
      
      @finderController = new NFinderController {
        hideDotFiles : yes
      }
      
      # Temporary fix, until its fixed in upstream ~ GG
      @finderController.isNodesHiddenFor = -> yes
      
      @addSubView @finderController.getView()
      @finderController.mountVm vm
      
      @finderController.on "FileNeedsToBeOpened", @bound 'openFile'

  openFile: (file) ->
    file.fetchContents (err, contents) =>
      unless err
        
        panel = @getDelegate()
        {JSEditor} = panel.panesByName
        
        JSEditor.openFile file, contents
        @emit "switchMode", 'develop'
        
      else
        
        new KDNotificationView
          type     : "mini"
          cssClass : "error"
          title    : "Sorry, couldn't fetch file content, please try again..."
          duration : 3000

  loadFile: (path)->
    file = FSHelper.createFileFromPath path
    kite = KD.getSingleton('vmController').getKiteByVmName file.vmName
    return  callback {message: "VM not found"}  unless kite

    @openFile file    

class EditorView extends KDView

  constructor:(options = {}, data)->
    options.cssClass = "editor-pane"
    super options, data

    @storage = KD.singletons.localStorageController.storage "Phonegap"
    @createEditorInstance()

  createEditorInstance:->
    path      = "localfile:/index.html"
    file      = FSHelper.createFileFromPath path
    {content} = @getOptions()

    @ace      = new Ace
      delegate        : this
      enableShortcuts : no
    , file

    @ace.once "ace.ready", =>
      if content then @ace.editor.setValue content
      @prepareEditor()

  closeFile:->
    @openFile FSHelper.createFileFromPath 'localfile:/index.html'

  loadFile:(path, callback = noop)->
    file = FSHelper.createFileFromPath path
    kite = KD.getSingleton('vmController').getKiteByVmName file.vmName
    return  callback {message: "VM not found"}  unless kite

    file.fetchContents (err, content)=>
      return callback err  if err

      file.path = path
      @openFile file, content

      KD.utils.defer -> callback null, {file, content}

  loadLastOpenFile:->
    path = @storage.getAt @_lastFileKey
    return  unless path

    @loadFile path, (err)=>
      if err?
      then @storage.unsetKey @_lastFileKey
      else @emit "RecentFileLoaded"

  openFile: (file, content)->
    validPath = file instanceof FSFile and not /^localfile\:/.test file.path

    if validPath
    then @storage.setValue @_lastFileKey, file.path
    else @storage.unsetKey @_lastFileKey
  
    @ace.editor.setValue content, -1
    @ace.setSyntax()
    @setData file

  viewAppended:->
    
    @findAndReplaceView = new AceFindAndReplaceView delegate: @
    @findAndReplaceView.hide()
    
    @addSubView @ace
    @addSubView @findAndReplaceView

  getValue: ->
    @ace.editor.getSession().getValue()
  
  requestSave:->
    
    file    = @getData()
    return  unless file
    content = @getValue()
    
    file.save content, (err)-> warn err  if err
    
  requestSaveAll:->
    log "save all"

  prepareEditor:->
    @ace.addKeyCombo "save",       "Ctrl-S",       @bound "requestSave"
    @ace.addKeyCombo "saveAll",    "Ctrl-Shift-S", @bound "requestSaveAll"
    @ace.addKeyCombo "find",       "Ctrl-F",       @ace.lazyBound "showFindReplaceView", no
    @ace.addKeyCombo "replace",    "Ctrl-Shift-F", @ace.lazyBound "showFindReplaceView", yes
    @ace.addKeyCombo "preview",    "Ctrl-Shift-P", => @getDelegate().preview()
    @ace.addKeyCombo "fullscreen", "Ctrl-Enter",   => @getDelegate().toggleFullscreen()
    @ace.addKeyCombo "gotoLine",   "Ctrl-G",       @ace.bound "showGotoLine"
    @ace.addKeyCombo "settings",   "Ctrl-,",       noop # ace creates a settings view for this shortcut, overriding it.
  
    @on "PaneResized", _.debounce(=> @ace.editor.resize()) , 400


class PhonegapMainView extends KDView
  
  user            = KD.nick()
  domain          = "#{user}.kd.io"
  outPath         = "/tmp/_Phonegapinstaller.out"
  phoneGapBin     = "/usr/bin/phonegap"
  installerScript = "https://rest.kd.io/bvallelunga/Phonegap.kdapp/master/installer.sh"
  gitResources    = "https://rest.kd.io/bvallelunga/Phonegap.kdapp/master/resources"
  iosApp          = "https://itunes.apple.com/app/id843536693"
  androidApp      = "https://play.google.com/store/apps/details?id=com.adobe.phonegap.app"
  readMore        = "http://phonegap.com/blog/2014/04/23/phonegap-developer-app/"

  constructor:(options = {}, data)->
    options.cssClass = 'phonegap main-view'
    super options, data

  viewAppended:->
    @addSubView @loadingContainer = new KDCustomHTMLView
      tagName    : "div"
      cssClass   : "loading-container"
    
    @loadingContainer.addSubView new KDCustomHTMLView
      tagName    : "img"
      attributes : 
          src    : "#{gitResources}/loading.gif"
    
    @loadingContainer.addSubView @loadingText = new KDCustomHTMLView
      tagName    : "div"
      partial    : "Please wait while your vm turns on..."
      
    @loadingContainer.addSubView @loadingButtons = new KDCustomHTMLView
      tagName    : "div"
      cssClass   : "buttons hidden"
    
    @loadingButtons.addSubView @loadingButton = new KDButtonView
      title         : "Kill The Service And Continue"
      cssClass      : 'main-button solid green'
      loader        :
        color       : "#FFFFFF"
        diameter    : 12
      callback      : @bound "killExistingService"
        
    @loadingButtons.addSubView @exitButton = new KDButtonView
      title         : "Exit App"
      cssClass      : 'main-button solid'
      loader        :
        color       : "#FFFFFF"
        diameter    : 12
      callback      : =>
        KD.singletons.router.handleRoute "/Apps"
        
    @loadingContainer.addSubView @errorButtons = new KDCustomHTMLView
      tagName    : "div"
      cssClass   : "buttons hidden"
    
    @errorButtons.addSubView @loadingButton = new KDButtonView
      title         : "Reload Page"
      cssClass      : 'main-button solid green'
      loader        :
        color       : "#FFFFFF"
        diameter    : 12
      callback      : ->
        window.location.reload()
        
    @errorButtons.addSubView @exitButton = new KDButtonView
      title         : "Exit App"
      cssClass      : 'main-button solid'
      loader        :
        color       : "#FFFFFF"
        diameter    : 12
      callback      : ->
        KD.singletons.router.handleRoute "/Apps"

    
    @kiteHelper = new KiteHelper
    @kiteHelper.getKite().then (kite)=>
      vmc = KD.getSingleton 'vmController'
      vmc.run "echo -n $(lsof -i:3000 -t)", (error, res)=>
        if res.stdout
          vmc.run """
            ps aux | grep '#{res.stdout}' | head -1 | awk '{print $12} {print $15}'
          """ , (error, res)=>
            outputSplit = res.stdout.split "\n"
          
            if outputSplit[0] is "/usr/bin/phonegap"
              
              #if no port or port 3000
              if not outputSplit[2] or "3000" in outputSplit
                @killExistingService()
              else 
                @loadingText.updatePartial "Another service is listening to port 3000"
                @loadingButtons.show()

            else 
              @loadingText.updatePartial "Another service is listening to port 3000"
              @loadingButtons.show()
        else 
          @appendViews()
    
    .catch (err)=>
      @loadingText.updatePartial err.message
      @errorButtons.show()
    
  killExistingService:->
    vmc = KD.getSingleton 'vmController'
    vmc.run "kill -9 $(lsof -i:3000 -t) 2> /dev/null;", @appendViews
  
  appendViews:->
    KD.singletons.appManager.require 'Teamwork', =>
      #Work Container
      @addSubView @workContainer = new KDCustomHTMLView
        tagName    : "div"
        cssClass   : "work-container"
      
      @workContainer.addSubView @workDownload = new KDCustomHTMLView
        tagName    : "div"
        cssClass   : "download-view"
       
      @workDownload.addSubView new KDCustomHTMLView
        tagName       : 'img'
        cssClass      : 'logo'
        attributes    :
          src         : "#{gitResources}/app.png"
          
      @workDownload.addSubView new KDCustomHTMLView
        tagName    : "div"
        cssClass   : "helper"
        partial  : """ 
          <p>The PhoneGap Developer app aims to lower the barrier of entry to creating PhoneGap applications. You can now immediately preview your app on a device without installing platform SDKs, registering devices, or even compiling code.<a href=“#{readMore}”> Read more…</a></p>
          <p>
            <strong>1. Install the PhoneGap Developer App</strong><br>
            Now grab the mobile app, which is globally available in an app store near you:
            <br><br>
            <div class="links">
              <ul>
                <li><a href="#{iosApp}">iOS from the App Store</a></li>
                <li><a href="#{androidApp}">Android from Google Play</a></li>
              </ul>
            </div>
          </p>
          <p>
            <strong>2. Pair the CLI and Developer App</strong><br>
            This is where the magic happens. The CLI starts a tiny web server to serve your project. Then, the PhoneGap Developer App connects to that server.
            <br><br>
            First, use the CLI to serve your project:
            <img src="#{gitResources}/phonegap-pairing.png"/>
            <br>
            Second, enter the server address into the PhoneGap Developer App. Please ignore the ip address given by phonegap serve. <strong>Only use <span class="link">#{user}.kd.io:3000</span></strong>
          </p>
          <p>
            <strong>3. Get to Work</strong><br>
            Once paired, it’s business as usual. You can freely add, edit, and remove files from your project. Every saved change will automatically update the preview displayed in the PhoneGap Developer App.
            <img src="#{gitResources}/phonegap-success.png"/>
          </p>
          <div class="separator">
            Frequently Asked Questions
          </div>
          <p>
            <strong>How come I don't see my terminal or the formatting is off?</strong><br>
            This app has a resizing bug that is being looked into and will be fixed soon. 
            <br><br>
            To fix formatting resize your browser window. Reload the page if the terminal or editor does not appear.
          </p>
          <br><br>
          <p>
            <strong>How do I create or open a phonegap app?</strong><br>
            The PhoneGap Developer app is compatible with existing PhoneGap and Apache Cordova projects.
            <br><br>
            You can create a new app:
            <div class="code">
              $ phonegap create my-app
              <br>
              $ cd my-app/
            </div>
            <br>
            Or open an existing app:
            <div class="code">
              $ cd ~/PhoneGap/my-existing-app
            </div>
          </p>
          <br><br>
          <p>
            <strong>How do I create a phonegap server for testing?</strong><br>
            Starting a phonegap server enables developers to test on multiple simultaneously.
            <br><br>
            Serve phonegap on port <strong>3000</strong>:
            <div class="code">
              $ cd my-app/
              <br>
              $ phonegap serve
            </div>
            <br>
            Or serve to a specific port
            <div class="code">
              $ cd my-app/
              <br>
              $ phonegap serve --port &lt;port&gt;
            </div>
          </p>
        """
     
      @workContainer.addSubView @workEditor = new Workspace
        title      : "Text Editor"
        name       : "TextEditor"
        cssClass   : "editor-view"
        panels     : [
          title               : "Text Editor"
          layout              :
            direction         : "vertical"
            sizes             : ["180px", null]
            splitName         : "BaseSplit"
            views             : [
              {
                type          : "custom"
                name          : "finder"
                paneClass     : FinderView
              }                                       
              {
                type          : "custom"
                name          : "JSEditor"
                paneClass     : EditorView
              }                 
            ]
        ]
        
      @workContainer.addSubView @workTerminal = new Workspace
        title      : "Terminal"
        name       : "Terminal"
        cssClass   : "terminal-view"
        panels     : [
          title               : "Terminal"
          layout              :
            direction         : "vertical"
            sizes             : ["100%"]
            splitName         : "BaseSplit"
            views             : [
              {
                type          : "custom"
                name          : "Terminal"
                paneClass    : TerminalView
              }                 
            ]
        ]
  

      @workEditor.once "viewAppended", =>
        @emit 'ready'

        {JSEditor} = \
          @workEditor.activePanel.panesByName

        JSEditor.ace.once 'ace.ready', =>
      
          JSEditor.ace.editor.on "change", \
            _.debounce (@lazyBound 'emit', 'previewApp', no), 500

    
      #Installer Container
      @addSubView @installContainer = new KDCustomHTMLView
        tagName    : "div"
        cssClass   : "install-container"
      
      @installContainer.addSubView new KDHeaderView
        title         : "PhoneGap Installer"
        type          : "big"

      @installContainer.addSubView @installToggle = new KDToggleButton
        cssClass        : 'toggle-button'
        style           : "clean-gray" 
        defaultState    : "Show details"
        states          : [
          title         : "Show details"
          callback      : (cb)=>
            @installTerminal.setClass 'in'
            @installToggle.setClass 'toggle'
            @installTerminal.webterm.setKeyView()
            cb?()
        ,
          title         : "Hide details"
          callback      : (cb)=>
            @installTerminal.unsetClass 'in'
            @installToggle.unsetClass 'toggle'
            cb?()
        ]

      @installContainer.addSubView new KDCustomHTMLView
        tagName       : 'img'
        cssClass      : 'logo'
        attributes    :
          src         : "#{gitResources}/phonegap.png"

      @installContainer.addSubView @installProgress = new KDProgressBarView
        initial       : 100
        title         : "Checking installation..."

      @installContainer.addSubView @installTerminal = new TerminalPane
        cssClass      : 'terminal'

      @installContainer.addSubView @installButton = new KDButtonView
        title         : "Install PhoneGap"
        cssClass      : 'main-button solid'
        loader        :
          color       : "#FFFFFF"
          diameter    : 12
        callback      : => @installCallback()

      @installContainer.addSubView new KDCustomHTMLView
        cssClass : "phonegap-help"
        partial  : """
          <p>The Koding PhoneGap app provides you with a playground where you can easily do mobile app development for Android or iOS.</p>
          <p>By installing the Phonegap Developer companion app on either your <a href="#{iosApp}">iPhone</a> or your <a href="#{androidApp}">Android</a> device, you will beable to view and test your amazing new app in realtime.</p>
          <p>Build on Koding, run on your phone...simple! :)</p>
          <p><img src="https://raw.githubusercontent.com/bvallelunga/PhoneGap.kdapp/master/resources/screenshot.png"/></p>
          <p><strong>Note: Node.js and PhoneGap will be installed/updated.</strong></p>
        """
    
      @watcher = new LogWatcher
      @checkState()
  
  startWork:->
    {Terminal} = @workTerminal.activePanel.panesByName
    Terminal.terminal.runCommand "cd ~/PhoneGap;";
  
  startDemo:->
    {Terminal} = @workTerminal.activePanel.panesByName
    Terminal.terminal.runCommand """
      cd ~/PhoneGap/hello;
      phonegap serve;
    """;
    
    {finder} = @workEditor.activePanel.panesByName
    finder.loadFile("~/PhoneGap/hello/www/index.html")
  
  checkState:->
    vmc = KD.getSingleton 'vmController'
    @installButton.showLoader()

    FSHelper.exists phoneGapBin, vmc.defaultVmName, (err, PhoneGap)=>
      warn err if err
      
      unless PhoneGap
        @installProgress.updateBar 100, '%', "PhoneGap is not installed."
        @switchState 'install'
      else
        @switchState 'ready'
        
  switchState:(state = 'run')->
    @loadingContainer.hide()
    @watcher.off 'UpdateProgress'

    switch state
      when 'install'
        @installContainer.setClass "active"
        @workContainer.unsetClass "active"
        @installButton.hideLoader()
      when 'ready'
        @installContainer.hide()
        @workContainer.setClass "active"
        @startWork()
      when 'demo'
        @installContainer.hide()
        @workContainer.setClass "active"
        @startDemo()
    
    @workSpaceFix()
   
  workSpaceFix:->
    # This is a temporary resize fix for KDSplitView
    # TODO: Remove this when its fixed in KD Framework
    wc = KD.getSingleton("windowController")
    wc.notifyWindowResizeListeners()
    wc.notifyWindowResizeListeners()
    
  stopCallback:->
    @_lastRequest = 'stop'
    @checkState()

  installCallback:->
    @watcher.on 'UpdateProgress', (percentage, status)=>
      @installProgress.updateBar percentage, '%', status
      
      if percentage is "100"
        @installButton.hideLoader()
        @switchState 'demo'
      
      else if percentage is "10"
        @installTerminal.unsetClass 'in'
        @installToggle.setState 'Show details'
        @installToggle.unsetClass 'toggle'

    session = (Math.random() + 1).toString(36).substring 7
    tmpOutPath = "#{outPath}/#{session}"
    vmc = KD.getSingleton 'vmController'
    vmc.run "rm -rf #{outPath}; mkdir -p #{tmpOutPath}", =>
      @watcher.stopWatching()
      @watcher.path = tmpOutPath
      @watcher.watch()
      
      @installTerminal.setClass 'in'
      @installTerminal.webterm.setKeyView()
      @installToggle.setState 'Hide details'
      @installToggle.setClass 'toggle'
      @installTerminal.runCommand "curl --silent -L #{installerScript} | sudo bash -s #{session} #{user}"

class PhonegapController extends AppController

  constructor:(options = {}, data)->
    options.view    = new PhonegapMainView
    options.appInfo =
      name : "Phonegap"
      type : "application"

    super options, data

do ->

  # In live mode you can add your App view to window's appView
  if appView?

    view = new PhonegapMainView
    appView.addSubView view

  else

    KD.registerAppClass PhonegapController,
      name     : "Phonegap"
      routes   :
        "/:name?/Phonegap" : null
        "/:name?/bvallelunga/Apps/Phonegap" : null
      dockPath : "/bvallelunga/Apps/Phonegap"
      behavior : "application"