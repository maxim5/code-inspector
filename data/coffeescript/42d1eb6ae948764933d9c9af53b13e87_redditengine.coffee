root = window 

window.hello = "terve enginesta"

#jeesus.jeesus()
#console.log "Parsing redditengine!!!"

window.log = ->
	#alert arguments[0]
	#console.log Array::slice.call arguments 
	console.log Array::slice.call arguments if @console
	return


window.hello = "terve enginesta2"

class App
    start: ->


        log 21
        $("div[data-role=page]").page()

        @topicGroups = new RTopicGroupList
        
        #@topicGroups.fetch()


        @tgview = tg = new RTopicGroupView        
                
             
        createGroups = =>            
            log "creating"
            log 34,@tgview
            @tgview.addTg "Casual",["frontpage", "pics", "fffffffuuuuuuuuuuuu", "funny", "AdviceAnimals"]
            log 36
            @tgview.addTg "Code",["programming", "webdev", "javascript", "web_design", "html5", "coffeescript", "python"]
            log 35
	
        

	        
        $("#btnWizardCreate").on "click", =>
            createGroups()
            $("#pagesetupwizard").dialog("close")
            

        $("#pagepreview").on "click", =>
            $.mobile.changePage "#pagemain"
            $("#previewIframe").attr "src", ""
            
        log 48
        @shownCategories = new RCatList
        
        log 51
        root.redditengine = reng = new RedditEngine()

        log 53

        #tg.addTg "Funny stuff", ["pics", "fffffffuuuuuuuuuuuu"]
        #tg.addTg "Programming", ["javascript", "html5", "coffeescript"]
        
        if not @topicGroups.length
            
            createGroups()
            #_.delay (=>                              
            #    $.mobile.changePage "#pagesetupwizard"), 500

        log 63
        #@topicGroups.sync()
        @tgview.render()

        @mainview = mv = new RCatListView

        @vManageGroups = mg = new VManageGroups
        mg.render()

        @vGroupEditor = ge = new VGroupEditor
        
        EventDispatcher.bind "selectCategories", (ev, cats) =>            
            mv.setCategories (cats)
            mv.render()
            reng.fetchAll()

        reng.initialize()
        reng.fetchAll()        


window.hello = "terve enginesta3"
EventDispatcher = $({})
try
    app = new App()
catch e
    console.log "Error creating app",app
    

window.hello = "terve enginesta4"

collectionToJson = (coll)->    
    coll.map (m) ->
        o = m.toJSON()
        o.cid = m.cid
        o

window.hello = "terve enginesta10"
    
    
class RLink extends Backbone.Model
    defaults:
        linkdesc: "no description"

window.hello = "terve enginesta11"

class RCat extends Backbone.Model
    defaults:
        name: "funny"


class RTopicGroup extends Backbone.Model
    defaults:
        groupName: "mygroup"
        topics: ["pics", "funny"]

window.hello = "terve enginesta11.1"

class RTopicGroupList extends Backbone.Collection
    #localStorage: new Store("topicgroups")
    model: RTopicGroup
    
window.hello = "terve enginesta12"

class RTopicGroupView extends Backbone.View
    el: "#topic-group-area"
    
    initialize: ->
        log 136
        pat = $("#topic-group-template").html()
        @template = Handlebars.compile pat        
        @tglist = app.topicGroups
        @tglist.bind "change remove", (args...) =>            
            @render()
            
        log 143
        
    render: ->
        @$el.empty()
        
        @tglist.each (m) =>
            rend = @template
                tgname: m.get "groupName"
                topics: m.get "topics"
                tgid: m.cid
                
            
            @$el.append rend
            
    addTg: (name, topics) ->
        log 154
        #@tglist.create groupName: name, topics: topics
        m = new Backbone.Model groupName: name, topics: topics
        log 157
        @tglist.add m
        log 158

    makeCurrent: (elem) ->
        $(".tg-current-choice").removeClass "tg-current-choice"
        elem.addClass "tg-current-choice"
        
    doSelectGroup: (ev) ->        
        trg = $(ev.target)
        cid = trg.data("cid")
        @makeCurrent trg
        m = @tglist.getByCid cid
        
        
        EventDispatcher.trigger "selectCategories", [m.get "topics"]
        
    
    doSelectTopic: (ev) ->
        trg = $(ev.target)
        @makeCurrent trg
        topic = trg.text()        
        EventDispatcher.trigger "selectCategories", [[topic]]
        

    events: 
        "click .tg-name" : "doSelectGroup"
        "click .tg-topic" : "doSelectTopic"
    

class RLinkList extends Backbone.Collection
    model: RLink
    
class RCatList extends Backbone.Collection
    model: RCat

window.hello = "terve enginesta13"

class RCatListView extends Backbone.View

    el: "#catlist-container"
    
    initialize:  ->                
        _.bindAll @        
        @categories_coll = new RCatList
        pat = $("#catlist-template").html()
        @catlisttmpl = Handlebars.compile pat
        @singlecatviews = {}
            
        
    render: ->
        @$el.empty()
        
        all = $('<div class="gen-cat-list-container">')
        app.shownCategories.each (m) =>
            name = m.get "name"
            rendered = @catlisttmpl
                catname: name
             
            appended = $(rendered).appendTo(all)
            
            r = appended.find(".catlist-links")
            
            #if name in @singlecatviews
            #    @singlecatviews[name].el = r
            #else
            nv = new RCatView el: r
            @singlecatviews[name] = nv
            
            
        
        @$el.append(all)
        
    
    setCategories: (cats)->
        app.shownCategories.reset ({name} for name in cats)
    
    getView: (name) -> @singlecatviews[name]
        
    categories: -> @categories_coll
        
    
    
class RCatView extends Backbone.View
    
    events:
        "click .linkcontainer"  : "doSelect"
        "click .rightedge" : "doSelectComments"
        #"click .linkcomments" : "doSelectComments"
        
    modelByCid: (cid) -> @coll.getByCid cid
        
        
    openWindow: (url) ->
        #fr = $("#previewIframe")
        #fr.attr "src", url
        #$.mobile.changePage "#pagepreview"
        
        #resizeIframeWidth(fr.get(0))
        
        window.open url
        
    doSelect: (ev) ->
        
        cid = $(ev.currentTarget).data("cid")
        #console.log ev, cid
        m = @modelByCid cid
        #console.log m
        url = m.get "url"
        @openWindow url
        
    
    doSelectComments: (ev) ->
        cid = $(ev.currentTarget).parent().data("cid")
        
        m = @modelByCid cid
        plink = m.get "permalink"        
        
        fullurl = "http://reddit.com" + plink+".compact"
        @openWindow fullurl
        
        
        ev.stopPropagation()
        
    initialize: ->
        @coll = new RLinkList
        _.bindAll @
        pat = $("#link-template").html()
        @linktmpl = Handlebars.compile pat
        
        
    renderOne: (m) ->
        thumb = m.get "thumbnail"
        #console.log thumb
        if thumb in ["default", "self"]
            #console.log "squash because",thumb
            thumb = ""
            
        expanded = @linktmpl
        
            linkdesc: m.get "title"
            linkscore: m.get "score"
            linkimg: thumb            
            linkcomments: m.get "num_comments"
            cid: m.cid
            
        expanded
    
    render: ->
        all = $('<ul data-role="listview" data-theme="c">')
        @coll.each (m) =>
            all.append $(@renderOne(m))
                
        
        @$el.empty()
        @$el.append all
        @$el.trigger "create"
        #all.listview()
        #all.listview("refresh")

    mkModel: (d) ->
        m = new RLink
        m.set d
        #console.log m
        m
        
    addLink: (d) ->
        m = @mkModel d
        @coll.add m

class VManageGroups extends Backbone.View
    el: "#manage-groups-area"
    
    events:
        "click .topic-group-item" : "doSelectGroup"
        "click #btnNewGroup" : "doNewGroup"
        
    initialize: ->
        _.bindAll @
        pat = $("#manage-groups-template").text()
        @tmplManageGroups = Handlebars.compile pat
        app.topicGroups.bind "change remove", =>
            @render()
            
            
    
        
        #console.log "init!",pat
        

    render: ->        
        context = { groups: collectionToJson app.topicGroups }        
        h = @tmplManageGroups context
        @$el.html h
        #@.$(".rootlist").listview()
        @$el.trigger "create"
        
        
    modelByCid: (cid) -> app.topicGroups.getByCid cid
        
    doSelectGroup: (ev) ->        
        m = @modelByCid $(ev.currentTarget).data("cid")

        @switchToGroupEditor m         
    
    switchToGroupEditor: (m) ->
        app.vGroupEditor.setModel m
        #app.vGroupEditor.render()

        $.mobile.changePage "#pagegroupeditor"
        
        #_.defer =>        
        #    $.mobile.changePage "#pagegroupeditor"
        #    app.vGroupEditor.updateList()
        
        
    doNewGroup: (ev) ->        
        m = app.topicGroups.create groupName: "<untitled>", topics: []
        #app.tgview.render()
        #app.vManageGroups.render()
        @switchToGroupEditor m
        app.vGroupEditor.setModel m
        
        
class VGroupEditor extends Backbone.View
    el: "#group-editor-area"
    
    events:
        "click #btnAdd": "doAddCat"
        "click .aRemoveCat": "doRemoveCat"
        "click #btnApplyChangeGroupName" : "doChangeGroupName"
        "keyup #inpGroupName" : "doCheckGroupName"
        
    initialize: ->
        _.bindAll @
        pat = $("#group-editor-template").text()
        @tmpl = Handlebars.compile pat
    
        $("#btnDeleteGroup").on "click", =>            
            #app.topicGroups.remove @model
            @model.destroy()
            history.back()
            
    
        
        
        #$("#pagegroupeditor").on "pagebeforecreate", =>
        #    @render()
        
    doCheckGroupName: ->        
        b = $("#btnApplyChangeGroupName")
        t = $("#inpGroupName").val()
        kl = 'ui-disabled'
        ref = false
        if t != @model.get "groupName"
            if b.hasClass kl                
                b.removeClass "ui-disabled"
                ref = true
                
        else
            if not b.hasClass kl
                b.addClass "ui-disabled"                
                ref = true
                
            
        
    updateList: ->
        ul = @.$(".rootlist")        
        ul.listview()
        ul.listview("refresh")
        
    render: ->        
        if not @model
            return
            
        context = @model.toJSON()
        h = @tmpl context
        @$el.html h
        @$el.trigger "create"
        #@updateList()
        
    setModel: (m)->
        @model = m
        @render()
        m.on "change", =>
            @render()
        @doCheckGroupName()
        
    doAddCat: (ev) ->
        
        t = $("#inpNewCategory").val()        
        if t.length < 1
            return
        m = @model
        topics = m.get "topics"                
        #topics.push t
        m.set "topics", topics.concat [t]         
        m.save()
        
        #@render()
        #@updateList()
        
    doRemoveCat: (ev) ->
        elem =  $(ev.currentTarget)
        toRemove = elem.text()        
        ul = @.$(".rootlist")
        m = @model
        topics = _.without m.get("topics"), toRemove
        m.set "topics", topics        
        m.save()
        
        #@render()
        #@updateList()
        
    doChangeGroupName: (ev) ->
        t = $("#inpGroupName").val()        
        @model.set "groupName", t
        @model.save()
        @doCheckGroupName()
        
window.hello = "terve enginesta8"        
        
class RedditEngine    
    initialize: ->        
        #@linktmpl = _.template pat
        #console.log "template", @linktmpl
        @cats = []
        @linkviews = {}
        #@mkView "pics"
        #@mkView "funny"
        
        
        
        
        #mv.setCategories ["pics", "javascript"]
        #@mainview.addCategory("pics")
        #@mainview.addCategory("funny")
        #@mainview.render()

        
        
    fetchAll: ->
        
        app.shownCategories.each (m) => @fetchLinks m.get "name",""
            
    fetchLinks: (cat, qargs) ->                
        qargs = "jsonp=?&"
                
        if cat == "frontpage"
            catfrag = ""
        else
            catfrag = "/r/#{cat}/"
            
        url = "http://www.reddit.com#{catfrag}/.json?#{qargs} "

        
        lv = app.mainview.getView(cat)
        $.ajax
            url: url
            jsonp: "jsonp"
            dataType: "jsonp"
            success: (resp) =>
                items = resp.data.children
                #all = $("<div>")
                for it in items
                    d = it.data
                    #console.log d
                    
                    lv.addLink d
                    #all.append(expanded)
                    
                #console.log items
                lv.render()
                

window.hello = "terve enginesta9"        
root.RedditEngine = RedditEngine    
    
reng = null
    
root.redditapp = app

window.hello = "terve enginesta5"

$ ->
    window.hello = "terve enginesta6"
    log "starting up"
    #app.start()
    window.hello = "terve enginesta7"
    
