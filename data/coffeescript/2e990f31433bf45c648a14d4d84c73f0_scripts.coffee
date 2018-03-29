window.App = Ember.Application.create()

App.Section = Ember.Object.extend
  title: null
  subtitle: null
  placeholder: 'Add content ...'

App.ListingSection = App.Section.extend
  list: []
  maxItems: 7
  labelPlaceholder: ''
  valuePlaceholder: ''
  filterTopValues: false
  empty: (->
    result = true
    list = @get 'list'
    for item in list
      if item.label || item.value
        return false
    return true
  ).property('list.@each.label', 'list.@each.value')
  initList: (->
    list = []
    for i in [0..(@get('maxItems')-1)]
      list.push App.ListItem.create()
    @set 'list', list
  ).on('init')

App.ListItem = Ember.Object.extend
  label: null
  value: null
  empty: (->
    !(@get('label') and @get('value'))
  ).property('label', 'value')

App.ContentSection = App.Section.extend
  content: null
  author: null
  authorIsUser: false
  inputPlaceholder: null

App.ApplicationController = Ember.Controller.extend
  name:
    editing: false
    placeholder: 'Add name'
    value: null

  location:
    editing: false
    placeholder: 'Add location'
    value: ''
    address: null
    coordinates: null

  languages:
    editing: false
    placeholder: 'Add languages'
    value: null

  profileImage:
    editing: false
    placeholder: 'Add <br/> profile image ...'
    filename: null
    file: null

  skills:
    editing: false,
    placeholder: 'Add skill'
    options: [
      key: 'weight1'
      value: 'strong'
    ,
      key: 'weight2'
      value: 'average'
    ,
      key: 'weight3'
      value: 'weak'
    ]
    values: Ember.A []
    new:
      name: null
      class: null

  portfolio: App.ListingSection.create
    title: 'Portfolio'
    labelPlaceholder: 'Project name'
    valuePlaceholder: 'Skills used'
    filterTopValues: true

  experience: App.ListingSection.create
    title: 'Experience'
    labelPlaceholder: 'Skill'
    valuePlaceholder: 'Years of experience'

  content1: App.ContentSection.create
    title: 'The most amazing ...'
    inputPlaceholder: 'Tell us about the best software you have worked on...'
    authorIsUser: true

  content2: App.ContentSection.create
    title: 'In clients I look for ...'
    authorIsUser: true

  content3: App.ContentSection.create
    title: 'Note'

  content4: App.ContentSection.create
    title: 'Note'

  actions:
    beginNameEdit: ->
      @set 'name.editing', true
    endNameEdit: ->
      @set 'name.editing', false
    beginLocationEdit: ->
      @set 'location.editing', true
    endLocationEdit: ->
      @set 'location.editing', false
    beginLanguagesEdit: ->
      @set 'languages.editing', true
    endLanguagesEdit: ->
      @set 'languages.editing', false
    beginProfileImageEdit: ->
      @set 'profileImage.editing', true
    endProfileImageEdit: ->
      @set 'profileImage.editing', false
    beginSkillsEdit: ->
      @set 'skills.editing', true
    endSkillsEdit: ->
      @set 'skills.editing', false
      newSkill = @get 'skills.new'
      if newSkill.name
        @get('skills.values').pushObject newSkill
        @set 'skills.new',
          name: null
          class: null
    removeSkill: (skill) ->
      @get('skills.values').removeObject skill


App.FocusInputComponent = Ember.TextField.extend
  becomeFocused: (->
    @$().focus()
  ).on 'didInsertElement'

App.LocationView = Ember.View.extend
  templateName: 'location'
  geocoder: null
  showLoader: false
  loading: false
  value: null

  checkGeocoder: (value) ->
    if value
      geocoder = @get 'geocoder'
      unless geocoder?
        geocoder = new google.maps.Geocoder()
        @set 'geocoder', geocoder
      geocoder.geocode {'address': value}, (results, status) =>
        if status is google.maps.GeocoderStatus.OK
          @set 'controller.location.coordinates', results[0].geometry.location
          @set 'controller.location.address', results[0].formatted_address
        else
          @set 'controller.location.coordinates', null
          @set 'controller.location.address', null
    else
      @set 'controller.location.coordinates', null
      @set 'controller.location.address', null
    # setTimeout used so we don't bombard google API with garbage on every keystroke
    setTimeout =>
      unless value is @get 'controller.location.value'
        @checkGeocoder @get 'controller.location.value'
      else
        @set 'loading', false
    ,
      1000

  locationChanged: (->
    value = @get 'controller.location.value'
    unless @get 'loading'
      @set 'loading', true
      @checkGeocoder value
  ).observes('controller.location.value')

  toggleLoader: (->
    @$('span.loader').toggleClass 'hidden'
  ).observes('loading')

App.MapView = Ember.View.extend
  templateName: 'map'
  tagName: 'section'
  classNames: ['content']

  geocoder: null
  map: null
  marker: null
  address: null

  locationChanged: (->
    unless @get 'controller.location.editing'
      coordinates = @get 'controller.location.coordinates'
      address = @get 'controller.location.address'
      map  = @get 'map'
      markerCleanup = =>
        marker = @get 'marker'
        if marker
          # Cleanup
          marker.setMap null
          marker = null
          @set 'marker', null
      if coordinates and address
        # If map is initialized, adjust the marker and map center
        unless map
          latlng = new google.maps.LatLng -34.397, 150.644
          mapOptions =
            zoom: 8,
            center: latlng
          if @$('#map').length is 0
            @$('figure').prepend '<div id="map"/>'
          @set 'map', new google.maps.Map @$('#map')[0], mapOptions
          map = @get 'map'
        map.setCenter coordinates
        markerCleanup()
        @set 'marker', new google.maps.Marker
          map: map,
          position: coordinates
        @set 'address', address
      else
        markerCleanup()
        @$('#map').remove()
        map = null
        @set 'map', null
        @set 'address', null
  ).observes('controller.location.editing')

  addressAndNameChanges: (->
    name = @get('controller.name.value')
    address = @get('address')
    if name and address
      @$('figcaption').text "#{name} lives in #{address}"
    else
      @$('figcaption').text ''
  ).observes('address', 'controller.name.value').on('didInsertElement')

  didInsertElement: ->
    @set 'geocoder', new google.maps.Geocoder()

App.ProfileImage = Ember.View.extend
  tagName: 'div'
  classNames: ['image']
  attributeBindings: ['style']
  templateName: 'profileImage'
  image: null
  style: (->
    image = @get 'image'
    if image
      "background-image: url('#{image}');background-size: 100% auto;background-position:center center;background-repeat:no-repeat;"
    else
      ""
  ).property('image')

  updateImage: (->
    @set 'controller.profileImage.file', @get 'image'
  ).observes('image')


App.ImageUpload = Ember.TextField.extend
  tagName: 'input'
  attributeBindings: ['name']
  type: 'file'
  change: (e) ->
    reader = new FileReader()
    reader.onload = (e) =>
      @set 'parentView.image', e.target.result
    reader.readAsDataURL(e.target.files[0])



App.SectionComponent = Ember.Component.extend
  tagName: 'section'
  classNames: ['content']
  classNameBindings: ['editing:editing']
  editing: false

  actions:
    startEditSection: ->
      @set 'editing', true
    endEditSection: ->
      @set 'editing', false

App.ContentSectionComponent = App.SectionComponent.extend
  userFirstName: (->
    name = @get('userName')
    if typeof name is 'string' then name.match /^\w+/ else null
  ).property('userName')

App.ListingSectionComponent = App.SectionComponent.extend
  summarizeValues: (->
    unless @get 'editing'
      section = @get 'section'
      if section and section.get 'filterTopValues'
        occurrences = {}
        for item in section.get 'list'
          if item and item.value
            matches = item.value.match /\w+/gi
            matches.forEach (item, index) ->
              unless occurrences[item]
                occurrences[item] = 0
              occurrences[item]++
        values = []
        for key, value of occurrences
          values.push key
        values.sort (v1, v2) ->
          occurrences[v2] - occurrences[v1]
        unless values.length
          @set 'section.subtitle', null
        else
          values = values.slice 0, 3
          @set 'section.subtitle', values.join ', '

  ).observes('editing')