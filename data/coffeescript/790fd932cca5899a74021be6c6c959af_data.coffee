(data) ->

  map        = OpenLayers.devicesMap
  thisMap    = OpenLayers.thisMap
  projection = OpenLayers.thisMap.getProjectionObject() 
  
  createBubble = (device, lonlat, icon) -> 
    devid   = device._id
    gps     = device.gps
    content = "<div class=\"device-id-label\">#{devid}</div><div class=\"device-gps\">x: #{gps.x}, y: #{gps.y}</div>" 
    result  = new OpenLayers.Popup.FramedCloud \
                               "device-popup-"+devid, 
                               lonlat, 
                               new OpenLayers.Size(150, 100),
                               content,
                               icon,
                               true
    result.device = device                                                  
    result.toggle()
    result
   
  selectMarker = (marker) ->
    marker.setUrl "images/network_radar_selected.png"
    marker.selected = true
    $("#items").trigger "device-selected", [marker.device]
    map.selected_marker = marker
   
  unselectMarker = (marker) ->
    marker.setUrl "images/network_radar.png"
    marker.selected = false
    marker.popup.toggle() if marker.popup and marker.popup.visible()      
    $("#items").trigger "device-selected", [null]
   
  toggleMarker = (marker) ->
    selected_marker = map.selected_marker
    if selected_marker is undefined # initial state
      bounds = new OpenLayers.Bounds()
      bounds.extend marker.lonlat
      thisMap.setCenter bounds.getCenterLonLat(), thisMap.getZoomForExtent(bounds)-8
    if selected_marker isnt undefined and selected_marker isnt marker
      unselectMarker selected_marker    
    if !marker.selected
      selectMarker marker
    else
      unselectMarker marker
  
  onMouseDown = (_) ->
    if !@popup
      @popup  = createBubble @device, @lonlat, @icon    
      thisMap.addPopup @popup
    @popup.toggle()
    (toggleMarker @) if map.selected_marker isnt @
  
  markOnMap = (device, marker) ->
    gps_pos = (new OpenLayers.LonLat device.gps.x, device.gps.y).transform  \
                                    new OpenLayers.Projection("EPSG:4326"),
                                    projection
    if !marker?
      icon = new OpenLayers.Icon "images/network_radar.png", 
                                 new OpenLayers.Size(48,48), 
                                 new OpenLayers.Pixel(-24, 24)                                    
      marker = new OpenLayers.Marker gps_pos, icon    
      marker.events.register "mousedown", marker, onMouseDown
      map.addMarker marker
    lonlat = marker.lonlat
    if lonlat and gps_pos and (lonlat.lon != gps_pos.lon or lonlat.lat != gps_pos.lat)      
      marker.lonlat = gps_pos
      if marker.popup
        thisMap.removePopup marker.popup
        delete marker.popup
    marker.device = device
  
  ## main body
  
  values = new Array
  
  if data.rows.length > 0
    if $("#items").data("selected-device") is undefined
      $("#items").trigger "device-selected", [data.rows[0].doc]
    devids = new Array    
    to_add = {}  
    # gather some interesting arrays and hashes
    for item in data.rows
      device = item.doc
      devids.push device._id
      values.push device
      to_add[device._id] = device
    # initially, we want to add markers for all devices  
    to_del = new Array
    # but 1st let's check if some of the devices are already marked?
    for marker in map.markers
      devid = marker.device._id 
      # marked, so we've got to remove it from the "to-be-marked" list
      to_del.push(devid) if devid in devids      
      # iff the position of the device changed it should be noticed by the map:
      # correct the mark lon/lat!
      device = to_add[devid]
      markOnMap device, marker
    # remove devices which should not be marked
    delete to_add[devid] for devid in to_del
    # create marks  
    markOnMap(device) for _, device of to_add
    # select the first device marker
    toggleMarker map.markers[0]
    map.redraw()
  # return value
  {items : values}
