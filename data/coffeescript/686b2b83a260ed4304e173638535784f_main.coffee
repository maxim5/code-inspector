# To set up for debugging: 
# python -m SimpleHTTPServer & coffee -wclj CoffeeMol.coffee Selector.coffee CanvasContext.coffee Element.coffee Structure.coffee Chain.coffee Residue.coffee Atom.coffee main.coffee

# In pixels
ATOM_SIZE = 3
DEBUG = true

if typeof String.prototype.startswith != 'function'
	String.prototype.startswith = (str) ->
		@slice(0, str.length) == str

if typeof String.prototype.endswith != 'function'
	String.prototype.endswith = (str) ->
		@slice(-str.length) == str

if typeof Array.prototype.norm != 'function'
	Array.prototype.norm = ->
		Math.sqrt @.dot @

if typeof Array.prototype.dot != 'function'
	Array.prototype.dot = (v) ->
		if v.length != @.length
			alert "Lengths for dot product must be equal"
		summation (v[i]*@[i] for i in [0..v.length-1])

nuc_acids = ["A",  "C",  "G",   "T",
			 "DA", "DC", "DG", "DT",
			 "RA", "RC", "RG", "RT"]


# See http://www.science.uwaterloo.ca/~cchieh/cact/c120/bondel.html
# Currently, just use < 2
#average_bond_lengths =
#	["C", "C"]: 1.54
#	["N", "N"]: 1.45
#	["O", "O"]: 1.21
#	["C", "N"]: 1.47

supported_draw_methods = ["both", "lines", "points", "cartoon"]

summation = (v) ->
	r = 0
	for x in v
		r += x
	r

encodeHTML = (s) ->
	s.replace("<", "&lt;").replace(">", "&gt;")

timeIt = (fn) ->
	t_start = new Date
	fn()
	(new Date) - t_start

hexToRGBArray = (h) ->
	if h instanceof Array
		return h

	# Some flavors of hex...
	if h.startswith "0x"
		h = h.substring 2

	temp = (h.substring i, i+2 for i in [0..4] by 2)
	(parseInt t, 16 for t in temp)

arrayToRGB = (a) ->
	if typeof a == 'string'
		if a.startswith "#"
			return a
		else
			alert "Improperly formatted string -> color. \
					Must be of the form #XXXXXX"

	if not a?
		a = randomRGB()
		if DEBUG
			alert "No color defined for #{a.toString()}. Using a random color"
	
	# RGB must be an array of length 3
	if a.length != 3
		alert "Array To RGB must be of length 3, it is length #{a.length}: #{a}"

	# Make sure our colors are within 0 to 255 and are integers
	fixer = (c) ->
		c = if c > 255 then c = 255 else c
		c = if c < 0   then c = 0   else c
		parseInt c
	a = (fixer x for x in a)
	"rgb(#{a[0]}, #{a[1]}, #{a[2]})"

# Algorithm to determine whether or not two atoms should be bonded
isBonded = (a1, a2) ->
	if a1.parent.typeName() != a2.parent.typeName()
		return false

	# Precompute distance
	aad = atomAtomDistance(a1, a2)

	# Cartoon method shall only draw bonds between protein-protein C-alphas and 
	# along the DNA Phosphate backbone
	if a1.info.drawMethod == 'cartoon'
		if aad < 4 and a1.parent.isProtein() \
				and a1.original_atom_name == "CA" \
				and a2.original_atom_name == "CA"
			return true
		else if aad < 10 and a1.parent.isDNA() \
				and a1.original_atom_name == "P" \
				and a2.original_atom_name == "P"
			return true
		else
			return false
	else
		if aad < 2
			return true
		else
			return false

degToRad = (deg) ->
	deg*0.0174532925

radToDeg = (rad) ->
	rad*57.2957795

delay = (ms, f) -> 
	setInterval f, ms

pdbAtomToDict = (a_str) ->
	# Sometimes PDBs use `DA` instead of `A` for nucleotides
	handleResiName = (r) ->
		if r in nuc_acids[4..nuc_acids.length] then r.substr(1, 2) else r
	
	# We only need the elemental symbol
	handleAtomName = (a) ->
		a.substr 0, 1
	
	original_atom_name: $.trim a_str.substring 13, 16
	atom_name: handleAtomName $.trim a_str.substring 13, 16
	resi_name: handleResiName $.trim a_str.substring 17, 20
	chain_id:  $.trim a_str.substring 21, 22
	resi_id:   parseInt a_str.substring 23, 26
	x: parseFloat a_str.substring 31, 38
	y: parseFloat a_str.substring 38, 45
	z: parseFloat a_str.substring 46, 53

randomInt = (maxInt) ->
	Math.floor(Math.random()*maxInt)

randomRGB = ->
	rr = -> randomInt 255
	[rr(), rr(), rr()]

# Object deep-copy. See http://stackoverflow.com/a/122704/178073
deepCopy = (o) ->
	$.extend(true, {}, o)

randomDrawMethod = ->
	supported_draw_methods[randomInt supported_draw_methods.length]

defaultInfo = ->
	drawMethod: randomDrawMethod()
	drawColor: randomRGB()
	borderColor: [0, 0, 0]

genIFSLink  = (selector_str, key, val, pretty) ->
	link = "javascript:window.coffeemol.changeInfoFromSelectors('#{selector_str}', \
			'#{key}', '#{val}');"
	"<div class='dropdown-option'><a href=\"#{link}\">#{pretty}</a></div>"

mousePosition = (e) ->
	if not e.offsetX? or not e.offsetY?
		x: e.layerX - $(e.target).position().left
		y: e.layerY - $(e.target).position().top
	else
		x: e.offsetX
		y: e.offsetY

addNewStructure = (e) ->
	filepath = $("#add-new-structure .text").val()
	coffeemol.addNewStructure filepath

fromSplashLink = (filename) ->
	coffeemol.addNewStructure filename, {drawMethod: 'cartoon'} 

coffeemol = new CanvasContext "#coffeemolCanvas"

# If we are in the debug environment
# Attach coffeemol instance to window to use it in the HTML
window.coffeemol = coffeemol
window.fromSplashLink = fromSplashLink
