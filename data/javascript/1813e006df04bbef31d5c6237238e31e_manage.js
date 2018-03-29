/* ------------------------------------------------------------------------
 * manage.js
 * Copyright (c) 2007-2010 Brainberry, LLC. All rights reserved.
 * ------------------------------------------------------------------------ */

$(document).ready(function(){
  //$("input[type='text']:first", document.forms[0]).focus();
  var element = $('input:visible,select:visible,textarea:visible', document).get(0);
  if (element) element.focus();
});

function fade_flash()
{
  $('#manage_container div.message').slideUp('slow');
}

var Tree = {
	expand: function(e)
	{
	  var event = e || window.event;
    var target = event.target || event.srcElement;
  
	  var node = $(target).closest('a');
		if (typeof(node) == 'undefined') return;
  
    var item = node.parents('div.row-container');
    var stage = $('#' + item.attr('id') + '_children');
		
		if (stage != null) 
		{
		  var mode = stage.is(':visible') ? 1 : 0;
		  stage.toggle();
		  event.data.save(node.attr('id'), mode);
		}
		
		event.preventDefault();

		return false;
	},
	
	init: function(tree)
	{
		this.tree = $(tree);
		this.cookie = new wp_Cookie({ expires : 6, expires_unit : "months"});
		this.cookie_key = "mtree";
		
		var links = this.tree.find("a.dark-arr");
		
		for(var i = 0; i < links.length; i++)
		{
			node = $(links[i]);
      
			if (typeof(node) != 'undefined')
			{
			  node.bind('click', this, this.expand);
			  
			  mode = !this.state(node.attr('id'));
				
				var item = node.parents('div.row-container');
				var stage = $(item.attr('id') + '_children');
				
				if (stage != null)
				{
				  if (mode)	{stage.hide();} else {stage.show();}
				}
			}
		}
	},
	
	save: function(node_key, value)
	{	
		var str = this.cookie.get(this.cookie_key);
		var mas = new Array();
		
		if (str == null) str = '';
		
		var arr = str.split(';');
		
		mas.push(node_key + ":" + value);
		
		for(var i = 0; i < arr.length; i++)
		{
			hash = arr[i].split(':');
			if (hash[0] != node_key)
			{
				mas.push(arr[i]);
			}
		}
		
		str = mas.join(';');
		this.cookie.set(this.cookie_key, str);
	},
	
	state: function(node_key)
	{
		var str = this.cookie.get(this.cookie_key);
		if (str == null) str = '';
		var value = 0;
		
		var arr = str.split(';');
		for(var i = 0; i < arr.length; i++)
		{
			hash = arr[i].split(':');
			if (hash[0] == node_key)
			{
				value = parseInt(hash[1]);
				break;
			}
		}
	
		return value == 1;
	}
	
};

var items_class = null;

function init_collection(dom_id, class_name)
{ 
  var items = $("#" + dom_id).find('div.' + class_name);
  
  items_class = class_name;
  
  for (var i = 0; i < items.length; i++)
  {
    $(items[i]).bind('mouseover', handle_over);
    $(items[i]).bind('mouseout', handle_out);
  }
}

function handle_over(e)
{
  var event = e || window.event;
  var target = event.target || event.srcElement;
    
  var element = $(target).closest("div." + items_class);
  
  var dots = element.find('div.dot-block');
  var buttons = element.find('div.act-bl');
  
  if (dots != null) dots.show();
  if (buttons != null)  buttons.show();
}

function handle_out(e)
{
  var event = e || window.event;
  var target = event.target || event.srcElement;
    
  var element = $(target).closest("div." + items_class);
  
  var dots = element.find('div.dot-block');
  var buttons = element.find('div.act-bl');
  
  if (dots != null) dots.hide();
  if (buttons != null)  buttons.hide();
}


function append_node(sibling, element)
{
  sibling.css({left: 0, top: 0});
  element.css({left: 0, top: 0});
    
  $(element).insertAfter(sibling);
}

function move_node(dom_id, direction)
{
  var element = $('#' + dom_id);
  var sibling = null;
  var node = null;
  
  switch(direction)
  {
    case 'up':
      sibling = element.prev();
      node = element;
      element = sibling;
      sibling = node;
      break;
    case 'down':
      sibling = element.next();
      break;
  }
  
  if (sibling != null)
  { 
    var element_height = element.height() + 10;
    var sibling_height = sibling.height() + 10; 
    
    element.animate({left: 0, top: sibling_height}, { duration: "slow" });
    
    sibling.animate({left: 0, top: -element_height}, {duration: "slow", complete: function(){
        append_node(sibling, element); 
      }});
  }
}

var SelectList = {
	show: function(element, list)
	{ 
	  var size = { width: element.width(), height: element.height() };
	  var pos = element.offset();
	  var height = list.height();
	  var width = size.width + 20;
	  	  
	  list.css({
	    width: ((width < 150) ? 150 : width) + "px",
	    left: pos.left + "px",
	    top: pos.top + size.height + "px",
	    height: ((height > 200) ? 200 : height) + 'px',
	    zIndex: 5000
	  });
	  
	  list.slideDown();
	},
	
	hide: function(list)
	{
	  list.slideUp();
	},
	
	toggle: function(e)
	{
	  var event = e || window.event;
    var target = event.target || event.srcElement;
     
	  var element = $(target).closest("a");
	  var list = $('#' + element.attr('id') + '_list');
	  
	  list.is(':visible') ? this.hide(list) : this.show(element, list);
	  
	  return false;
	}
};

var TabPanel = {
	click: function(e)
	{
	  var event = e || window.event;
    var target = event.target || event.srcElement;
    
	  var element = $(target).closest("a");
	  var container = $('#' + element.attr('id') + "_block");
	  var li = element.parents('li');
	  var parent_ul = li.parents('ul');
	  
	  parent_ul.nextAll('div.add-white-bl').hide();
	  parent_ul.children('li').removeClass('active');
	  parent_ul.children('li').addClass('not-active');
	  
	  li.toggleClass("not-active");
	  li.addClass('active');
	  
	  container.show();
	}
};

function insert_fields(link, method, content) 
{
  var new_id = new Date().getTime();
  var regexp = new RegExp("new_" + method, "g")
  
  $(link).before(content.replace(regexp, new_id));
}

function remove_fields(link) 
{
  var hidden_field = $(link).prev("input[type=hidden]");
  
  if (hidden_field) {
    hidden_field.val('1');
  }
  
  $(link).closest("div.elem-bl").hide();
}

function toggle_element(dom_id)
{ 
  var cookie = new wp_Cookie({ expires : 3, expires_unit : "months"});
  var element = $('#' + dom_id);
  
  cookie.set(element.id, (element.is(':visible') ? 0 : 1));
  
  element.is(':visible') ? element.slideUp("slow") : element.slideDown("slow");
}
