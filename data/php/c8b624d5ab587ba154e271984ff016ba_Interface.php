<?php # Interface Handler [axiixc]

class InterfaceHandler
{
	
	public $user_interface;
	public $path_system, $path_web;
	public $configuration;
	public $content, $content_override;
	public $interface, $interface_override, $interface_login_window;
	public $interface_keys = array();
	public $title_template, $title_system, $title_application;
	public $tagline, $footer, $description, $keywords;
	public $favicon, $iphoneicon;
	public $page_type, $mannagedUI;
	public $sidebars = array(), $javascript = array(), $notifications = array();
	public $templates = array();
	public $small_avatar, $medium_avatar, $large_avatar;
	
	public function __construct()
	{  
		$this->user_interface = cfRead('Default Interface');
		$this->path_system = root . 'Resources/Interfaces/' . $this->user_interface . '/';
		$this->path_web = www . 'Resources/Interfaces/' . $this->user_interface . '/';
		
		require $this->path_system . 'Configuration.php';
		$this->configuration = $interface_configuration;
		unset($interface_configuration);
		
		$this->content = null;
		$this->content_override = null;
		
		$this->interface = $this->configuration['interface'];
		$this->interface_override = $this->configuration['interface_override'];
		$this->interface_login_window = $this->configuration['login_window'];
		$this->interface_keys = array_merge(cfRead('Interface Keys'), $this->configuration['interface_keys']);
		
		$this->title_template = cfRead('Title Template');
		$this->title_system = cfRead('System Title');
		$this->title_application = null;
		
		$this->tagline = cfRead('System Tagline');
		$this->description = cfRead('System Description');
		$this->keywords = cfRead('System Keywords');
		
		$this->favicon = priority_select(cfRead('System Favicon'), 
			$this->path_web . 'Images/' . $this->configuration['favicon'], 
			www . 'Resources/Interfaces/System/Images/Favicon.png');
		$this->iphoneicon = priority_select(cfRead('System iPhone Icon'),
		 	$this->path_web . 'Images/' . $this->configuration['iphoneicon'], 
			www . 'Resources/Interfaces/System/Images/iPhoneIcon.png');
		
		$this->page_type = normal;
		$this->mannagedUI = true;
		
		$this->sidebars = array();
		$this->javascript = array();
		$this->javascript['head']['user'] = array();
		$this->javascript['body']['user'] = array();
		$this->javascript['head']['include'] = array();
		$this->javascript['body']['include'] = array();
		$this->javascript['onload'] = array();
		$this->notifications = array();
		
		if (file_exists($this->path_system . 'Templates.php'))
		{
			require $this->path_system . 'Templates.php';
			$interface_templates = $templates;
			unset($templates);
			
			require root . 'Resources/Interfaces/System/Templates.php';
			$system_templates = $templates;
			unset($templates);
			
			$this->templates = array_merge($interface_templates, $system_templates);
		}
		else
		{
			require root . 'Resources/Interfaces/System/Templates.php';
			$this->templates = $templates;
			unset($templates);
		}
		
		$this->avatar_small = priority_select($this->path_web . 'Images/' . $this->configuration['avatar_small'],
		   www . 'Resources/Interfaces/System/Images/Avatars/Small.png');
		$this->avatar_medium = priority_select($this->path_web . 'Images/' . $this->configuration['avatar_medium'],
		   www . 'Resources/Interfaces/System/Images/Avatars/Medium.png');
		$this->avatar_large = priority_select($this->path_web . 'Images/' . $this->configuration['avatar_large'],
		   www . 'Resources/Interfaces/System/Images/Avatars/Large.png');
	}

	public function content($content = null)
	{
		if ($content === true)
		{
			if ($this->page_type == normal)
			{
				echo $this->content;
			}
			else
			{
				echo $this->content_override;
			}
		}
		else
		{
			$this->content .= $content;
		}
	}
	
	public function interfaceFile()
	{
		if ($this->page_type == normal)
		{
			return $this->path_system . 'Interfaces/' . $this->interface_keys[$this->interface] . '.php';
		}
		else if ($this->page_type == override)
		{
			return $this->path_system . 'Interfaces/' . $this->interface_keys[$this->interface_override] . '.php';
		}
		else if ($this->page_type == login)
		{
			return $this->path_system . 'Interfaces/' . $this->interface_keys[$this->interface_login_window] . '.php';
		}
	}
	
	public function title()
	{
		$title_application = (is_null($this->title_application)) ? $this->tagline : $this->title_application;
		$title = str_replace('[APPLICATION]', $title_application, $this->title_template);
		$title = str_replace('[SITE NAME]', $this->title_system, $title);
		echo "<title>$title</title>";
	}
	
	public function css()
	{
		if (file_exists($this->path_system . 'Style/Base.css'))
		{
			$css[] = $this->path_web . 'Style/Base.css';
		}
		else if (file_exists($this->path_system . 'Style/Base.php'))
		{
			$css[] = $this->path_web . 'Style/Base.php';
		}
		else
		{
			exLog('Interface->css(): No base stylesheet found');
		}
		
		if ($this->page_type == normal)
		{
			$interface = $this->interface_keys[$this->interface];
		}
		else if ($this->page_type == override)
		{
			$interface = $this->interface_keys[$this->interface_override];
		}
		else if ($this->page_type == login)
		{
			$interface = $this->interface_keys[$this->interface_login_window];
		}
		
		if (file_exists($this->path_system . 'Style/' . $interface . '.css'))
		{
			$css[] = $this->path_web . 'Style/' . $interface . '.css';
		}
		else if (file_exists($this->path_system . 'Style/' . $interface . '.php'))
		{
			$css[] = $this->path_web . 'Style/' . $interface . '.php';
		}
		else
		{
			exLog('Interface->css(): No interface stylesheet found');
		}
		
		if (count($css) > 0)
		{
			foreach ($css as $stylesheet)
			{
				echo '<link rel="stylesheet" type="text/css" href="' . $stylesheet . '" />';
			}
		}
		else
		{
			exLog('Interface->css(): No stylesheets found');
		}
	}
	
	public function meta()
	{
		echo '<meta name="description" content="' . $this->description . '" />';
		echo '<meta name="keywords" content="' . $this->keywords . '" />';
	}
	
	public function head()
	{
		$this->title();
		$this->css();
		$this->favicon();
		$this->iphoneicon();
		$this->meta();
		$this->javascript();
	}
	
	public function favicon()
	{
		echo '<link rel="icon" href="' . $this->favicon . '" type="image/x-icon" />';
		echo '<link rel="shortcut icon" href="' . $this->favicon . '" type="image/x-icon" />';
	}
	
	public function iphoneicon()
	{
		echo '<link rel="apple-touch-icon" href="' . $this->iphoneicon . '" />';
	}
	
	public function sidebar()
	{
		if ($args[0] === true)
		{
			$sidebars = $this->sidebars;
		}
		else
		{
			foreach ($this->sidebar as $sidebar)
			{
				if (in_array($sidebar['location'], $args))
				{
					$sidebars[] = $sidebar;
				}
			}
		}
		
		foreach ($sidebars as $sidebar)
		{
			echo $this->sidebarFormat($sidebar);
		}
	}
	
	public function sidebarFormat($sidebar)
	{
		if ($sidebar['type'] == 'menu')
		{
			$content = $this->menu($sidebar['content']);
			if ($sidebar['head'])
			{
				return sprintf($this->template('Sidebar Menu Head'), $sidebar['head'], $content, $sidebar['identifier']);
			}
			else
			{
				return sprintf($this->template('Sidebar Menu None'), $content, $sidebar['identifier']);
			}
		}
		else if ($sidebar['type'] == 'image')
		{
			if ($sidebar['head'])
			{
				return sprintf($this->template('Sidebar Image Head'), $sidebar['head'], $sidebar['image'], $sidebar['identifier']);
			}
			else
			{
				return sprintf($this->template('Sidebar Image None'), $sidebar['image'], $sidebar['identifier']);
			}
		}
		else if ($sidebar['type'] == 'linked-image')
		{
			if($sidebar['head'])
			{
				return sprintf($this->template('Sidebar Linked Image Head'), $sidebar['head'], $sidebar['link'], $sidebar['image'], $sidebar['identifier']);
			}
			else
			{
				return sprintf($this->template('Sidebar Linked Image None'), $sidebar['link'], $sidebar['image'], $sidebar['identifier']);
			}
		}
		else
		{
			if ($sidebar['head'])
			{
				return sprintf($this->template('Sidebar DIV Head'), $sidebar['head'], $sidebar['content'], $sidebar['identifier']);
			}
			else
			{
				return sprintf($this->template('Sidebar DIV None'), $sidebar['content'], $sidebar['identifier']);
			}
		}
	}
	
	public function javascriptAdd($javascript, $head = true)
	{
		if ($head)
		{
			$this->javascript['head']['user'][] .= $javascript;
		}
		else
		{
			$this->javascript['body']['user'][] .= $javascript;
		}
	}
	
	public function javascriptInclude($javascript, $head = true)
	{
		if ($head)
		{
			$this->javascript['head']['include'][] = $javascript;
		}
		else
		{
			$this->javascript['body']['include'][] = $javascript;
		}
	}
	
	public function javascriptOnload($javascript)
	{
		if (!substr($javascript, -1, 1) == ';')
		{
			$javascript .= ';';
		}
		$this->javascript['onload'] .= $javascript;
	}
	
	public function javascript($head = true)
	{
	   $key = ($head) ? 'head' : 'body';
		
		foreach ($this->javascript[$key]['user'] as $javascript)
		{
			echo '<script type="text/javascript">' . $javascript . '</script>';
		}
		
		foreach ($this->javascript[$key]['include'] as $javascript)
		{
			echo '<script type="text/javascript" src="' . $javascript . '"></script>';
		}
	}
	
	public function notification($type, $message)
	{
		$this->notifications[$type][] = $message;
	}
	
	public function notifications($type)
	{
		if ($type == error)
		{
			foreach ($this->notifications[error] as $notification)
			{
				printf($this->template('Notification Error'), $notification);
			}
		}
		else if ($type == notice)
		{
			foreach ($this->notifications[notice] as $notification)
			{
				printf($this->template('Notification Notice'), $notification);
			}
		}
		else
		{
			foreach ($this->notifications[error] as $notification)
			{
				printf($this->template('Notification Error'), $notification);
			}
			foreach ($this->notifications[notice] as $notification)
			{
				printf($this->template('Notification Notice'), $notification);
			}
		}
	}
	
	public function notificationCount($type)
	{
		if($type == error or $type == notification)
		{
			$count = count($this->notifications[error]);
		}
		
		if($type == notice or $type == notification)
		{
			$count += count($this->notifications[notice]);
		}
		
		return $count;
	}
	
	public function error($title = null, $message = null)
	{
		$this->page_type = override;
		$this->content_override = sprintf($this->template('Error', '<div><h1>%s</h1><p>%s</p></div>'), $title, $message);
	}
	
	public function template($identifier, $backup = null)
	{
		crunch($identifier);
		if (is_null($backup))
		{
		   exLog("Interface->template(): No backup provided for $identifier");
	   }
		return (array_key_exists($identifier, $this->templates)) ? $this->templates[$identifier] : $backup ;
	}
	
	public function menu($input, $pre = null, $item = null, $post = null, $pre2 = null)
	{
		if($input == 0 and is_string($input))
		{
			crunch($input);
			$result = query();
			if (is_resource($result))
			{
				if (mysql_num_rows($result) == 1)
				{
					$menu = mysql_fetch_assoc($result);
				}
				else
				{
					exLog("Interface->menu($input 'string'): No menu found");
				}
			}
			else
			{
				exLog("Interface->menu($input 'string'): Result was not mysql value");
			}
		}
		else if (is_array($input))
		{
			$navigation = $input;
		}
		else
		{
			$result = query();
			if (is_resource($result))
			{
				if (mysql_num_rows($result) == 1)
				{
					$menu = mysql_fetch_assoc($result);
				}
				else
				{
					exLog("Interface->menu($input 'string'): No menu found");
				}
			}
			else
			{
				exLog("Interface->menu($input 'string'): Result was not mysql value");
			}
		}
		
		if (!isset($navigation))
		{
			$result = query();
			while ($row = mysql_fetch_assoc($result))
			{
				$navigation[] = $row;
			}
		}
		
		if ($pre == 'return formated')
		{
			unset($pre);
			$pre = $pre2;
			$return = true;
		}
		else if ($pre == 'return unformatted')
		{
			return $navigation;
		}
			
		if (is_null($pre))
		{
			$pre = $this->template('Menu Pre');
		}
		if (is_null($item))
		{
			$item = $this->template('Menu Item');
		}
		if (is_null($post))
		{
			$item = $this->template('Menu Post');
		}
		
		
		$output = $pre;
		foreach ($navigation as $item)
		{
			if ($item['name'] == '[SESSION_PORTAL]' and $item['link'] == '[SESSION_PORTAL]')
			{
				if (System::Authority()->verified)
				{
					$link['name'] = 'Logout';
					$link['link'] = 'ex://Users/Logout';
				}
				else
				{
					$link['name'] = 'Login';
					$link['link'] = 'ex://Users/Login';
				}
			}
			
			if ($item['link'] == '[HOME]')
			{
				$item['link'] = cfRead('Home Link');
			}
			
			if (is_null($item['link']))
			{
				$item['link'] = 'javascript:;';
			}
			
			$link = $this->parseLink($item['link']);
			$current = ($this->isCurrentPage($link)) ? ' class="current" ' : null ;
			$output .= sprintf($item, $link, $link['name'], $current);
		}
		$output .= $post;
		
		if ($return === true)
		{
			return $output;
		}
		else
		{
			echo $output;
		}
	}
	
	public function parseLink($link) // This really needs looked at by someone who knows regex
	{
		if (substr($link, 0, 5) == 'ex://')
		{
			if (preg_match("^ex://([0-9a-zA-Z]+)/([0-9a-zA-Z_./-]+)([0-9a-zA-Z_./-?%&=]+)^", $link, $bits) == 1)
			{
				$path = $bits[2] . $bits[3];
				if ($bits[1] == 'Interface')
				{
					return $this->path_www . $path;
				}
				else if ($bits[1] == 'Resources')
				{
					return www . 'Resources/' . $path;
				}
				else if ($bits[1] == 'Application')
				{
					return www . 'Applications/' . $path;
				}
				else if ($bits[1] == 'Media')
				{
					return www . 'Media/' . $path;
				}
				else if ($bits[1] == 'Root')
				{
					return www . $path;
				}
				else
				{
					return www . $bits[1] . '/' . $path;
				}
			}
			else if (preg_match("^ex://([0-9a-zA-Z]+)([/]*)^", $link, $bits) == 1)
			{
				return www . $bits[1];
			}
		}
		else
		{
			return $link;
		}
	}
	
	public function isCurrentPage($link) // This is not being implimented properly
	{
		return (strtolower($link) == strtolower(whereami()));
	}
	
}

/* Plain Function Accessors (mixed name convetions) */

function add()
{
	$args = func_get_args();
	$string = array_shift($args);
	System::InterfaceHandler()->content(vsprintf($string, $args));
}

function setTitle($title)
{ 
	System::InterfaceHandler()->title_application = $title;
}

function setPageType($type)
{
	System::InterfaceHandler()->page_type = $type;
}

function setMangedUI($switch)
{
	System::InterfaceHandler()->managedUI = $switch;
}

function addSidebar($sidebar)
{
	System::InterfaceHandler()->sidebars[] = $sidebar;
}

function title()
{
	return System::InterfaceHandler()->title();
}

function css()
{
	return System::InterfaceHandler()->css();
}

function head()
{
	return System::InterfaceHandler()->head();
}

function favicon()
{
	return System::InterfaceHandler()->favicon();
}

function iphoneicon()
{
	return System::InterfaceHandler()->iphoneicon();
}

function jsAdd($javascript, $head = true)
{
	System::InterfaceHandler()->javascriptAdd($javascript, $head);
}

function jsInclude($javascript, $head = true)
{
	System::InterfaceHandler()->javascriptInclude($javascript, $head);
}

function jsOnload($javascript)
{
	System::InterfaceHandler()->javascriptOnload($javascript);
}

function javascript()
{
	return System::InterfaceHandler()->javascript($head);
}

function notification($type, $message)
{
	System::InterfaceHandler()->notification($type, $message);
}

function error($title = null, $message = null)
{
	System::InterfaceHandler()->error($title, $message);
}

function template($identifier, $backup = null)
{
	return System::InterfaceHandler()->template($identifier, $backup);
}

function menu($input, $pre = null, $item = null, $post = null, $pre2 = null)
{
	return System::InterfaceHandler()->menu($input, $pre, $item, $post, $pre2);
}

function parseLink($link)
{
	return System::InterfaceHandler()->parseLink($link);
}

function getAvatar($size)
{
	crunch($size);
	if ($size == 'small')
	{
		return System::InterfaceHandler()->avatar_small;
	}
	else if ($size == 'large')
	{
		return System::InterfaceHandler()->avatar_large;
	}
	else
	{
		return System::InterfaceHandler()->avatar_medium;
	}
}