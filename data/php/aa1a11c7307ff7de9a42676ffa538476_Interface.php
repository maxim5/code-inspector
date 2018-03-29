<?php

/* vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4: */

/**
 * Common interface must be implements application wide classes
 *
 * Long description for file (if any)...
 *
 * PHP version 5
 *
 * LICENSE: This source file is subject to version 3.0 of the PHP license
 * that is available through the world-wide-web at the following URI:
 * http://www.php.net/license/3_0.txt.  If you did not receive a copy of
 * the PHP License and are unable to obtain it through the web, please
 * send a note to license@php.net so we can mail you a copy immediately.
 *
 * @category   Application
 * @package    Interface
 * @package    App_Abstract
 * @author     Original Author <olekhy@gmail.com>
 * @copyright  2009-2005 The Webfact GmbH, Copyright (c) 2010 Webfact GmbH (http://www.webfact.de)
 * @license    http://www.php.net/license/3_0.txt  PHP License 3.0
 * @version    SVN: $Id:$
 * @link       http://.../PackageName
 * @see        ()
 * @since      File available since Release 1.2.0
 * @deprecated No
 */

// Place includes, constant defines and $_GLOBAL settings here.

/**
 * This interface must be implemented from any application class
 *
 * Long description for class (if any)...
 *
 * @category   Application
 * @package    Interface
 * @package    App_Abstract
 * @author     Original Author <olekhy@gmail.com>
 * @copyright  2009-2010 The Webfact GmbH, Copyright (c) 2010 Webfact GmbH (http://www.webfact.de)
 * @license    http://www.php.net/license/3_0.txt  PHP License 3.0
 * @version    Release: @package_version@
 * @link       http://
 * @see        ()
 * @since      Class available since Release 1.2.0
 * @deprecated No
 */
interface App_Core_Interface
{
    public function getLog();
    public function isDebug();
    public function getCfg();
}
