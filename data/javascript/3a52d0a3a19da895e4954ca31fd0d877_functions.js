/**
 * Manage functions list
 */
with ({
		HIDDEN   : 'hidden',
		SELECTED : 'selected'
	}) {
	PHPFR.functions = (function () {
		var _wait, _template, _timer, _widgSysCall, _funcs, _funcsHTML, _functionList, _functionCount, _funcDivs;
		_wait = 1000;
		_templates = {
			func   : new Template('<div id="#{func}" title="#{func}" class="function #{className}" onclick="PHPFR.functions.show(\'#{func}\');" ondblclick="PHPFR.functions.gotoOnlineDoc(\'#{func}\');">#{func}<\/div>'),
			local  : new Template('function.#{func}.html'),
			online : new Template('http://www.php.net/manual/function.#{func}.php')
		};
		var _readFunctions, _buildList, _updateCount, _showAll, _hilite;
		_readFunctions = function (obj) {
			if (obj.outputString.indexOf(']}') > -1) {
				clearTimeout(_timer);
				_widgSysCall.cancel();
				_widgSysCall.close();
				_funcs = eval(obj.outputString);
				_buildList(_funcs);
			}
		};
		/**
		 * Build and display a list of functions
		 * @param array funcs Array of function names
		 */
		_buildList = function (funcs) {
			var html, counter, className;
			funcs  = funcs || _funcs;
			funcs.sort(function(a, b) {
				a = a.toUpperCase();
				b = b.toUpperCase();
				if (a > b) return  1;
				if (a < b) return -1;
				return 0;
			});
			html    = [];
			counter = 0;
			funcs.each(function (func) {
				++counter;
				className = (0 === counter % 2)? 'even' : 'odd';
				html[html.length] = _templates.func.evaluate({func: func.underscored(), className: className});
			});
			if ('undefined' === typeof _funcsHTML) {
				_funcsHTML = html.join('');
			}
			_functionList.update(html.join(''));
			_storeFuncDivs();
			PHPFR.functions.hilite();
			PHPFR.ui.scrollBars.refresh('func');
			PHPFR.ui.scrollBars.scrollToTop('func');
			_updateCount();
		};
		_updateCount = function (showing) {
			showing = showing || _funcDivs.length;
			_functionCount.update(showing);
		};
		// short cut for showing all functions
		_showAll = function () {
			_functionList.update(_funcsHTML);
			PHPFR.ui.scrollBars.refresh('func');
			PHPFR.ui.scrollBars.scrollToTop('func');
			_updateCount(_funcs.length);
		};
		// store the current list of divs for fast access later
		_storeFuncDivs = function () {
			_funcDivs = $$('div.function');
		};
		return {
			init: function () {
				var cmd;
				_functionList  = $('function-list');
				_functionCount = $('function-list-count');
				// fetch full list of function names
				cmd = PHPFR.phpPath + " 'Assets/php/functions.php'";
				_widgSysCall = WW.system(cmd, _readFunctions);
				// rinse and repeat every few seconds until successful
				clearTimeout(_timer);
				_timer = setTimeout(function () {
					_widgSysCall.cancel();
					_widgSysCall.close();
					PHPFR.functions.init();
					_wait = _wait * 2;
					if (_wait > 8000) _wait = 8000;
				}, _wait);
			},
			// wrapper to _buildList
			display: function (funcs) {
				funcs = funcs || [];
				_buildList(funcs);
			},
			/**
			 * Filter the function list
			 * @param string pattern User-entered input, used as a regular expression
			 */
			filter: function (pattern) {
				var funcs, regxp;
				PHPFR.topics.reset();
				PHPFR.favorites.displayState = HIDDEN;
				if ('' === pattern) {
					_showAll();
				} else {
					funcs = $A([]);
					regxp = new RegExp(pattern.dashed(), 'i');
					// EXPENSIVE OPERATION
					_funcs.each(function (func) {
						if (regxp.test(func)) funcs[funcs.length] = func;
					});
					_buildList(funcs);
				}
			},
			/**
			 * Show a function in the view frame
			 * @param string func Function name
			 */
			show: function (func) {
				var page;
				page = _templates.local.evaluate({func: func});
				PHPFR.ui.followLink(page);
			},
			// hilite the currently showing function
			hilite: function () {
				var func, hilite;
				// remove the class name from every hilited function div
				$$('div.selected').each(function (div) {
					div.removeClassName(SELECTED);
				});
				func = ('undefined' === typeof PHPFR.pages.nowShowing)? [] : PHPFR.pages.nowShowing.match(PHPFR.regexs.func);
				if (func && func.length > 1) {
					hilite = $(func[1].underscored());
					if (null !== hilite) hilite.addClassName(SELECTED);
				}
			},
			/**
			 * Take the user to PHP.net at the specified function
			 * @param string func Function name
			 */
			gotoOnlineDoc: function (func) {
				var url;
				url = _templates.online.evaluate({func: func.dashed()});
				PHPFR.util.gotoURL(url);
			}
		};
	})();
};