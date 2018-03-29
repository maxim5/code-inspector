/**
 * Manage display frame
 */
with ({
		VIEW    : 'view',
		PHPINFO : 'phpinfo'
	}) {
	PHPFR.pages = (function () {
		var _templates, _linkReplace, _elements, _widgSysCall, _wait, _timer, _page, _hash;
		_wait = 1000;
		_templates = {
			php  : new Template('php_manual/#{lang}/html/#{page}'),
			html : new Template('html/#{page}')
		};
		_linkReplace = '<a onclick="PHPFR.ui.followLink(this.href);return false;"';
		_elements = {
			viewport  : undefined,
			favorites : undefined
		};
		var _displayPage;
		_displayPage = function (obj) {
			var html, spans;
			if (obj.outputString.indexOf('<!--EOF-->') > -1) {
				clearTimeout(_timer);
				_widgSysCall.cancel();
				_widgSysCall.close();
				html = obj.outputString.replace(PHPFR.regexs.link, _linkReplace);
				_elements.viewport.update(html);
				PHPFR.favorites.updateUI(_page);
				PHPFR.ui.viewFrame.open();
				if (_hash) {
					PHPFR.ui.showTab(_hash);
				} else {
					// if there is nav on the page, show the first tab
					spans = $$('ul.phpfr-nav span');
					if (spans.length > 0) {
						spans[0].onclick();
					}
				}
				PHPFR.history.addItem(_page);
				PHPFR.pages.nowShowing = _page;
				PHPFR.util.stripe();
				PHPFR.functions.hilite();
				PHPFR.ui.scrollBars.refresh(VIEW);
				PHPFR.ui.scrollBars.scrollToTop(VIEW);
			}
		};
		return {
			nowShowing: undefined,
			init: function () {
				_elements.viewport  = $('viewport-container');
				_elements.favorites = $('favorites-control');
				// set default page
				PHPFR.ui.followLink('phpfr-help.html#welcome');
			},
			/**
			 * Display a local HTML page, either from php_manual or html directories
			 * @param string page 
			 */
			display: function (page) {
				var pieces;
				pieces = page.split('#');
				_page = pieces[0].dashed();
				_hash = ('undefined' !== typeof pieces[1])? pieces[1]: false;
				if (PHPINFO === _page) {
					_widgSysCall = WW.system(PHPFR.phpPath + ' Assets/php/phpinfo.php', _displayPage);
				} else {
					if (PHPFR.regexs.html.test(_page)) {
						path = _templates.html.evaluate({page: _page});
					} else if (PHPFR.regexs.file.test(_page)) {
						_page = (_page.split('/'))[(_page.split('/')).length - 1];
						path = _templates.php.evaluate({lang: PHPFR.languages.lang, page: _page});
					} else { // e.g., ('function.strstr.html'.split('.')).length === 3
						path = _templates.php.evaluate({lang: PHPFR.languages.lang, page: _page});
					}
					// fetch HTML
					if ('tw' === PHPFR.languages.lang || 'hk' === PHPFR.languages.lang) {
						_widgSysCall = WW.system(PHPFR.phpPath + " Assets/php/return_html.php '" + path + "' | iconv -f BIG5 -t UTF-8", _displayPage);
					} else if ('ro' === PHPFR.languages.lang) {
						_widgSysCall = WW.system(PHPFR.phpPath + " Assets/php/return_html.php '" + path + "' | iconv -f ISO-8859-2 -t UTF-8", _displayPage);
					} else {
						_widgSysCall = WW.system(PHPFR.phpPath + " Assets/php/return_html.php '" + path + "'", _displayPage);
					}
				}

				clearTimeout(_timer);
				_timer = setTimeout(function () {
					PHPFR.pages.display(page);
				}, _wait);
			},
			showPHPInfo: function () {
				this.display(PHPINFO);
			}
		};
	})();
};
