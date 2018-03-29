/**
 * Manage user favorites
 */
with ({
		PREFKEY   : 'favs',
		ADD       : 'add',
		REMOVE    : 'remove',
		SHOWING   : 'showing',
		HIDDEN    : 'hidden',
		DELIMITER : ','
	}) {
	PHPFR.favorites = (function () {
		var _srcs, _titles, _prefKey, _favs, _favsControl;
		_srcs = {
			add    : 'url(file://' + PHPFR.basePath + '/Assets/imgs/famfamfam/heart_add.png)',
			remove : 'url(file://' + PHPFR.basePath + '/Assets/imgs/famfamfam/heart_delete.png)'
		};
		_titles = {
			add    : __('Click to add this page to your favorites'),
			remove : __('Click to remove this page from your favorites')
		};
		var _add, _remove, _save, _retrieve;
		// add a fav to the list
		_addFav = function (page) {
			_favs.push(page);
			_favs = _favs.uniq();
			_updateUI(REMOVE);
			_saveFavs();
		};
		// remove a fav from the list
		_removeFav = function (page) {
			_favs = _favs.without(page);
			_updateUI(ADD);
			_saveFavs();
		};
		// save favs to prefs
		_saveFavs = function () {
			PHPFR.prefs.set(PREFKEY, _favs.join(DELIMITER));
		};
		// get favs from prefs
		_getFavs = function () {
			var favsPref;
			favsPref = PHPFR.prefs.get(PREFKEY);
			_favs = $A((favsPref)? favsPref.split(DELIMITER) : []);
			return _favs;
		};
		// update the UI to reflect state
		_updateUI = function (state, page) {
			if (PHPFR.regexs.func.test(page)) {
				_favsControl.setStyle({'background-image': _srcs[state]});
				_favsControl.title = _titles[state];
				_favsControl.onclick = function () {
					PHPFR.favorites.toggle(page);
				};
				if (SHOWING === PHPFR.favorites.displayState) {
					PHPFR.favorites.show();
				}
				_favsControl.show();
			} else {
				_favsControl.hide();
			}
		};
		return {
			displayState: undefined,
			init: function () {
				this.displayState = HIDDEN;
				_favs = _getFavs();
				_favsControl = $('favorites-control');
			},
			show: function () {
				var favs;
				favs = $A([]);
				_favs.each(function (fav) {
					favs[favs.length] = (fav.match(PHPFR.regexs.func))[1];
				});
				PHPFR.functions.display(favs);
				this.displayState = SHOWING;
				PHPFR.topics.reset();
			},
			updateUI: function (page) {
				var state;
				state = (this.isFavorite(page))? REMOVE : ADD;
				_updateUI(state, page);
			},
			/**
			 * Toggle whether a page is in the favorites list
			 * @param string page Filename for a page (e.g., function.abs.html)
			 */
			toggle: function (page) {
				if (this.isFavorite(page)) {
					_removeFav(page);
					_updateUI(ADD, page);
				} else {
					_addFav(page);
					_updateUI(REMOVE, page);
				}
			},
			isFavorite: function (page) {
				var favs;
				favs = _getFavs();
				return (favs.indexOf(page) > -1);
			}
		};
	})();
};
