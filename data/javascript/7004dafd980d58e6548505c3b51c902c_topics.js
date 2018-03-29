/**
 * Manage topics
 */
with ({
		FILE   : 'file',
		NAME   : 'name',
		HIDDEN : 'hidden'
	}) {
	PHPFR.topics = (function () {
		var _wait, _option, _widgSysCall, _timer, _topics, _filterField, _topicLabel, _topicsSelect;
		_wait   = 1000;
		_option = document.createElement('OPTION');
		_topics = '';
		var _readTopics, _buildSelect, _addOption, _updateLabel, _fetchFunctions;
		_readTopics = function (obj) {
			if (obj.outputString.indexOf(']}') > -1) {
				clearTimeout(_timer);
				_widgSysCall.cancel();
				_widgSysCall.close();
				_buildSelect(eval(obj.outputString));
				// when topics is all done, we have a free thread to fetch the functions
				PHPFR.functions.init();
			}
		};
		_buildSelect = function (topics) {
			topics = topics || _topics;
			_topicsSelect.options.length = 0;
			_addOption('0', __('--Select a Topic--'));
			$A(topics).each(function (topic) {
				_addOption(topic[FILE], topic[NAME]);
			});
			_updateLabel();
		};
		_addOption = function (val, txt) {
			var option;
			option = _option.cloneNode(false);
			option.value = val;
			option.text  = txt;
			_topicsSelect.appendChild(option);
		};
		_updateLabel = function () {
			var label;
			if (_topicsSelect.options.length > 0) {
				label = _topicsSelect.options[_topicsSelect.selectedIndex].text.truncate(30);
				_topicLabel.update(label);
			}
		};
		_fetchFunctions = function (topic) {
			var callback, filename;
			callback = function (obj) {
				var funcs;
				funcs = eval(obj.outputString);
				PHPFR.functions.display(funcs);
			};
			filename = PHPFR.basePath + '/php_manual/' + PHPFR.languages.lang + '/html/ref.' + topic + '.html';
			WW.system(PHPFR.phpPath + " Assets/php/return_functions.php " + filename, callback);
		};
		return {
			init: function () {
				var cmd;
				_filterField  = $('filter');
				_topicLabel   = $('topic-label');
				_topicsSelect = $('topics');
				// create the topics array file
				cmd = PHPFR.phpPath + ' Assets/php/topics.php';
				_widgSysCall = WW.system(cmd, _readTopics);
				// rinse and repeat every couple of seconds until successful
				clearTimeout(_timer);
				_timer = setTimeout(function () {
					PHPFR.topics.init();
				}, _wait);
			},
			showTopic: function () {
				var topic;
				topic = $F(_topicsSelect);
				if (0 === +topic) {
					PHPFR.functions.filter('');
				} else {
					_fetchFunctions(topic);
				}
				_updateLabel();
				_filterField.value = '';
				_filterField.blur();
				PHPFR.favorites.displayState = HIDDEN;
			},
			reset: function () {
				_topicsSelect.selectedIndex = 0;
				_updateLabel();
			}
		};
	})();
};
