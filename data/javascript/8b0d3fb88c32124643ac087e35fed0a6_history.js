/**
 * Manage display frame history
 */
with ({
		ACTIVE   : 'active',
		INACTIVE : 'inactive'
	}) {
	PHPFR.history = (function () {
		var _stack, _position, _limit, _arrows;
		_stack    = $A([]);
		_position = 0;
		_limit    = 256;
		_arrows = {
			elements: {
				back    : undefined,
				forward : undefined
			},
			srcs: {
				back: {
					active   : 'url(file://' + PHPFR.basePath + '/Assets/imgs/button-back-enabled.png)',
					inactive : 'url(file://' + PHPFR.basePath + '/Assets/imgs/button-back-disabled.png)'
				},
				forward: {
					active   : 'url(file://' + PHPFR.basePath + '/Assets/imgs/button-forward-enabled.png)',
					inactive : 'url(file://' + PHPFR.basePath + '/Assets/imgs/button-forward-disabled.png)'
				}
			}
		};
		var _updateArrows;
		_updateArrows = function () {
			var back, forward;
			back    = INACTIVE;
			forward = INACTIVE;
			if (_stack.length > 1) {
				back    = ACTIVE;
				forward = ACTIVE;
				if (0 === _position)                 back    = INACTIVE;
				if (_stack.length - 1 === _position) forward = INACTIVE;
			}
			_arrows.elements.back.setStyle({'background-image': _arrows.srcs.back[back]});
			_arrows.elements.forward.setStyle({'background-image': _arrows.srcs.forward[forward]});
		};
		return {
			init: function () {
				_arrows.elements.back    = $('history-back');
				_arrows.elements.forward = $('history-forward');
				_arrows.elements.back.onclick    = this.goBack;
				_arrows.elements.forward.onclick = this.goForward;
				_updateArrows();
			},
			/**
			 * Add an item to the browsing history (limited to 256 items)
			 * @param string page Filename for a page (e.g., function.opendir.html or ref.bbcode.html)
			 */
			addItem: function (page) {
				if (page !== _stack[_position]) {
					_stack.push(page);
					_position = _stack.length - 1;
				}
				while (_stack.length > _limit) {
					_stack.shift();
				};
				_updateArrows();
			},
			goForward: function () {
				++_position;
				if (_position > _stack.length - 1) _position = _stack.length - 1;
				if ('undefined' !== typeof _stack[_position]) {
					PHPFR.ui.followLink(_stack[_position]);
				}
				_updateArrows();
			},
			goBack: function () {
				--_position;
				if (_position < 0) _position = 0;
				if ('undefined' !== typeof _stack[_position]) {
					PHPFR.ui.followLink(_stack[_position]);
				}
				_updateArrows();
			}
		};
	})();
};
