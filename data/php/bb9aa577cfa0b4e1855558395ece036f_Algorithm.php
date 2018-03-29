<?php

/**
 * Description of Algorithm
 *
 * @version 0.1.0
 * @author Xedin Unknown <xedin.unknown+xdn@gmail.com>
 */
class Xedin_Crypt_Model_Hash_Algorithm extends Xedin_Crypt_Model_Abstract {
	
	protected function _construct() {
		parent::_construct();
		$this->_init('crypt/hash_algorithm');
	}
	
	public function save() {
		return $this;
	}
}