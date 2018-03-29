<?php

/**
 * Description of Algorithm
 *
 * @version 0.1.0
 * @author Xedin Unknown <xedin.unknown+xdn@gmail.com>
 */
class Xedin_Crypt_Model_Resource_Hash_Algorithm extends Xedin_Core_Model_Resource_Abstract {
	
	public function load(Xedin_Core_Model_Abstract $object, $id = null, $field = null) {
		if( is_null($id) ) {
			$id = $object->getId();
		}
		
		$object->unsetData();
		
		if( empty($id) ) {
			return $this;
		}
		
		if( in_array($id, hash_algos()) ) {
			$object->setData(array(
				'id'		=>	$id,
				'code'		=>	$id,
				'label'		=>	strtoupper($id)
			));
		}
		
		return $this;
	}
}