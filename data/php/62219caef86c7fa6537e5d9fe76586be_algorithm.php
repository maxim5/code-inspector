<?php
if (! defined ( 'BASEPATH' ))
	exit ( 'No direct script access allowed' );
/**
 */
class Algorithm extends CI_Controller {
	function __construct() {
		parent::__construct ();
		$this->load->helper ( 'script' );
	}
	public function test_page() {
		$this->load->helper ( 'form' );
		$this->load->view ( 'algorithm/test_form' );
	}
	public function query_item_defaults_by_barcode() {
		$barcode = $this->input->post ( 'barcode' );
		if (empty ( $barcode )) {
			$data ['result'] = FAILURE;
			$data ['message'] = 'Internal Error: Barcode is empty.';
		} else {
			$input = array (
					$barcode 
			);
			$cmd = SCRIPT_PATH . 'query_item_defaults_by_barcode.py';
			log_message ( 'debug', $cmd );
			$result = shell_exec ( 'python ' . $cmd . ' ' . escapeshellarg ( json_encode ( $input ) ) );
			$result = $this->get_real_result ( $result );
			if (empty ( $result )) {
				$data ['result'] = FAILURE;
				$data ['message'] = 'Internal Error: No result found for barcode.';
			} else {
				$data ['result'] = SUCCESS;
				$data ['data'] = array (
						'item_defaults' => $result 
				);
			}
		}
		$this->output->set_content_type ( 'application/json' )->set_output ( json_encode ( $data ) );
	}
	public function query_item_prices_by_barcode() {
		$barcode = $this->input->post ( 'barcode' );
		if (empty ( $barcode )) {
			$data ['result'] = FAILURE;
			$data ['message'] = 'Internal Error: The barcode is empty.';
		} else {
			$input = array (
					$barcode,
					'' 
			);
			$cmd = SCRIPT_PATH . 'query_item_prices.py';
			log_message ( 'debug', $cmd );
			$result = shell_exec ( 'python ' . $cmd . ' ' . escapeshellarg ( json_encode ( $input ) ) );
			$result = $this->get_real_result ( $result );
			if (empty ( $result )) {
				$data ['result'] = FAILURE;
				$data ['message'] = 'Internal Error: No result found for barcode.';
			} else {
				$data ['result'] = SUCCESS;
				$data ['data'] = array (
						'item_prices' => $this->format_price_dict ( $result ) 
				);
			}
		}
		$this->output->set_content_type ( 'application/json' )->set_output ( json_encode ( $data ) );
	}
	public function query_categories_by_title() {
		$title = $this->input->post ( 'title' );
		if (empty ( $title )) {
			show_error ( 'Internal Error: Title is empty.' );
			return;
		} else {
			$categories = call_script ( 'query_categories_by_title.py', array (
					$title 
			) );
			
			// insert the algo session in the table
			$this->db->insert ( 'algo_history', array (
					'title' => $title,
					'categories' => json_encode ( $categories ) 
			) );
			$algo_session_id = $this->db->insert_id ();
			
			$data ['title'] = 'Category List';
			$data ['query_title'] = $title;
			$data ['categories'] = $categories;
			$data ['algo_session_id'] = $algo_session_id;
			
			$this->load->helper ( 'form' );
			$this->load->view ( 'templates/header_app', $data );
			$this->load->view ( 'algorithm/categories', $data );
			$this->load->view ( 'templates/footer_app' );
		}
		// $this->output->set_content_type ( 'application/json' )->set_output ( json_encode ( $data ) );
	}
	public function query_similar_itmes() {
		$title = $this->input->post ( 'title' );
		$catNum = $this->input->post ( 'catNum' );
		if (empty ( $title ) || empty ( $catNum )) {
			show_error ( 'Internal Error: Title or category number is empty.' );
			return;
		} else {
			$items = call_script ( 'query_similar_items.py', array (
					$title,
					$catNum 
			) );
			
			// update the selected category num into the table
			$algo_session_id = $this->input->post ( 'algo_session_id' );
			if ($algo_session_id) {
				$this->db->where ( 'algo_session_id', $algo_session_id );
				$this->db->update ( 'algo_history', array (
						'selected_category' => $catNum 
				) );
			}
			
			$this->load->helper ( 'form' );
			$this->load->helper ( 'html' );
			
			$data ['query_title'] = $title;
			$data ['catNum'] = $catNum;
			
			$this->load->view ( 'templates/header_app', $data );
			if (count ( $items ) == 1) {
				$data ['similarItemUrl'] = $items [0] ['url'];
				$this->load->view ( 'algorithm/jump_final', $data );
			} else {
				$data ['title'] = 'Step 2/2: Item List';
				$data ['items'] = $items;
				$this->load->view ( 'algorithm/items', $data );
			}
			$this->load->view ( 'templates/footer_app' );
		}
	}
	public function query_item_info_by_similar_item() {
		$title = $this->input->post ( 'title' );
		$catNum = $this->input->post ( 'catNum' );
		$similarItemUrl = $this->input->post ( 'similarItemUrl' );
		log_message ( 'debug', 'query_item_info_by_similar_item: $title = ' . $title );
		log_message ( 'debug', 'query_item_info_by_similar_item: $catNum = ' . $catNum );
		log_message ( 'debug', 'query_item_info_by_similar_item: $similarItemUrl = ' . $similarItemUrl );
		
		if (empty ( $title ) || empty ( $catNum ) || empty ( $similarItemUrl )) {
			$data ['result'] = FAILURE;
			$data ['message'] = 'Internal Error: Title, category number or item url is empty.';
		} else {
			$input = array (
					$title,
					$catNum,
					$similarItemUrl 
			);
			$cmd = SCRIPT_PATH . 'query_item_info_by_similar_item.py';
			log_message ( 'debug', $cmd );
			$result = shell_exec ( 'python ' . $cmd . ' ' . escapeshellarg ( json_encode ( $input ) ) );
			$result = $this->get_real_result ( $result );
			if (empty ( $result )) {
				$data ['result'] = FAILURE;
				$data ['message'] = 'Internal Error: No result found for title & catNum.';
			} else {
				$result ['title'] = $title;
				$result ['catNum'] = $catNum;
				$result ['similarItemUrl'] = $similarItemUrl;
				$result ['priceGroup'] = $this->format_price_dict ( $result ['priceGroup'] );
				$data ['result'] = SUCCESS;
				$data ['data'] = array (
						'item_info' => $result 
				);
			}
		}
		$this->output->set_content_type ( 'application/json' )->set_output ( json_encode ( $data ) );
	}
	private function get_real_result($result) {
		log_message ( 'debug', 'row result:' . $result );
		
		$pos = stripos ( $result, PYTHON_PLACEHOLD );
		if ($pos) {
			$result = substr ( $result, $pos + strlen ( PYTHON_PLACEHOLD ) );
			$result = json_decode ( $result, true );
		}
		return $result;
	}
	private function format_price_dict($priceGroup) {
		foreach ( $priceGroup as $conditionKy => $prices ) {
			$prices ['marketPriceMin'] = strval ( $prices ['marketPriceMin'] );
			$prices ['marketPriceMax'] = strval ( $prices ['marketPriceMax'] );
			$prices ['expectedPrice'] = strval ( $prices ['expectedPrice'] );
			$priceGroup [$conditionKy] = $prices;
		}
		return $priceGroup;
	}
}

?>
