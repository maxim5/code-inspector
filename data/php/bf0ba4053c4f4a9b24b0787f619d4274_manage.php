<?php
class Manage extends Controller {

	function __construct()
	{
		parent::Controller();
		$this->dx_auth->check_uri_permissions();
	}
	
	function index()
	{
		$this->load->model('poweruser');
		$data['pendingArtists'] = $this->poweruser->countArtists();
		$data['pendingPictures'] = $this->poweruser->countPictures();
		$data['pendingAlbumPictures'] = $this->poweruser->countAlbumPictures();
		$data['pendingQuestionable'] = $this->poweruser->countQuestionable();
		$data['pendingLyrics'] = $this->poweruser->countLyrics();
		$data['title'] = 'Power User Management Station';
		//$this->output->enable_profiler(TRUE); 
			$this->template->write('title', 'Welcome to Power User Console');
			$this->template->write_view('content', 'manage/index', $data, true);
			$this->template->render();	
	}
	
	function clearCache()
	{
		$memcache = new Memcache;
		$memcache->connect('localhost', 11211) or $memcache = false;
		$memcache->flush();
		$config = Array(
			'protocol' => 'smtp',
			'smtp_host' => 'ssl://smtp.googlemail.com',
			'smtp_port' => 465,
			'smtp_user' => 'notify@unravelthemusic.com',
			'smtp_pass' => 'aoeiii1900',
		);
		$this->load->library('email', $config);
		$this->email->set_newline("\r\n");

		$this->email->from('notify@unravelthemusic.com', 'Unravel Notify');
		$this->email->to('drew.town@gmail.com');

		$this->email->subject('Memcache Cleared');
		$this->email->message('Memcache has been cleared');


		//if (!$this->email->send())
		//	show_error($this->email->print_debugger());
		//else
		//	echo 'Your e-mail has been sent!';		
	}
	
	function artists()
	{
		$data['title'] = 'Power User Console: Manage New Artists';
		$this->output->enable_profiler();
		if($this->uri->segment(3) == 'albums')
		{
			$this->load->model('AlbumModel');
			$songs = unserialize($this->input->post('songs'));
			while (list($key,$value) = each($_POST)){
				if($key != 'final' AND $key != 'artist_id' AND $key != 'submit' and $key != 'inputs' and $key != 'songs')
				{
					$value = unserialize($value);
					$info = explode('**', $value);
			
					if($info[1] == null)
					{
						$date = '0000';
					} else {
					
					$date = $info[1];
					}

	
				}
			}

				$this->load->model('HomeModel');
				$this->db->where('artist_id', $this->input->post('artist_id'));
				$query = $this->db->get('artists');
				if($query->num_rows() == 1)
				{
					$rowArtist = $query->row();
					
				}
				
			redirect('manage/artists');

		}
		
		
		else if($this->uri->segment(3) == 'confirm')
		{
			$this->load->model('usermodel_unravel');
			$this->load->model('AlbumModel');
			$this->load->model('dx_auth/UserModel', 'users');
			$this->load->model('ArtistModel');
			$this->load->model('HomeModel');
			$id = $this->uri->segment(4);
			
			$artistId = $this->ArtistModel->loadById($id);
			if($artistId->num_rows() > 0)
			{
				$row = $artistId->row();
				if($row->verified == 0)
				{
					$data = array('verified' => '1');
					$this->db->update('artists', $data, array('artist_id' => $id));
					
					$username = $row->artist_created_by;
					$this->load->model('dx_auth/UserModel', 'users');
					$user = $this->users->get_user_by_username($username);
					$userRow = $user->row();
					$points = 30;
					$this->usermodel_unravel->givePoints($userRow->username, $points);
					$this->HomeModel->addArtistToFeed($row->artist, $row->artist_created_by);
					if($row->notify == 1)
					{
						$from = 'notify@unravelthemusic.com';
						$to = $userRow->email;
						$subject = 'Your Suggestion has been approved';
						$message = 'You suggestion for ' . $row->artist . ' has just been approved by our admin team and you have been awarded ' . $points . 'points!' . 
						'\r\nYou can view your suggestion at http://www.unravelthemusic.com/artists/view/' . $row->artist_seo_name . 
						'\r\nThank You for your suggestion!\n\nUnravel Admin Team';

						$config = Array(
							'protocol' => 'smtp',
							'smtp_host' => 'ssl://smtp.googlemail.com',
							'smtp_port' => 465,
							'smtp_user' => 'notify@unravelthemusic.com',
							'smtp_pass' => 'aoeiii1900',
						);
						$this->load->library('email', $config);
						$this->email->set_newline("\r\n");

						$this->email->from($from);
						$this->email->to($to);

						$this->email->subject($subject);
						$this->email->message($message);


						if (!$this->email->send())
							show_error($this->email->print_debugger());

					}
					$client = new SoapClient("http://lyricwiki.org/server.php?wsdl");
					
					$artist = $row->artist; 
					$data['artist'] = $row->artist;
					$result = $client->getArtist($artist); 


						foreach($result['albums'] as $album)
						{
							if($album->album != 'Other Songs')
							{
								$data = array(
										'album' => $album->album,
										'artist_id' => $row->artist_id,
										'album_seo_name' => url_title($album->album),
										'locked' => '1'
										);
								if($album->year == '' || $album->year == null)
								{
									$data['release_date'] = '0000';
								} else {
									$data['release_date'] = $album->year;
								}
							} else {
								$data = array(
										'album' => 'Other Songs',
										'release_date' => '0000',
										'artist_id' => $row->artist_id,
										'album_seo_name' => 'Other-Songs',
										'locked' => '0'
										);
							}
									
								$this->db->insert('albums', $data);
								if($album->album == 'Other Songs')
								{
									$query = $this->AlbumModel->loadExtendedById('Other Songs', $row->artist_id);
								} else {
									$query = $this->AlbumModel->loadExtendedById($album->album, $row->artist_id);
								}
								if($query->num_rows() > 0)
								{
									$row2 = $query->row();
									$data = 'insert into `songs` (`album_id`, `artist_id`, `song`, `song_seo_name`) VALUES';
									foreach($album->songs as $song)
									{						
										$data .= ' ("' . $row2->album_id . '", "' . $row->artist_id . '", "' . str_replace('"', '\"', $song) . '", "' . str_replace('"', '\"', url_title($song)) . '"), ';
	
									}
									$data = rtrim($data, ", ");
									$this->db->query(str_replace("'", "\'", $data));
								} else {
									die('no album by this name');
								}
						}
					redirect('/manage/artists');
				} else {
					$this->session->flashdata('flashMessage', 'already verified');
					redirect('manage/artists');
				}

			} else {
				echo('artist does not exist');
			}
		}
		
		else if($this->uri->segment(3) == 'remove')
		{
			$this->load->model('dx_auth/UserModel', 'users');
			$this->load->model('ArtistModel');
			$id = $this->uri->segment(4);
			$data = array('verified' => '-1');
			$this->db->update('artists', $data, array('artist_id' => $id));
			
			$query = $this->ArtistModel->loadById($id);
			
			$row = $query->row();
			
			$username = $row->artist_created_by;
			$user = $this->users->get_user_by_username($username);
			$userRow = $user->row();
			$this->usermodel_unravel->takePoints($userRow->user, 10);	
			if($row->notify == 1)
			{
				
				$from = 'notify@UnravelTheMusic.com';
				$to = $userRow->email;
				$subject = 'Your Suggestion has been denied';
				$message = 'You suggestion for ' . $row->artist . ' has just been denied by our admin team.\r\nIf you feel that this has been a mistake please e-mail us at suggestions@unravelthemusic.com with information about the band including, name, alternate names and spellings, website, wikipedia article, etc.  We will re-review your suggestion as quickly as possible.\r\n\r\nThanks Unravel Admin Team';
				$config = Array(
					'protocol' => 'smtp',
					'smtp_host' => 'ssl://smtp.googlemail.com',
					'smtp_port' => 465,
					'smtp_user' => 'notify@unravelthemusic.com',
					'smtp_pass' => 'aoeiii1900',
				);
				$this->load->library('email', $config);
				$this->email->set_newline("\r\n");

				$this->email->from($from);
				$this->email->to($to);

				$this->email->subject($subject);
				$this->email->message($message);


				if (!$this->email->send())
					show_error($this->email->print_debugger());
			}

			redirect('manage/artists'); 
		}
		else
		{
			$this->db->where('verified', '0');
			$data['query'] = $this->db->get('artists', 1);
			$data['inApi'] = array();
			foreach($data['query']->result() as $artist)
			{
				$wsdl = "http://lyricwiki.org/server.php?wsdl";
						$ctx = stream_context_create(array(
							'http' => array(
								'timeout' => 3
								)
							)
						);
				try {
					if(!@file_get_contents($wsdl, 0, $ctx)) {
						throw new SoapFault('Server', 'No WSDL found at ' . $wsdl);
					}
					$client = new SoapClient($wsdl);					 
					$result = $client->getArtist($artist->artist);

					//echo($result[$artist->artist]->album);
					if(!empty($result['albums']))
					{
						$data['inApi'][] = 'This artist is in the API';
					} else {
						$data['inApi'][] = '<strong>This artist is NOT in the API</strong>';
					}
					
				} catch (SoapFault $e) {
					$data['error'] = 'api is down don\'t add any new artists';
				}			
			
			}
			
			$this->template->write('title', 'Manage pending artist requests');
			$this->template->write_view('content', 'manage/artists', $data, true);
			$this->template->render();	
		}			
		
	}

	function albumpictures()
	{
		if($this->uri->segment(3) != 'confirm' OR $this->uri->segment(3) != 'remove')
		{
			
			$this->db->join('artists', 'artists.artist_id = albums.artist_id');
			$this->db->where('album_picture_verified', '0');
			$this->db->where('album_picture !=', 'null');
			$this->db->orderby('album');
			$data['query'] = $this->db->get('albums', 10);
			$this->template->write('title', 'Power User Console: Manage Album Pictures');
			$this->template->write_view('content', 'manage/albumpictures', $data, true);
			$this->template->render();
		}		
		
		if($this->uri->segment(3) == 'confirm')
		{
			$id = $this->uri->segment(4);
			$this->db->where('album_id', $id);
			$query = $this->db->get('albums', 1);
			$row = $query->row();
			$user = $row->picture_uploader;
			
			
			$data = array('album_picture_verified' => '1');
			$this->db->update('albums', $data, array('album_id' => $id));
			$this->load->model('usermodel_unravel');
			$points = 5;
			$this->usermodel_unravel->givePoints($user, $points);
			redirect('manage/albumpictures');
		}
		
		if($this->uri->segment(3) == 'remove')
		{
			$id = $this->uri->segment(4);
			$data = array('album_picture_verified' => '-1');
			$this->db->update('albums', $data, array('album_id' => $id));
			redirect('manage/albumpictures'); 
		}		
		
	}
	
	
	function artistpictures()
	{
		if($this->uri->segment(3) != 'confirm' OR $this->uri->segment(3) != 'remove')
		{
			$this->db->where('artist_picture_verified', '0');
			$this->db->where('artist_picture !=', 'null');
			$this->db->orderby('artist');
			$data['query'] = $this->db->get('artists', 10);
			$this->template->write('title', 'Power User Console: Manage Pictures');
			$this->template->write_view('content', 'manage/artistpictures', $data, true);
			$this->template->render();				
		}		
		
		if($this->uri->segment(3) == 'confirm')
		{
			$id = $this->uri->segment(4);
			$this->db->where('artist_id', $id);
			$query = $this->db->get('artists', 1);
			$row = $query->row();
			$user = $row->picture_uploader;
			
			$points = 5;
			$this->load->model('usermodel_unravel');
			$this->usermodel_unravel->givePoints($user, $points);
			
			$data = array('artist_picture_verified' => '1');
			$this->db->update('artists', $data, array('artist_id' => $id));
			redirect('manage/artistpictures');
		}
		
		if($this->uri->segment(3) == 'remove')
		{
			$id = $this->uri->segment(4);
			$data = array('artist_picture_verified' => '-1');
			$this->db->update('artists', $data, array('artist_id' => $id));
			redirect('manage/artistpictures'); 
		}		
		
	}
	
	function albums()
	{
		if($this->uri->segment(3) != 'clean' OR $this->uri->segment(3) != 'remove')
		{
			$this->db->join('artists', 'artists.artist_id = albums.artist_id');
			$this->db->where('questionable', '1');
			$data['query'] = $this->db->get('albums', 10);
			$this->template->write('title', 'Power User Console: Manage Questionable Albums');
			$this->template->write_view('content', 'manage/albums', $data, true);
			$this->template->render();					
		}		
		
		if($this->uri->segment(3) == 'clean')
		{
			$id = $this->uri->segment(4);
			$data = array('questionable' => '0', 'locked' => '0');
			$this->db->where('album_id', $id);
			$this->db->update('albums', $data);
			redirect('manage/albums');
		}
		
		if($this->uri->segment(3) == 'remove')
		{
			$id = $this->uri->segment(4);
			
			$this->db->where('album_id', $id);
			$this->db->delete('albums');
			
			$this->db->where('album_id', $id);
			$this->db->delete('songs');
			
			$this->db->where('album_id', $id);
			$this->db->delete('meanings');
			
			$this->db->where('album_id', $id);
			$this->db->delete('meaning_replies');
			
			redirect('manage/albums'); 
		}		
	
	
	}
	function lyric_reports()
	{
		if($this->uri->segment(3) != 'clean')
		{
	        
			$this->db->join('songs', 'songs.song_id = lyrics.song_id');
			$this->db->join('albums', 'albums.album_id = songs.album_id');
			$this->db->join('artists', 'artists.artist_id = songs.artist_id');
			$this->db->where('lyrics.report >', '0');
			$this->db->orderby('artist');
			$data['query'] = $this->db->get('lyrics', 10);
			$this->template->write('title', 'Power User Console: Manage Lyric Reports');
			$this->template->write_view('content', 'manage/lyric_reports', $data, true);
			$this->template->render();	
		}		
		
		if($this->uri->segment(3) == 'clean')
		{
			$id = $this->uri->segment(4);

			$this->db->where('lyrics_id', $id);
			$data = array('report' => '0');
			$this->db->update('lyrics', $data);
			redirect('manage/lyric_reports'); 
		}
	
	}
	
	function lyrics()
	{
		if($this->uri->segment(3) != 'confirm' OR $this->uri->segment(3) != 'remove')
		{
	        
			$this->db->join('songs', 'songs.song_id = lyrics.song_id');
			$this->db->join('albums', 'albums.album_id = songs.album_id');
			$this->db->join('artists', 'artists.artist_id = songs.artist_id');
			$this->db->where('lyrics.verified', '0');
			$this->db->orderby('artist');
			$data['query'] = $this->db->get('lyrics', 10);
			$this->template->write('title', 'Power User Console: Manage Lyrics');
			$this->template->write_view('content', 'manage/lyrics', $data, true);
			$this->template->render();	
		}		
		
		if($this->uri->segment(3) == 'confirm')
		{
			$id = $this->uri->segment(4);
			
			$this->db->join('songs', 'songs.song_id = lyrics.song_id');
			$this->db->join('albums', 'albums.album_id = songs.album_id');
			$this->db->join('artists', 'artists.artist_id = songs.artist_id');
			$this->db->where('lyrics_id', $id);
			$this->db->select('artists.artist, albums.album, albums.release_date, songs.song, lyrics.lyrics, lyrics.submitted_by');
			$query = $this->db->get('lyrics');	
			
			$row = $query->row();

			$data = array('verified' => '1');
			$this->db->update('lyrics', $data, array('lyrics_id' => $id));
			$points = 20;
			$this->load->model('usermodel_unravel');
			$this->usermodel_unravel->givePoints($row->submitted_by, $points);
				$this->load->library('LyricsPrep');
				$lyrics = $this->lyricsprep->removeBreaks($row->lyrics);

				$wsdl = "http://lyricwiki.org/server.php?wsdl";
						$ctx = stream_context_create(array(
							'http' => array(
								'timeout' => 3
								)
							)
						);
				try {
					if(!@file_get_contents($wsdl, 0, $ctx)) {
							throw new SoapFault('Server', 'No WSDL found at ' . $wsdl);
						}
					$client = new SoapClient($wsdl);

					$auth = array();
					$auth['USER'] = 'unravelthemusic';
					$auth['PASSWORD'] = 'aoeiii1900';
					$user=new SoapVar($auth['USER'],SOAP_ENC_OBJECT);
					$password = new SoapVar($auth['PASSWORD'],SOAP_ENC_OBJECT);
					$header = array();
					$header[] = new SoapHeader("http://schemas.xmlsoap.org/soap/envelope/", 'username',$user);
					$header[] = new SoapHeader("http://schemas.xmlsoap.org/soap/envelope/", 'password',$password);

					//$client->__setSoapHeaders($header);

					$onAlbums = array();
					$onAlbums[] = array('artist'=>'', 'album'=>$row->album, 'year' => $row->release_date); 
					$response = $client->postSong(false, $row->artist, $row->song, $lyrics, $onAlbums);
					redirect('manage/lyrics');
				} catch (SoapFault $e) {
					echo 'api is down don\'t add any new artists';
				}	
		}
		
		if($this->uri->segment(3) == 'remove')
		{
			$id = $this->uri->segment(4);
			$this->db->where('lyrics_id', $id);
			$this->db->delete('lyrics');
			redirect('manage/lyrics'); 
		}			
	}
	
	function import()
	{
		$str = '';
		echo("INSERT INTO `artists` (`artist`, `artist_seo_name`, `artist_created_by`) VALUES");
		$handle = fopen("http://www.unravelthemusic.com/import.csv", "r");
		while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
		    $num = count($data);
			$str = '("'. $data[0] . '", "' . url_title($data[0]) . '","Unravel_Bot"),<br />';
			echo(str_replace("'", "\'", $str));

		      
		}
		fclose($handle);
	
	
	}
	
}

?>