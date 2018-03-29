#!/usr/bin/env python2
 
import sys
from struct import *
import threading
import random
import time
import urllib2
import json
import getopt

HOST = '127.0.0.1'
PORT = 666
APIV = 1

ERROR_UNKNOWN                =  -1

ERROR_SUCCESS                =   1

ERROR_WRONG_ARGUMENTS_COUNT  = 1001
ERROR_WRONG_API_VERSION      = 1002
ERROR_UNKNOWN_COMMAND        = 1003
ERROR_INVALID_LINK_TYPE      = 1004
ERROR_NOT_LOGGED_IN          = 1005
ERROR_SEARCH_FAILED          = 1006
ERROR_BROWSE_FAILED          = 1007
ERROR_RESOURCE_NOT_FOUND     = 1008
ERROR_RESOURCE_UNAVAILABLE   = 1009

ERROR_LOGIN_CLIENT_TOO_OLD            = 2001
ERROR_LOGIN_UNABLE_TO_CONTACT_SERVER  = 2002
ERROR_LOGIN_BAD_USERNAME_OR_PASSWORD  = 2003
ERROR_LOGIN_USER_BANNED               = 2004
ERROR_LOGIN_USER_NEEDS_PREMIUM        = 2005
ERROR_LOGIN_OTHER_TRANSIENT           = 2006
ERROR_LOGIN_OTHER_PERMANENT           = 2007

class MainThread ( threading.Thread ):
	def validate(self, jsn, expected, url):
		if expected == ERROR_UNKNOWN:
			return True
		if 'error' not in jsn:
			print "Validation failed: error is not in json: %s, url: %s" % (json.dumps(jsn), url) 
			return False
		elif 'code' not in jsn['error']:
			print "Validation failed: error.code is not in json: %s, url: %s" % (json.dumps(jsn), url) 
			return False
		elif jsn['error']['code'] != expected:
			print "Validation failed: code: %d != expected: %d. json: %s, url: %s" % (jsn['error']['code'], expected, json.dumps(jsn), url) 
			return False
		return True
			
	def command(self, cmd, expected, apiv=APIV):
		url = 'http://%s:%d/%d/%s' % (HOST, PORT, apiv, urllib2.quote(cmd))
		print "%s..." % url
		data = urllib2.urlopen(url).read()
		try:
			jsn = json.loads(data)
		except:
			print "Invalid json: %s" % data
			exit(1)
		if self.validate(jsn, expected, url) is not True:
			exit(1)
		return jsn

	def suite_login(self, username, password):
		print "Suite login"
		# Rest state, logout
		self.command('logout', ERROR_UNKNOWN)
		
		# API version: try with bad api version.
		self.command('login', ERROR_WRONG_API_VERSION, 9999)
		
		# Login: Wrong number of arguments
		self.command('login', ERROR_WRONG_ARGUMENTS_COUNT)
		
		# Login: Try to perform command without login.
		self.command('status', ERROR_NOT_LOGGED_IN)
				
		# Login: Try to login with bad credentials.
		self.command('login/bad/creds', ERROR_LOGIN_BAD_USERNAME_OR_PASSWORD)

		# Login: Try to login with correct credentials.
		self.command('login/%s/%s' % (username, password), ERROR_SUCCESS)
		
		# Logout: Try to logut.
		self.command('logout', ERROR_SUCCESS)

		# Logout: Try to perform command without login.
		self.command('status', ERROR_NOT_LOGGED_IN)
		
		# Login: Try to login with correct credentials again.
		self.command('login/%s/%s' % (username, password), ERROR_SUCCESS)
		
		# Login: Get status and verify username.
		jsn = self.command('status', ERROR_SUCCESS)
		if 'username' not in jsn or jsn['username'] != username:
			print "Validation failed: Username in status does not equal expected username, json: %s" % json.dumps(jsn)
			exit(1)
			
		# Turn off repeat().
		time.sleep(1)
		jsn = self.command('repeat/0', ERROR_SUCCESS)
		if jsn['repeat'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Turn off shuffle().
		time.sleep(1)
		jsn = self.command('shuffle/0', ERROR_SUCCESS)
		if jsn['shuffle'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)

		# Clear queue.
		self.command('queue/clear', ERROR_SUCCESS)

	def suite_browse_track(self):
		print "Suite browse track"
		# Browse non-existing track.
		uri = 'spotify:track:0E2EPL6XnSe3gqjfLuJECe'
		self.command(uri, ERROR_RESOURCE_NOT_FOUND)
	
		# Browse track.
		uri = 'spotify:track:0E2EPL6XnSe3gqjfLuJECb'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'I Wish' or jsn['artist']['name'] != 'Skee-Lo' or jsn['album']['name'] != 'Hip Hop - The Classics CD':
			print 'Validation failed: The track does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Browse different track.
		uri = 'spotify:track:3lA3Pm88kZiD7F4X0cXeI4'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'The Show Must Go On' or jsn['artist']['name'] != 'Queen' or jsn['album']['name'] != 'Innuendo':
			print 'Validation failed: The track does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Browse first track again.
		uri = 'spotify:track:0E2EPL6XnSe3gqjfLuJECb'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'I Wish' or jsn['artist']['name'] != 'Skee-Lo' or jsn['album']['name'] != 'Hip Hop - The Classics CD':
			print 'Validation failed: The track does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
					
		# And again.
		uri = 'spotify:track:0E2EPL6XnSe3gqjfLuJECb'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'I Wish' or jsn['artist']['name'] != 'Skee-Lo' or jsn['album']['name'] != 'Hip Hop - The Classics CD':
			print 'Validation failed: The track does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)

			
	def suite_browse_album(self):
		print "Suite browse album"
		# Browse non-existing album.
		uri = 'spotify:album:1vKeo7Od3sQkPRnLGuqpQj'
		self.command(uri, ERROR_RESOURCE_NOT_FOUND)
	
		# Browse album.
		uri = 'spotify:album:1vKeo7Od3sQkPRnLGuqpQh'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Coffee House Blend' or len(jsn['tracks']) != 35 or jsn['tracks'][23]['name'] != 'Amazing Grace':
			print 'Validation failed: The album does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Browse different album.
		uri = 'spotify:album:2v7B3ztXxn6nCFBIga1P6p'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'I Love You, Man' or len(jsn['tracks']) != 12 or jsn['tracks'][5]['name'] != 'Bullets':
			print 'Validation failed: The album does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Browse first album again.
		uri = 'spotify:album:1vKeo7Od3sQkPRnLGuqpQh'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Coffee House Blend' or len(jsn['tracks']) != 35 or jsn['tracks'][23]['name'] != 'Amazing Grace':
			print 'Validation failed: The album does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# And again.
		uri = 'spotify:album:1vKeo7Od3sQkPRnLGuqpQh'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Coffee House Blend' or len(jsn['tracks']) != 35 or jsn['tracks'][23]['name'] != 'Amazing Grace':
			print 'Validation failed: The album does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
	def suite_browse_artist(self):
		print "Suite browse artist"
		# Browse non-existing artist.
		uri = 'spotify:artist:0UizQYD2qphg8nfPOdeOde'
		self.command(uri, ERROR_RESOURCE_NOT_FOUND)	
	
		# Browse artist.
		uri = 'spotify:artist:0UizQYD2qphg8nfPOdeOdX'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Songs To Wear Pants To' or len(jsn['similar_artists']) != 20 or jsn['albums'][0]['name'] != 'Corduroys, With Tassles' or len(jsn['tracks']) == 0:
			print 'Validation failed: The artist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Browse same artist again.
		uri = 'spotify:artist:0UizQYD2qphg8nfPOdeOdX'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Songs To Wear Pants To' or len(jsn['similar_artists']) != 20 or jsn['albums'][0]['name'] != 'Corduroys, With Tassles' or len(jsn['tracks']) == 0:
			print 'Validation failed: The artist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Browse different artist.
		uri = 'spotify:artist:2UZMlIwnkgAEDBsw1Rejkn'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Tom Petty' or len(jsn['similar_artists']) != 20 or jsn['albums'][0]['name'] != 'Highway Companion' or len(jsn['tracks']) == 0:
			print 'Validation failed: The artist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Browse artist and return no tracks.
		uri = 'spotify:artist:2BGRfQgtzikz1pzAD0kaEn'
		jsn = self.command('%s/no_tracks' % uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Elvis Costello' or len(jsn['similar_artists']) != 20 or jsn['albums'][0]['name'] != 'The Return Of The Spectacular Spinning Songbook' or len(jsn['tracks']) != 0:
			print 'Validation failed: The artist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Browse artist and return no album and no tracks.
		uri = 'spotify:artist:43ZHCT0cAZBISjO8DG9PnE'
		jsn = self.command('%s/no_albums_or_tracks' % uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Elvis Presley' or len(jsn['similar_artists']) != 20 or len(jsn['albums']) != 0 or len(jsn['tracks']) != 0:
			print 'Validation failed: The artist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
	def suite_browse_playlist(self):
		print "Suite browse playlist"
		# Browse non-existing playlist.
		#uri = 'spotify:user:johboh:playlist:2caXAPBrC9j12JNIYb2ooe'
		#self.command(uri, ERROR_RESOURCE_NOT_FOUND)

		# Regular playlist browse
		uri = 'spotify:user:digster.se:playlist:7LRqIcQTHUL4TBedADnYy2'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Digster HITS' or len(jsn['tracks']) != 40:
			print 'Validation failed: The playlist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Browse only header, no tracks.
		uri = 'spotify:user:sonymusicuk:playlist:2V7mhhIKXUSDSdek5rYHLb'
		jsn = self.command('%s/no_tracks' % uri, ERROR_SUCCESS)
		if jsn['uri'] != uri or jsn['name'] != 'Merry Christmas!' or len(jsn['tracks']) != 0:
			print 'Validation failed: The playlist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
	def suite_browse_playlists(self):
		print "Suite browse playlists"
		# Browse rootlist
		jsn = self.command('playlists', ERROR_SUCCESS)
		if len(jsn['playlists']) == 0:
			print 'Validation failed: The playlist does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)


	def suite_search(self):
		print "Suite search"
		# Standard search.
		jsn = self.command('search/grodan', ERROR_SUCCESS)
		if jsn['query'] != 'grodan' or jsn['did_you_mean'] != 'groban' or jsn['tracks'][0]['name'] != 'Grodan Boll': 
			print 'Validation failed: The search does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Search with arguments
		jsn = self.command('search/Bieber/8/16/32', ERROR_SUCCESS)
		if jsn['query'] != 'Bieber' or jsn['did_you_mean'] != '' or len(jsn['tracks']) != 8 or len(jsn['albums']) != 16 or len(jsn['artists']) != 32: 
			print 'Validation failed: The search does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Search using uri.
		uri = 'spotify:search:hoste'
		jsn = self.command(uri, ERROR_SUCCESS)
		if jsn['query'] != 'hoste' or jsn['did_you_mean'] != '' or jsn['tracks'][0]['uri'] != 'spotify:track:0OkR3K7rmUmUPOZGTPvRra': 
			print 'Validation failed: The search does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Search using uri with arguments
		uri = 'spotify:search:alabama'
		jsn = self.command('%s/0/0/8' % uri, ERROR_SUCCESS)
		if jsn['query'] != 'alabama' or jsn['did_you_mean'] != '' or len(jsn['tracks']) != 0 or len(jsn['albums']) != 0 or len(jsn['artists']) != 8: 
			print 'Validation failed: The search does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)

	def suite_playback_track(self):
		print "Suite playback track"
		# Try to play artist, which should not work.
		uri = 'spotify:artist:1vCWHaC5f2uS3yhpwWbIA6'
		self.command('%s/play' % uri, ERROR_WRONG_ARGUMENTS_COUNT)

		# Try to play non-existing track.
		uri = 'spotify:track:06a05ZIfZtE0Ss50fDWVuq'
		self.command('%s/play' % uri, ERROR_RESOURCE_NOT_FOUND)
		
		# Try to play unavailable track.
		uri = 'spotify:track:3HfA1uEiX5ZL8KbxR4bghZ'
		self.command('%s/play' % uri, ERROR_RESOURCE_UNAVAILABLE)

		# Regular playback of track.
		uri = 'spotify:track:0rdlHF5qkLd0Y9OQMT3yi6'
		self.command('%s/play' % uri, ERROR_SUCCESS)
		
		# Wait and check status.
		time.sleep(2)
		jsn = self.command('status', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# pause playback.
		jsn = self.command('pause', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != uri or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Start playback again.
		time.sleep(1)
		jsn = self.command('play', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Set repeat().
		time.sleep(1)
		jsn = self.command('repeat/1', ERROR_SUCCESS)
		if jsn['repeat'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Goto next track and verify that we are still on the same track..
		time.sleep(1)
		jsn = self.command('next', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Turn off repeat().
		self.command('repeat/0', ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)
		if jsn['repeat'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Goto next track and verify that playback ends.
		self.command('next', ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)
		if 'context' in jsn or 'track'in jsn or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)

	def suite_playback_album(self):
		print "Suite playback album"
		# Try to play non-existing album.
		uri = 'spotify:album:24F63XEaYPy8ZYsg10TgFe'
		self.command('%s/play' % uri, ERROR_RESOURCE_NOT_FOUND)
		
		# Play album, starting from track 5.
		uri = 'spotify:album:24F63XEaYPy8ZYsg10TgFP'
		self.command('%s/play/5' % uri, ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:6TxoeNdgfBb53WafVVJiKu' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Verify play queue.
		jsn = self.command('queue', ERROR_SUCCESS)
		if len(jsn['tracks']) != 6:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Goto next, verify that track is number 6.
		self.command('next', ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:0rysEwg5fDG78a8t9B55D0' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Play last track in album.
		uri = 'spotify:album:24F63XEaYPy8ZYsg10TgFP'
		self.command('%s/play/10' % uri, ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)		
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:773kx1F7ScD2kl5GXCfwLR' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Goto next, verify that playback ends.
		time.sleep(1)
		jsn = self.command('next', ERROR_SUCCESS)
		if 'context' in jsn or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Play last track in album again.
		uri = 'spotify:album:24F63XEaYPy8ZYsg10TgFP'
		self.command('%s/play/10' % uri, ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)		
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:773kx1F7ScD2kl5GXCfwLR' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Set repeat().
		self.command('repeat/1', ERROR_SUCCESS)
		
		# Play next track, verify that first is played.
		jsn = self.command('next', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:42fwGhwT79zegnn7SG0FZL' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Play previous track, verify that last is played.
		jsn = self.command('previous', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:773kx1F7ScD2kl5GXCfwLR' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# pause playback.
		jsn = self.command('pause', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:773kx1F7ScD2kl5GXCfwLR' or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)

	def suite_playback_playlist(self):
		print "Suite playback playlist"
		# Play non-existing playlist.
		
		# Play playlist.
		uri = 'spotify:user:viktorlt:playlist:48L6EgB1x807lU8T4hEV59'
		self.command('%s/play/7' % uri, ERROR_SUCCESS)
		while True:
			jsn = self.command('status', ERROR_SUCCESS)
			if jsn['loading'] == 0:
				break
			time.sleep(2)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:5e1PDDkb0pnyMqVzSXaEot' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# pause playback.
		jsn = self.command('pause', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:5e1PDDkb0pnyMqVzSXaEot' or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
	
	def suite_playback_search(self):
		print "Suite playback search"
		# Play from search result.
		uri = 'spotify:search:sniffy'
		self.command('%s/play/1' % uri, ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:2kP0vckR1dl6o9hDKxCE4l' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# pause playback.
		jsn = self.command('pause', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:2kP0vckR1dl6o9hDKxCE4l' or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
	
	def suite_playback_queue(self):
		print "Suite playback queue"
		# No repeat or shuffle.
		self.command('repeat/0', ERROR_SUCCESS)
		self.command('shuffle/0', ERROR_SUCCESS)

		# Clear queue.
		self.command('queue/clear', ERROR_SUCCESS)
	
		# Play album to use as playback context.
		uri = 'spotify:album:25hgXh4ZOyncVZarZEHycm'
		album_track_uri = 'spotify:track:4CNIiODuKS3NJ8qPALVwMp'
		self.command('%s/play' % uri, ERROR_SUCCESS)
		time.sleep(1)
		jsn = self.command('status', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != album_track_uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
	
		# Verify play queue.
		jsn = self.command('queue', ERROR_SUCCESS)
		if len(jsn['tracks']) != 21:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Get current track.
		jsn = self.command('status', ERROR_SUCCESS)
		current_track_uri = jsn['track']['uri'];
		
		# Add three tracks to queue.
		track_1_uri = 'spotify:track:0w2zkZxjH2Cg7FdhUywFSk'
		track_2_uri = 'spotify:track:20zz4AJCEXS0oE9xEN8bwJ'
		track_3_uri = 'spotify:track:3Vqnl2gNVEcFVGHUp7ILDq'
		self.command('queue/add/%s' % track_1_uri, ERROR_SUCCESS)
		self.command('queue/add/%s' % track_2_uri, ERROR_SUCCESS)
		self.command('queue/add/%s' % track_3_uri, ERROR_SUCCESS)
		
		# Verify their existence.
		jsn = self.command('queue', ERROR_SUCCESS)
		if len(jsn['tracks']) != 24 or jsn['tracks'][0]['uri'] != current_track_uri or jsn['tracks'][1]['uri'] != track_1_uri or jsn['tracks'][2]['uri'] != track_2_uri or jsn['tracks'][3]['uri'] != track_3_uri:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Remove middle track.
		self.command('queue/remove/%d' % 1, ERROR_SUCCESS)
		
		# Verify their existence.
		jsn = self.command('queue', ERROR_SUCCESS)
		if len(jsn['tracks']) != 23 or jsn['tracks'][1]['uri'] != track_1_uri or jsn['tracks'][2]['uri'] != track_3_uri:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Play next track, verify that track 1 is played.
		jsn = self.command('next', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != track_1_uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Play previous track, verify that first track in album is played.
		jsn = self.command('previous', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != album_track_uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Play next track, verify that track 3 is played.
		jsn = self.command('next', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != track_3_uri or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Play next track, verify album track 2 is played.
		jsn = self.command('next', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['track']['uri'] != 'spotify:track:2WfbFEAMEvbhsDVis8kull' or jsn['playing'] != 1:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
		
		# Add three tracks to queue again.
		self.command('queue/add/%s' % track_1_uri, ERROR_SUCCESS)
		self.command('queue/add/%s' % track_2_uri, ERROR_SUCCESS)
		self.command('queue/add/%s' % track_3_uri, ERROR_SUCCESS)
		
		# Verify their existence.
		jsn = self.command('queue', ERROR_SUCCESS)
		if len(jsn['tracks']) != 23 or jsn['tracks'][1]['uri'] != track_1_uri or jsn['tracks'][2]['uri'] != track_2_uri or jsn['tracks'][3]['uri'] != track_3_uri:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# Clear queue.
		self.command('queue/clear', ERROR_SUCCESS)
		
		# Verify that the queue is cleared.
		jsn = self.command('queue', ERROR_SUCCESS)
		if len(jsn['tracks']) != 20 or jsn['tracks'][0]['uri'] != 'spotify:track:2WfbFEAMEvbhsDVis8kull':
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
			
		# pause playback.
		jsn = self.command('pause', ERROR_SUCCESS)
		if jsn['context'] != uri or jsn['playing'] != 0:
			print 'Validation failed: The status does not match the expected one. Json: %s' % json.dumps(jsn)
			exit(1)
	
	def suite_stress(self):
		print "Suite stress"
		for x in range(0, 2049):
			print x
			self.command('status', ERROR_SUCCESS)
			
		for x in range(0, 100):
			print x
			self.command('spotify:album:0TBmLh3U5sk1F8eF0twjwi', ERROR_SUCCESS)
	
	def run (self):
	
		# Login/logout test, and initialization.
		self.suite_login(self._Thread__kwargs['username'], self._Thread__kwargs['password'])
		
		# Invalid commands.
		self.command('blu', ERROR_UNKNOWN_COMMAND)
		
		#self.suite_stress()
		
		# Browse track, album, artist and playlist.
		#self.suite_browse_track()
		#self.suite_browse_album()
		#self.suite_browse_artist()
		self.suite_browse_playlist()
		#self.suite_browse_playlists()
		
		# Search suite.
		self.suite_search()
		
		# Playback of track, playlist and album
		self.suite_playback_track()
		self.suite_playback_album()
		self.suite_playback_search()
		self.suite_playback_playlist()
		self.suite_playback_queue()
		
		print 'Test ended!'

		sys.exit(0)

try:
	opts, args = getopt.getopt(sys.argv[1:], "u:p:")
except getopt.GetoptError, err:
	# print help information and exit:
	print str(err) # will print something like "option -a not recognized"
	usage()
	sys.exit(2)
username = 'empty1'
password = 'empty2'
for o, a in opts:
	if o == "-u":
		username = a
	elif o == "-p":
		password = a
	else:
		assert False, "unhandled option"
# ...

# Thread main so we can abort the program when for example acquiring for a
# semaphore
mainThread = MainThread(kwargs={'username' : username, 'password' : password})
mainThread.start()