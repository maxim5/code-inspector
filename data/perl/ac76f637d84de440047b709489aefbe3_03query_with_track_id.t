#!/usr/bin/perl -w

# Tests for query that includes a track-id.

use strict;
use warnings;
use lib './lib';
use Test::More tests => 11;

BEGIN { use_ok('AudioFile::Identify::MusicBrainz::Query'); }

my $query = AudioFile::Identify::MusicBrainz::Query->new();

isa_ok($query, "AudioFile::Identify::MusicBrainz::Query",
       "created query object");

# --------------------------------------------------------------------------
# Try a query that includes the track_id

unless (ok($query->FileInfoLookup(artist => 'Aerosmith',
				  album => 'Big Ones',
		       'album_id' => '2d5d5d4c-d0d7-4772-962f-de7185605ff8',
		       'artist_id' => '3d2b98e5-556f-4451-a3ff-c50ea18d57cb',
		       'track_id' => 'f27a903c-f5cc-465f-91c4-5c67d8b3d6d6',
				  track => "Amazing",
				  secs => '356.4',
				  tracknum => '2',
				  items => 20,), "Do query")) {
  diag("FileInfoLookup failed, returned '" . $query->error() . "'\n",
       "Skipping remaining tests that depend on query response");
} else {

  is($query->resultCount(), 1, "Got one hit");

  my $result = $query->result(0);

  isa_ok($result, "AudioFile::Identify::MusicBrainz::Result",
	 "got result object");

  is($result->relevance, 100, "Perfect relevance");

  like($result->track->artist->title, qr/Aerosmith/i,
       "Matching artist name");
  like($result->album->title, qr/Big Ones/i, "Matching album name");
  is($result->album->asin, "B000000OTU", "Matching ASIN");
  is($result->album->coverart,
     undef, # "http://images.amazon.com/images/P/B000000OTU.01.MZZZZZZZ.jpg",
     "Matching cover art URL");
  like($result->track->title, qr/Amazing/i, "Matching track title");
}
