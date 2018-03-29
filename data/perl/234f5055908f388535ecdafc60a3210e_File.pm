# Manage the link database (in a file).
#
# Public interface:
#
#  get_link_id($fora, $user, $link, $description)
#  get_link_by_id($id)
#  get_link_obj_by_id($id)
#  get_stale_links($age)
#  get_unchecked_links()
#  get_recent_links($limit)
#  get_links_since($time)
#  link_set_status($link_id, $status)
#  link_set_redirect($link_id, $redirect)
#  link_set_title($link_id, $title)
#  link_set_meta_desc($link_id, $desc)
#  link_set_meta_keys($link_id, $keys)
#  link_set_head_time($link_id, $time)
#  link_get_head_size($link_id)
#  link_set_head_size($link_id, $size)
#  link_set_head_type($link_id, $type)

package Database::File;

use strict;
use Exporter;

use POE;
use Util::Conf;

use vars qw(@ISA @EXPORT);
@ISA    = qw(Exporter);
@EXPORT = qw( FLUSH_FIRST_MINUTES FLUSH_REST_MINUTES
              LINK DESC USER TIME PAGE_TITLE PAGE_DESC
              PAGE_KEYS PAGE_TIME PAGE_SIZE PAGE_TYPE
              CHECK_TIME CHECK_STATUS MENTION_COUNT
              REDIRECT FORA
              get_link_id get_link_by_id get_link_obj_by_id
	      get_recent_links get_stale_links get_unchecked_links
	      get_links_since
              link_set_status link_set_title link_set_meta_desc
              link_set_meta_keys link_set_head_time link_set_head_size
              link_set_head_type link_set_redirect
              link_get_head_size
            );

#------------------------------------------------------------------------------
# exported constants

sub FLUSH_FIRST_MINUTES () { 30 }  # First flush after N minutes.
sub FLUSH_REST_MINUTES  () { 60 }  # Subsequent flushes after N minutes.

sub LINK          () {  0 }
sub DESC          () {  1 }
sub USER          () {  2 }
sub TIME          () {  3 }
sub PAGE_TITLE    () {  4 }
sub PAGE_DESC     () {  5 }
sub PAGE_KEYS     () {  6 }
sub PAGE_TIME     () {  7 }
sub PAGE_SIZE     () {  8 }
sub PAGE_TYPE     () {  9 }
sub CHECK_TIME    () { 10 }
sub CHECK_STATUS  () { 11 }
sub MENTION_COUNT () { 12 }
sub REDIRECT      () { 13 }
sub FORA          () { 14 }

#------------------------------------------------------------------------------
# Helper function to record links.

use vars qw( %id_by_link %link_by_id $link_seq @recent $log_file );

BEGIN {
  my $database = (get_names_by_type('database'))[0];
  my %conf  = get_items_by_name($database);
  $log_file = $conf{dbname};

  unless (-e $log_file) {
    open LOG_FILE, ">$log_file" or die "can't create $log_file: $!";
    close LOG_FILE;
  }

  open LOG_FILE, "<$log_file" or die "can't read $log_file: $!";
  while (<LOG_FILE>) {
    chomp;
    my @link = split /\t/;
    my $id   = shift @link;

    # Fix up late things.
    $link[USER] =~ s/\,$//;
    $link[MENTION_COUNT] = 1
      unless $link[MENTION_COUNT];
    $link[FORA] = 'global'
      unless defined($link[FORA]) and length($link[FORA]);
    $link[TIME] = 0 unless defined($link[TIME]) and $link[TIME] =~ /^\d+$/;
    $link[CHECK_TIME] = 0
      unless defined($link[CHECK_TIME]) and $link[CHECK_TIME] =~ /^\d+$/;

    # Store the link by its unique ID.
    $link_by_id{$id} = \@link;

    # Record ID by link, but partition links by fora.  This is where
    # forum partitioning comes in.
    $id_by_link{$link[LINK]} = $id;

    # So new links are added with new IDs.
    $link_seq = $id;
  }
  close LOG_FILE or warn "can't close $log_file: $!";
}

sub flush_links {
  my $backup = $log_file . ".backup";

  unlink $backup;
  rename $log_file, $backup;

  if (open LOG_FILE, ">$log_file") {
    local $^W = 0;

    foreach my $id (sort { $a <=> $b } keys %link_by_id) {
      my $link = $link_by_id{$id};
      print LOG_FILE join("\t", $id, @$link), "\n";
    }
    close LOG_FILE;
  }
  else {
    rename $backup, $log_file;
  }
}

END {
  flush_links();
}

#------------------------------------------------------------------------------
# Get an ID for a link.

sub get_id_by_link {
  my $link = shift;
  return $id_by_link{$link} if exists $id_by_link{$link};
  return undef;
}

#------------------------------------------------------------------------------
# Get an ID for a link.  It may store the link if it's new.

sub get_link_id {
  my ($fora, $user, $link, $description) = @_;

  my $id = get_id_by_link($link);
  if (defined $id) {
    $link_by_id{$id}->[MENTION_COUNT]++;
    return get_id_by_link($link);
  }

  $id_by_link{$link} = ++$link_seq;

  $link_by_id{$link_seq} =
    [ $link,         # LINK
      $description,  # DESC
      $user,         # USER
      time(),        # TIME
      undef,         # PAGE_TITLE
      undef,         # PAGE_DESC
      undef,         # PAGE_KEYS
      undef,         # PAGE_TIME
      undef,         # PAGE_SIZE
      undef,         # PAGE_TYPE
      0,             # CHECK_TIME
      undef,         # CHECK_STATUS
      1,             # MENTION_COUNT
      undef,         # REDIRECT
    ];

  # Blow away caches.
  undef @recent;

  # Request a lookup.
  $poe_kernel->post( linkchecker => enqueue => 'ignore this' => $link_seq );

  return $link_seq;
}

#------------------------------------------------------------------------------
# Get a link by its ID.  Creates a link record 

sub get_link_by_id {
  my $id = shift;
  return $link_by_id{$id}->[LINK] if exists $link_by_id{$id};
  return undef;
}

#------------------------------------------------------------------------------
# Get a link object by its ID.

sub get_link_obj_by_id {
  my $id = shift;
  return $link_by_id{$id} if exists $link_by_id{$id};
  return undef;
}

#------------------------------------------------------------------------------

# Fetch stale links.  Stale links are ones that have been checked, but
# they haven't been checked recently.

sub get_stale_links {
  my $age = shift;
  my $now = time();
  my @stale =
    ( sort { $link_by_id{$b}->[TIME] <=> $link_by_id{$a}->[TIME] }
      grep { my $check_time = $link_by_id{$_}->[CHECK_TIME];
             defined($check_time) and (($now - $check_time) >= $age)
	   } keys %link_by_id
    );
  return @stale;
}

# Unchecked links are ones that have never been checked before.

sub get_unchecked_links {
  my @unchecked =
    ( sort { $link_by_id{$b}->[TIME] <=> $link_by_id{$a}->[TIME] }
      grep { my $link = $link_by_id{$_};
	     ( !defined($link->[CHECK_TIME]) or
               $link->[CHECK_TIME] == 0
	     )
	   } keys %link_by_id
    );
  return @unchecked;
}

#------------------------------------------------------------------------------
# Get up to N of the most recent links.

sub get_recent_links {
  my $limit = shift;

  # Global cached value.
  @recent = ( sort { $link_by_id{$b}->[TIME] <=> $link_by_id{$a}->[TIME] }
              keys %link_by_id
            )
    unless @recent;

  return @recent if @recent < $limit;
  return @recent[0..$limit-1];
}

#------------------------------------------------------------------------------
# Get links changed since a time.

sub get_links_since {
  my $time = shift;

  # Global cached value.
  @recent = ( sort { $link_by_id{$b}->[TIME] <=> $link_by_id{$a}->[TIME] }
              keys %link_by_id
            )
    unless @recent;

  my @since;
  foreach (@recent) {
    next if $link_by_id{$_}->[TIME] < $time;
    push @since, $_;
  }

  return @since;
}

#------------------------------------------------------------------------------
# Accessors.

sub link_set_status {
  my ($link_id, $status) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[CHECK_STATUS] = $status;
  $link_rec->[CHECK_TIME]   = time();
}

sub link_set_redirect {
  my ($link_id, $redirect) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[REDIRECT] = $redirect;
  $link_rec->[CHECK_TIME] = time();
}

sub link_set_title {
  my ($link_id, $title) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[PAGE_TITLE] = $title;
  $link_rec->[CHECK_TIME] = time();
}

sub link_set_meta_desc {
  my ($link_id, $desc) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[PAGE_DESC] = $desc;
  $link_rec->[CHECK_TIME] = time();
}

sub link_set_meta_keys {
  my ($link_id, $keys) = @_;

  my @keys = split(/ *, */, lc($keys));
  my %keys;
  @keys{@keys} = @keys;
  $keys = join(', ', sort keys %keys );

  my $link_rec = $link_by_id{$link_id};
  $link_rec->[PAGE_KEYS] = $keys;
  $link_rec->[CHECK_TIME] = time();
}

sub link_set_head_time {
  my ($link_id, $time) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[PAGE_TIME] = $time;
  $link_rec->[CHECK_TIME] = time();
}

sub link_get_head_size {
  my $link_id = shift;
  my $link_rec = $link_by_id{$link_id};
  return $link_rec->[PAGE_SIZE];
}

sub link_set_head_size {
  my ($link_id, $size) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[PAGE_SIZE] = $size;
  $link_rec->[CHECK_TIME] = time();
}

sub link_set_head_type {
  my ($link_id, $type) = @_;
  my $link_rec = $link_by_id{$link_id};
  $link_rec->[PAGE_TYPE] = $type;
  $link_rec->[CHECK_TIME] = time();
}

#------------------------------------------------------------------------------
# Periodically flush links to disk.

POE::Session->new
  ( _start => sub {
      $_[KERNEL]->delay( flush_links => FLUSH_FIRST_MINUTES * 60 );
    },

    flush_links => sub {
      $_[KERNEL]->delay( flush_links => FLUSH_REST_MINUTES * 60 );
      flush_links();
    },
  );

#------------------------------------------------------------------------------
1;
