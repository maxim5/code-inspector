# Epic NMS manage functions library
#
# Copyright (C) 2007 Ryan Kubica (ryankubica@yahoo.com)
# Distributed under GPLv2; see License_Epic for details

# TODO - audit/fix this library for use of %opts from main

use strict 'vars';

use vars qw(%cfg
            %db
            $cgi
            $dsn
            $dbh);

#
# - Subroutines
#

sub manage_warning {

   # returns 1 if YES 0 if not

   my(%hash) = @_;

   print <<EOF;
------------------------------------------------------------------------
WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING

   $hash{'text'}

   Are you sure you want do to this?

WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
------------------------------------------------------------------------

EOF

   print "Type: YES or NO > ";

   if (<STDIN> =~ /^YES/) {
      return(1);
   }

   return(0);

}
sub manage_generate_authkey {

   # !!! Do Not make this command available via CGI !!!

   my(%hash) = @_;

   my($sth,
      $authkey);
 
   if ($hash{'init'}) {

      $sth = $dbh->prepare("SELECT * FROM configuration WHERE node='Global' && name='manage:authkey'");
      $sth->execute();
      if ($sth->rows() > 0) {
         return();
      }

   }

   if ($hash{'argv'}->[0]) {
      $authkey = $hash{'argv'}->[0];
   }
   else {
      $authkey = md5_hex(time . "$$" . join('', keys %ENV));
   }

   $dbh->do("DELETE FROM configuration WHERE node='Global' && name='manage:authkey'");
   $dbh->do("INSERT INTO configuration SET   node='Global',   name='manage:authkey', permanent='1', value='$authkey'");

   print "manage:authkey set to: $authkey\n";

}
sub manage_aggregate_list {

   my(%hash) = @_;

   my($sth,
      $ref,
      $reply,
      $WHERE);

   $WHERE = "WHERE ng='$hash{'g'}'" if ($hash{'g'});

format Aggregate_Format =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<<<<<<<<<<<
$ref->{'ng'}, $ref->{'aggregate'}
.

   $~ = 'Aggregate_Format';

   $sth = $dbh->prepare("SELECT ng, aggregate FROM aggregate_node ORDER BY ng");

   $sth->execute();

   while ($ref = $sth->fetchrow_hashref()) {

      if ($ref->{'aggregate'} == 1) { $ref->{'aggregate'} = 'enabled';  }
      else                          { $ref->{'aggregate'} = 'disabled'; }

      write;

   }

}
sub manage_aggregate_enable {

   my(%hash) = @_;
   my($obj)  = {};

   my($sth,
      $ref);

   if ($#{$hash{'argv'}} == -1) {
      manage_obj($obj, text => "no arguments supplied to command", attribute => $hash{'n'}, status => 'FAILURE');
      manage_output($obj);
      return($obj);
   }

   foreach (@{$hash{'argv'}}) {

      $sth = $dbh->prepare("SELECT * FROM aggregate_node WHERE ng='$_'");

      $sth->execute();

      if ($sth->rows() == 0) {
         $dbh->do("INSERT INTO aggregate_node SET ctime=now(), permanent='1', ng='$_', aggregate='1'");
         manage_obj($obj, text => "enabling aggregation group", attribute => "$_", status => 'SUCCESS');
      }
      else {

         $ref = $sth->fetchrow_hashref();

         if ($ref->{'aggregate'} ne '1') {
            $dbh->do("UPDATE aggregate_node SET aggregate='1' WHERE ng='$_'");
            manage_obj($obj, text => "enabling aggregation group", attribute => "$_", status => 'SUCCESS');
            update_cache('reportd');
         }
         else {
            manage_obj($obj, text => "aggregation group already enabled", attribute => "$_", status => 'SUCCESS');
         }

      }

      $sth->finish();

   }

   manage_output($obj);

   return($obj);

}
sub manage_aggregate_disable {

   my(%hash) = @_;
   my($obj)  = {};

   my($sth,
      $ref);

   if ($#{$hash{'argv'}} == -1) {
      manage_obj($obj, text => "no arguments supplied to command", attribute => $hash{'n'}, status => 'FAILURE');
      manage_output($obj);
      return($obj);
   }

   foreach (@{$hash{'argv'}}) {

      $sth = $dbh->prepare("SELECT * FROM aggregate_node WHERE ng='$_'");

      $sth->execute();

      if ($sth->rows() == 0) {
         manage_obj($obj, text => "aggregation group does not exist", attribute => "$_", status => 'SUCCESS');
      }
      else {
         $dbh->do("UPDATE aggregate_node SET aggregate='0' WHERE ng='$_'");
         manage_obj($obj, text => "disabling aggregation group", attribute => "$_", status => 'SUCCESS');
         update_cache('reportd');
      }

      $sth->finish();

   }

   manage_output($obj);

   return($obj);

}
sub manage_audit_rrd_aged {

   my(%hash) = @_;

   my($sth,
      $ref,
      $c,
      $info);

   $hash{'a'} = 180 if (!$hash{'a'});

   if (($hash{'a'} < 2) && (!$hash{'f'})) {

      print "WARNING: overriding option a input, less than 2 days, resetting to 7 days\n" if ( -t STDOUT );
      $hash{'a'} = 7;

   }

   print "Auditing RRD Finding Aged Data Files: older than $hash{'a'} days\n" if ( -t STDOUT );

   $sth = $dbh->prepare("SELECT count(*) c FROM semaphore WHERE name='database' && value='$cfg{'rrd_node_id'}' && node='$cfg{'hostname'}';");
   $sth->execute();
   $ref = $sth->fetchrow_hashref();
   $sth->finish();

   if ($ref->{'c'} != 1) {
      print "Must run on an Epic RRD Database server.\n";
      print "Current epic node $cfg{'hostname'} is not an RRD database server: exiting.\n";
      exit;
   }

   $sth = $dbh->prepare("SELECT filename FROM rrd_filesystem$cfg{'rrd_node_id'};");
   $sth->execute();
   $ref = $sth->fetchall_arrayref();
   $sth->finish();

   for $c (0 ... $#{$ref}) {


      if (-f "$ref->[$c][0]") {

         eval {
            $info = RRDs::info($ref->[$c][0]);
         };

         if ($@) {
            print "skipping $ref->[$c][0] - $@\n";
            next;
         }

         next if (!$info->{'last_update'});

         if ($info->{'last_update'} < (time - (1440 * 60 * $hash{'a'}))) {
            if (!$hash{'v'}) { print "$ref->[$c][0]\n";                                                                     }
            else             { print "old $ref->[$c][0] - " . int((time - $info->{'last_update'}) / 1440 / 60) . " days\n"; }
         }
         else {
            print "ok  $ref->[$c][0]\n" if ($hash{'v'});
         }

      }

   }

}
sub manage_audit_rrd_check {

   my(%hash) = @_;

   $hash{'a'} = 15 if (!$hash{'a'});

   print "Not implimented yet; nice idea though hunh?\n";

   return;

   print "Auditing RRD Checking Data Files: created within last $hash{'a'} days\n";

}
sub manage_audit_rrd_filesystem {

   my(%hash) = @_;
   my($obj)  = {};

   my($sth,
      $ref,
      $c,
      $rrd_files,
      %duplicate,
      %rrd_filesystem,
      $inconsistant,
      $action);

   $sth = $dbh->prepare("SELECT count(*) c FROM semaphore WHERE name='database' && value='$cfg{'rrd_node_id'}' && node='$cfg{'hostname'}';");
   $sth->execute();
   $ref = $sth->fetchrow_hashref();
   $sth->finish();

   if ($ref->{'c'} != 1) {

      if (!manage_warning(text => "Current epic node $cfg{'hostname'} is not an RRD database server")) {
         return($obj);
      }

   }

   # Checking SQL->filesystem
   #

   manage_obj($obj, text => "Auditing RRD Filesystem: sql table entries -> rrd files");

   $sth = $dbh->prepare("SELECT filename, node, ds, id FROM rrd_filesystem$cfg{'rrd_node_id'}");
   $sth->execute();
   $ref = $sth->fetchall_arrayref();
   $sth->finish();

   $inconsistant = 0;

   for $c (0 ... $#{$ref}) {

      if (! -f "$ref->[$c][0]") {

         if ($hash{'d'}) {
            $dbh->do("DELETE FROM rrd_filesystem$cfg{'rrd_node_id'} WHERE filename='$ref->[$c][0]'");
            manage_obj($obj, text => "removed: $ref->[$c][0]", attribute => "$ref->[$c][0]", status => 'REMOVED RRD')     if ($hash{'v'});
            $action = " corrected"; # note this word is used below in a manage_obj() call
            $inconsistant++;
            next;
         }
         else {
            manage_obj($obj, text => "missing rrd: $ref->[$c][0]", attribute => "$ref->[$c][0]", status => 'MISSING RRD') if ($hash{'v'});
            $inconsistant++;
         }

      }

      my(@rrd_location)                = split(/\//, $ref->[$c][0]);
         $rrd_location[$#rrd_location] =~ s/\.rrd$//;

      $rrd_filesystem{"/$rrd_location[$#rrd_location - 1]/$rrd_location[$#rrd_location]"} = $ref;

      if ((!$ref->[$c][1]) || (!$ref->[$c][2])) {

         manage_obj($obj, text => "incorrect: /$rrd_location[$#rrd_location - 1]/$rrd_location[$#rrd_location] -> $ref->[$c][0]", attribute => "$rrd_location[$#rrd_location]", status => 'INCORRECT')   if ($hash{'v'});

         eval {
            $dbh->do("UPDATE rrd_filesystem$cfg{'rrd_node_id'} SET node='$rrd_location[$#rrd_location - 1]', ds='$rrd_location[$#rrd_location]', vds='epoch' WHERE id='$ref->[$c][3]'") if ($hash{'d'});
         };

         if ($@) {
            print "wtf: $@\n";
         }

         $inconsistant++;

      }

   }

   manage_obj($obj, text => "Completed sql->file check on " . ($#{$ref} + 1) . " datapoint files, $inconsistant inconsistencies$action.");

   update_cache('memcache:vault', 'writed', 'reportd') if (($hash{'d'}) && ($inconsistant > 0));

   # Checking filesystem->SQL
   #

   manage_obj($obj, text => "Auditing RRD Filesystem: rrd files -> sql table entries");

   $inconsistant = 0;
   $rrd_files    = 0;

   my($filesystem,
      $filename);

   foreach $filesystem (get_rrd_filesystem(all => 1)) {

      print "searching $filesystem\n";

      foreach $filename (`find -L $filesystem -type f`) {

         next if ($filename !~ /\.rrd$/);

         $rrd_files++;

         chomp($filename);

         my(@rrd_location)                = split(/\//, $filename);
            $rrd_location[$#rrd_location] =~ s/.rrd$//;

         $ref = $rrd_filesystem{"/$rrd_location[$#rrd_location - 1]/$rrd_location[$#rrd_location]"};

         if (!$ref->[$c]) {

            manage_obj($obj, text => "missing sql: /$rrd_location[$#rrd_location - 1]/$rrd_location[$#rrd_location] -> $filename", attribute => "$rrd_location[$#rrd_location]", status => 'MISSING SQL') if ($hash{'v'});

            eval {
               if ($hash{'d'}) {
                  $dbh->do("INSERT INTO rrd_filesystem$cfg{'rrd_node_id'} SET ctime=now(), node='$rrd_location[$#rrd_location - 1]', ds='$rrd_location[$#rrd_location]', vds='epoch', filename='$filename'");
                  $action = " corrected"; # note this word is used below in a manage_obj() call
               }
            };

            if ($@) {
               print "wtf: $@\n";
            }

            $inconsistant++;

         }

      }

   }

   update_cache('memcache:vault', 'writed', 'reportd') if (($hash{'d'}) && ($inconsistant > 0));

   manage_obj($obj, text => "Completed file->sql check on $rrd_files datapoint files, $inconsistant inconsistencies$action.");

   manage_output($obj);

   return($obj);

}
sub manage_node_secondary_group_add {

   my(%hash) = @_;
   my($obj)  = {};

   eval {
      $dbh->do("INSERT INTO node_group SET ctime=NOW(), permanent='1', ng='$hash{'G'}', node='$hash{'n'}'");
   };

   if ($@) {
      if ($@ =~ /Duplicate entry/) {
         manage_obj($hash{'obj'}, text => "secondary group already exists: $hash{'G'}", attribute => $hash{'n'}, status => 'SUCCESS');
      }
      else {
         manage_obj($hash{'obj'}, text => "secondary group addition failed: $@", attribute => $hash{'n'}, status => 'FAILURE');
      }
   }
   else {
      manage_obj($hash{'obj'}, text => "adding to secondary group: $hash{'G'}", attribute => $hash{'n'}, status => 'SUCCESS');
      update_cache('pollerd', 'reportd');
   }

   manage_output($obj);

   return($obj);

}
sub manage_node_secondary_group_remove {

   my(%hash) = @_;
   my($obj)  = {};

   eval {
      $dbh->do("DELETE FROM node_group WHERE ng='$hash{'G'}' && node='$hash{'n'}'");
   };

   if ($@) {
      manage_obj($hash{'obj'}, text => "secondary group remove failed: $@", attribute => $hash{'n'}, status => 'FAILURE');
   }
   else {
      manage_obj($hash{'obj'}, text => "removing secondary group: $hash{'G'}", attribute => $hash{'n'}, status => 'SUCCESS');
      update_cache('pollerd', 'reportd');
   }

   manage_output($obj);

   return($obj);

}
sub manage_cnode_add {

   # this function manages the node table as well if ng (-g) is passed in
   # what occurs is the node group is dealt with in a FIFO method

   # TODO - use -a for age, default to 30 minutes like below is hardcoded for

   my(%hash) = @_;

   my($sql,
      $sth,
      $ref,
      $remote);

   if ($#{$hash{'argv'}} == -1) {
      print "no arguments supplied to command: exiting\n";
      exit;
   }

   $hash{'a'} = 1800 if (!$hash{'a'});

   if ($hash{'a'} !~ /^\d+$/) {
      print "age must be numeric\n";
      exit;
   }

   foreach (@{$hash{'argv'}}) {

      my($node, $cnode) = split(/:/);

      if ($hash{'g'}) {

         $remote = $node;
         $remote = $cnode if ($cnode);

         $sql = "SELECT /*! SQL_CACHE */ node slot FROM node WHERE ng='$hash{'g'}' && PollerDisabled='1' && ctime<=DATE_SUB(NOW(), INTERVAL $hash{'a'} SECOND) && node LIKE '$hash{'g'}%' ORDER BY node DESC LIMIT 1";
         $sth = $dbh->prepare($sql);
         $sth->execute();
         $ref = $sth->fetchrow_hashref();
         $sth->finish();

         if ($ref->{'slot'}) {

            $ref->{'slot'} =~ s/^$hash{'g'}//;

         }
         else {

            $sql = "SELECT /*! SQL_CACHE */ node slot FROM node WHERE ng='$hash{'g'}' && node LIKE '$hash{'g'}%' ORDER BY node DESC";
            $sth = $dbh->prepare($sql);
            $sth->execute();
            $ref = $sth->fetchrow_hashref();
            $sth->finish();

            if ($ref->{'slot'}) {
               $ref->{'slot'} =~ s/^$hash{'g'}//;
               $ref->{'slot'}++;
            }
            else {
               $ref->{'slot'} = 0;
            }

            $ref->{'slot'} = sprintf("%04d", $ref->{'slot'});

         }

         $cnode = "$hash{'g'}$ref->{'slot'}";

         manage_node_add(g => $hash{'g'}, n => "$cnode:$remote", f => 1);

         manage_node_enable(n => "$cnode", feature => 'Poller');
         manage_node_enable(n => "$cnode", feature => 'Aggregate');

      }

      $dbh->do("INSERT INTO node_canonical SET ctime=now(), permanent='1', node='$node', cnode='$cnode'");

   }

}
sub manage_install_client {

   my(%hash) = @_;

   my($sth, 
      $ref,
      $reply,
      $WHERE);

   if ($hash{'g'}) {

      $WHERE = "WHERE ng='$hash{'g'}' && PollerDisabled='0'" if ($hash{'g'});

      $sth = $dbh->prepare("SELECT node, ng, ip FROM node $WHERE ORDER BY ng, node");

      $sth->execute();

      while ($ref = $sth->fetchrow_hashref()) {

         print "Remote install on $ref->{'node'}\n";

         system("ssh $ref->{'node'} mkdir -p /usr/local/epic/");

         system("rsync -ae ssh --delete --exclude=.svn /apps/epic/epic-$cfg{'module'}/dist/epic/ $ref->{'node'}:/usr/local/epic/");
         system("rsync -ae ssh          --exclude=.svn /apps/epic/dist/epic-$cfg{'module'}/epic/ $ref->{'node'}:/usr/local/epic/");

         system("/apps/epic/epic-$cfg{'module'}/bin/wrap -T 120 -i $ref->{'node'} -s \"ssh %S% /usr/local/epic/distro/install\"");

      }

   }

   if ($#ARGV > -1) {

      foreach (@ARGV) {

         print "Remote install on $_\n";

         system("ssh $_ mkdir -p /usr/local/epic/");

         system("rsync -ae ssh --delete /apps/epic/epic-$cfg{'module'}/dist/epic/ $_:/usr/local/epic/");
         system("rsync -ae ssh          /apps/epic/dist/epic-$cfg{'module'}/epic/ $_:/usr/local/epic/");

         system("/apps/epic/epic-$cfg{'module'}/bin/wrap -T 120 -i $_ -s \"ssh %S% /usr/local/epic/distro/install\"");

      }

   }

}
sub manage_create_client {

   my($version,
      $release,
      $tmpdir,
      $tmptar,
      $curtar,
      $curdir);

   foreach ('BUILD','RPMS','SOURCES','SPECS','SRPMS') {
      system("mkdir -p /apps/epic/dist/epic-$cfg{'module'}/$_") if (! -d "/apps/epic/dist/epic-$cfg{'module'}/$_");
   }

   $curtar = readlink("/apps/epic/dist/epic-$cfg{'module'}/SOURCES/epic-client-latest.tar.gz");
   $curdir = $curtar;
   $curdir =~ s/.tar.gz//;

   $version = '1.2';
   $release = int(time);
   $tmpdir  = "epic-client-$version-$release";
   $tmptar  = "epic-client-$version-$release.tar.gz";

   system("mkdir -p /apps/epic/dist/epic-$cfg{'module'}/SOURCES/$tmpdir");

   chdir("/apps/epic/dist/epic-$cfg{'module'}/SOURCES") || die $!;

   system("rsync -ae ssh --delete --exclude=.svn /apps/epic/epic-$cfg{'module'}/dist/epic/ $tmpdir/");
   system("rsync -ae ssh          --exclude=.svn /apps/epic/dist/epic-$cfg{'module'}/epic/ $tmpdir/") if (-d "/apps/epic/dist/epic-$cfg{'module'}/epic/");

   if ((!$curdir) || (system("diff -r $curdir/ /apps/epic/dist/epic-$cfg{'module'}/SOURCES/$tmpdir/"))) {

      print "updating client package\n";

      unlink("epic-client-$version");
      symlink("$tmpdir", "epic-client-$version");

      # create version file in version-release dir

      open(VERSION, "> $tmpdir/conf/version-$version-$release"); close VERSION;

      # build tar.gz

      system("tar czf /apps/epic/dist/epic-$cfg{'module'}/SOURCES/$tmptar $tmpdir/ epic-client-$version > /dev/null"); 

      # build rpm

      system("/bin/sed s/RELEASE/$release/ /apps/epic/epic-$cfg{'module'}/conf/default/epic-client.spec | /bin/sed s/INSTANCE/$cfg{'module'}/ > /apps/epic/dist/epic-$cfg{'module'}/SPECS/epic-client.spec");

      open(SPEC, ">>/apps/epic/dist/epic-$cfg{'module'}/SPECS/epic-client.spec");
      open(FIND, "/usr/bin/find /apps/epic/dist/epic-$cfg{'module'}/SOURCES/$tmpdir -type f |");
      while (<FIND>) {
         next if (/install.sh|epic.cf/);
         s#/apps/epic/dist/epic-$cfg{'module'}/SOURCES/$tmpdir#/usr/local/epic#;
         print SPEC;
      }
      close FIND;
      close SPEC;

      chdir("/apps/epic/dist/epic-$cfg{'module'}");

      system("/usr/bin/rpmbuild -v -bb --clean SPECS/epic-client.spec > /dev/null 2>&1");

      # remove version file (since it changes between builds but build contents might not)

      chdir("/apps/epic/dist/epic-$cfg{'module'}/SOURCES");
      unlink("$tmpdir/conf/version-$version-$release");

      # create tracing symlinks

      unlink("/apps/epic/dist/epic-$cfg{'module'}/epic-client-latest.tar.gz");
      symlink("/apps/epic/dist/epic-$cfg{'module'}/SOURCES/$tmptar", "/apps/epic/dist/epic-$cfg{'module'}/SOURCES/epic-client-latest.tar.gz");

   }
   else {

      print "no change since last create; exiting.\n";
      system("rm -rf /apps/epic/dist/epic-$cfg{'module'}/$tmpdir/");

   }

}
sub manage_node_list {

   my(%hash) = @_;

   my($sth,
      $ref,
      $reply,
      $sql,
      $WHERE,
      $ORDERBY,
      @entries,
      $obj);

   $hash{'e'} = 1 if ($cfg{'program'} eq 'epic-cgi');

   # select for nodes

   $sql   = "SELECT * FROM node";

   if ($hash{'a'}) {
      if    ($hash{'a'} =~ /^\d+$/) {
         $WHERE = "WHERE (mtime < DATE_SUB(NOW(), INTERVAL $hash{'a'} DAY))";
      }
      elsif ($hash{'a'} =~ /^-\d+$/) {
         $WHERE = "WHERE (mtime > DATE_SUB(NOW(), INTERVAL $hash{'a'} DAY))";
      }
   }

   $hash{'order'} = $hash{'o'} if ($hash{'o'}); # command line uses -o for 'order'

   CASE: {
      $ORDERBY = "ORDER BY $hash{'order'}", last if ($hash{'order'} =~ /^[a-z]/i);
      $ORDERBY = "ORDER BY ng, node";
   }

   $sth = $dbh->prepare("$sql $WHERE $ORDERBY");

   $sth->execute();

   while ($ref = $sth->fetchrow_hashref()) {

      ($ref->{'cnode'})        = get_node_names(n => $ref->{'node'});
      ($ref->{'secondary_ng'}) = join(',', grep(!/^$ref->{'ng'}$/, get_ngs_for_node(n => $ref->{'node'})));

      if ($hash{'g'}) {
         next if (($hash{'g'} ne $ref->{'ng'}) && ($ref->{'secondary_ng'} !~ /\b$hash{'g'}\b/));
      }

      if ($ref->{'PollerDisabled'} == 1)    { $ref->{'PollerDisabled'}    = 'disabled'; }
      else                                  { $ref->{'PollerDisabled'}    = 'enabled';  }

      if ($ref->{'AggregateDisabled'} == 1) { $ref->{'AggregateDisabled'} = 'disabled'; }
      else                                  { $ref->{'AggregateDisabled'} = 'enabled';  }

      if ($hash{'filter'}) {
         if (($ref->{'node'}  =~ /$hash{'filter'}/i) ||
             ($ref->{'ng'}    =~ /$hash{'filter'}/i)) {

            push(@entries, $ref);

         }
      }
      else {
         push(@entries, $ref);
      }

   }

   $sth->finish();

   if ($cfg{'program'} eq 'epic-cgi') {

      if ($hash{'dojo'}) {

         $obj->{'identifier'} = 'node';
         $obj->{'numRows'}    = scalar @entries;

         if (($hash{'start'} ne '') && ($hash{'count'} ne '')) {
            foreach (0 ... ($hash{'start'} - 1)) {
               shift(@entries);
            }
            $#entries = ($hash{'count'} - 1) if ($hash{'count'} < scalar @entries);
         }

         $obj->{'items'} = \@entries;

      }
      else {
         $obj->{'manage'}->{'identifier'} = 'node';
         $obj->{'manage'}->{'node_list'}  = \@entries;
      }

      return($obj);

   }
   else {

      my(@tb_header,
         @tb_rows);

      return() if ($#entries == -1);

      if ($hash{'e'}) {

         @tb_header = ("ng", "node", "ip", "PollerDisabled", "AggregateDisabled", "ctime", "mtime", "cnode", 'secondary_ng');

         my ($c) = 0;

         foreach $ref (@entries) {
            foreach (@tb_header) {
               push(@{$tb_rows[$c]}, $ref->{$_});
            }
            $c++;
         }

         print make_table(
                          \@tb_header,
                          \@tb_rows
                         );

      }
      else {

         @tb_header = ("ng", "node", "ip");

         my ($c) = 0;

         foreach $ref (@entries) {
            foreach (@tb_header) {
               push(@{$tb_rows[$c]}, $ref->{$_});
            }
            $c++;
         }

         print make_table(
                          \@tb_header,
                          \@tb_rows
                         );

      }


   }

}
sub manage_node_check_against_dns {

   my(%hash) = @_;

   my($sth,
      $ref,
      $reply);

   $sth = $dbh->prepare("SELECT id, node, ip FROM node");

   $sth->execute();

   while ($ref = $sth->fetchrow_hashref()) {

      if (`/usr/bin/host $ref->{'node'}` =~ /has address (\S+)/) {
         if ($ref->{'ip'} ne $1) {
            print "DNS for $ref->{'node'} does not match: $ref->{'ip'} ne $1\n";
            if ($hash{'u'}) {
               print "   Update now Y/N? ";
               chomp($reply = <STDIN>);
               if ($reply eq 'Y') {
                  $dbh->do("UPDATE node SET ip='$1' WHERE id='$ref->{'id'}'");
               }
               else {
                  print "Not updating record, please be aware for maximum annoyingness response is case-sensative.\n";
               }
            }
         }
      }

   }

}
sub manage_node_add {

   my(%hash) = @_;
   my($obj)  = {};

   my($sth,
      $ref,
      $remote,
      $clear_cache);

   @{$hash{'argv'}} = split(/[,!]/, $hash{'n'}) if ($hash{'n'});

   if ($#{$hash{'argv'}} == -1) {

      manage_obj($obj, function_error => "no arguments supplied to command: exiting");

      manage_output($obj);

      return($obj);

   }

   if ((!$hash{'g'}) && (!$hash{'G'})) {

      manage_obj($obj, function_error => "no primary group or secondary given: exiting");

      manage_output($obj);

      return($obj);

   }

   foreach (@{$hash{'argv'}}) {

      if ($hash{'G'}) {
         manage_node_secondary_group_add(obj => $obj, G => $hash{'G'}, n => $_); 
      }

      next if (!$hash{'g'});

      my($hostname, $remote) = split(/:/);

      if    ($remote =~ /^\d{3}\.\d{3}\.\d{3}\.\d{3}/) {
         # do nothing
      }
      elsif ($hostname =~ /^\d+$/) {
         manage_obj($obj, text => "$hostname: does not exist in DNS", attribute => $hostname, status => 'FAILURE');
         next;
      }
      elsif (!$remote) {
         $remote = join('.', unpack('C4', (gethostbyname($hostname))[4]));
      }
      elsif ($remote) {
         $remote = join('.', unpack('C4', (gethostbyname($remote))[4]));
      }

      if ($remote ne '') {

         $sth = $dbh->prepare("SELECT * FROM node WHERE node='$hostname'");

         $sth->execute();

         if ($sth->rows() == 0) {

            manage_obj($obj, text => "inserting: ng=$hash{'g'} node=$hostname ip=$remote", attribute => $hostname, status => 'SUCCESS');

            $dbh->do("INSERT INTO node SET ctime=now(), ng='$hash{'g'}', node='$hostname', ip='$remote'");

            system("/apps/epic/epic-$cfg{'module'}/sbin/discover-agent -f -s -n $hostname > /dev/null 2>&1 &");

            $clear_cache = 1;

         }
         else {

            $ref = $sth->fetchrow_hashref();

            if (($ref->{'ip'} eq $remote) && ($ref->{'ng'} eq $hash{'g'})) {

               manage_obj($obj, text => "$hostname: node exists, group and DNS IP lookup matches configuration", attribute => $hostname, status => 'SUCCESS');

               if ($ref->{'PollerDisabled'}) {
                  manage_obj($obj, text => "   enable service with '-e host' and clear-cache");
               }
               else {
                  manage_obj($obj, text => "   host already enabled if not showing up yet use, control clear-cache");
                  manage_obj($obj, text => "   and/or remoteInstall the epic client (configs snmp and api modules on client)");
               }

            }
            else {

               if ($hash{'f'}) {

                  manage_obj($obj, text => "$hostname: node exists, group and/or DNS IP lookup do not match configuration: force correcting", attribute => $hostname, status => 'SUCCESS');

                  $dbh->do("UPDATE node SET ng='$hash{'g'}', ip='$remote' WHERE node='$hostname'");

                  $clear_cache = 1;

                  next;

               }

               # Due to shell input required this section does not use manage_obj/manage_output

               print "$hostname node exists, but group and/or DNS IP lookup do not match configuration, shall I fix? [Y/N] ";

               if (<STDIN> =~ /^Y/) {
                  print "Updating record\n";
                  $dbh->do("UPDATE node SET ng='$hash{'g'}', ip='$remote' WHERE node='$hostname'");
                  $clear_cache = 1;
               } 
               else {
                  print "Not updating record, please be aware for maximum annoyingness response is case-sensative.\n";
               }

            }

            print "\n";

         }

         $sth->finish();

      }
      else {

         manage_obj($obj, text => "$hostname: does not exist in DNS", attribute => $hostname, status => 'FAILURE');

      }
   }

   if ($clear_cache) {
      update_cache('pollerd', 'reportd');
   }

   manage_output($obj);

   return($obj);

}
sub manage_node_enable {

   my(%hash) = @_;
   my($obj)  = {};

   return() if (!$hash{'feature'});

   @{$hash{'argv'}} = split(/[,!]/, $hash{'n'}) if ($hash{'n'});

   return() if (scalar @{$hash{'argv'}} == 0);

   foreach (@{$hash{'argv'}}) {

      push(@{$obj->{'manage'}->{'node_enable'}->{'text'}}, "enabled $hash{'feature'}: $_");
      push(@{$obj->{'manage'}->{'node_enable'}->{'status'}}, { $_ => SUCCESS }           );

      if (/^\d+\.\d+/) {
         $dbh->do("UPDATE node SET $hash{'feature'}Disabled='0' WHERE ip='$_'");
      }
      else {
         $dbh->do("UPDATE node SET $hash{'feature'}Disabled='0' WHERE node='$_'");
      }

   }

   # TODO - there is no check for if someone typed in a valid ip or node, so this 
   # TODO - update is going to implicitly happen even on 'mistakes'

   update_cache('pollerd') if ($hash{'feature'} eq 'Poller');
   update_cache('reportd') if ($hash{'feature'} eq 'Aggregate');

   manage_output($obj);

   return($obj);

}
sub manage_node_disable {

   my(%hash) = @_;
   my($obj)  = {};

   return() if (!$hash{'feature'});

   @{$hash{'argv'}} = split(/[,!]/, $hash{'n'}) if ($hash{'n'});

   foreach (@{$hash{'argv'}}) {

      manage_obj($obj, text => "disabled $hash{'feature'}: $_", attribute => $_, status => 'SUCCESS');

      if (/^\d+\.\d+/) {
         $dbh->do("UPDATE node SET $hash{'feature'}Disabled='1' WHERE ip='$_'");
      }
      else {
         $dbh->do("UPDATE node SET $hash{'feature'}Disabled='1' WHERE node='$_'");
      }
   }

   # TODO - there is no check for if someone typed in a valid ip or node, so this 
   # TODO - update is going to implicitly happen even on 'mistakes'

   update_cache('pollerd') if ($hash{'feature'} eq 'Poller');
   update_cache('reportd') if ($hash{'feature'} eq 'Aggregate');

   manage_output($obj);

   return($obj);

}
sub manage_node_remove {

   my(%hash) = @_;
   my($obj)  = {};

   @{$hash{'argv'}} = split(/[,!]/, $hash{'n'}) if ($hash{'n'});

   if ($hash{'G'}) {

      foreach (@{$hash{'argv'}}) {
         manage_node_secondary_group_remove(obj => $obj, G => $hash{'G'}, n => $_);
      }

      manage_output($obj);

      return;

   }
   else {

      if (($hash{'f'}) || (manage_warning(text => "This will remove the node and _all_ of it's data from the system"))) {

         foreach (@{$hash{'argv'}}) {

            manage_obj($obj, text => "$_ being removed", attribute => $_, status => 'FAILURE');

            system("/apps/epic/epic-$cfg{'module'}/sbin/inst.removeNode $_");

         }

         # TODO - there is no check for if someone typed in a valid ip or node, so this 
         # TODO - update is going to implicitly happen even on 'mistakes'

         update_cache('pollerd') if ($hash{'feature'} eq 'Poller');
         update_cache('reportd') if ($hash{'feature'} eq 'Aggregate');

         manage_output($obj);

         return($obj);

      } 
      else {

         print "Exiting due to non-compliant response\n";

         exit;

      } 

   }

}
sub manage_obj {

   # this function is a generic output function for anything meeting strict output rules such that
   # can be easily displayed either to STDOUT shell or CGI via JSON

   # status  (scalar or ARRAY)
   # text    (scalar or ARRAY)

   # function_error (scalar)

   my($obj,
      %hash) = @_;

   my($whowasi);

   $whowasi =  (caller(1))[3];
   $whowasi =~ s/\S+::manage_//;

   $obj->{'manage'}->{$whowasi}->{'function_error'} = $hash{'function_error'}                    if ($hash{'function_error'});

   push(@{$obj->{'manage'}->{$whowasi}->{'text'}},   "$hash{'text'}"                           ) if ($hash{'text'});
   push(@{$obj->{'manage'}->{$whowasi}->{'status'}}, { $hash{'attribute'} => $hash{'status'} } ) if ($hash{'attribute'});

}
sub manage_output {

   # this function is a generic output function for anything meeting strict output rules such that
   # can be easily displayed either to STDOUT shell or CGI via JSON

   # status  (scalar or ARRAY)
   # text    (scalar or ARRAY)

   # function_error (scalar)

   my($obj) = @_;

   if ($cfg{'program'} eq 'epic-cgi') {

      return();

   }
   else {

      my($whowasi);

      $whowasi =  (caller(1))[3];
      $whowasi =~ s/main::manage_//;

      print "ERROR: $obj->{'manage'}->{$whowasi}->{'function_error'}\n" if ($obj->{'manage'}->{$whowasi}->{'function_error'});

      if ($obj->{'manage'}->{$whowasi}->{'text'} =~ /ARRAY\(/) {
         foreach (@{$obj->{'manage'}->{$whowasi}->{'text'}}) {
            print "$_\n";
         }
      }
      else {
         print "$obj->{'manage'}->{$whowasi}->{'text'}\n" if ($obj->{'manage'}->{$whowasi}->{'text'});
      }

   }

}
#
# These show commands are for showing non-expensive lookups in Epic's DB's.
#
sub manage_config_table_columns {

   my(%hash) = @_;

   my(@tb_header);

   CASE: {

      @tb_header = ( "id",    "ctime",     "permanent", "ng",    "node",    "ctime" ),                                                                 last if ($hash{'t'} eq 'node_group');

      @tb_header = ( "id",    "mtime",     "permanent", "node",   "name", "proc",  "value" ),                                                          last if ($hash{'t'} eq 'configuration');
      @tb_header = ( "id",    "mtime",     "permanent", "parent", "name", "label", "href"  ),                                                          last if ($hash{'t'} eq 'epic_navbar');
      @tb_header = ( "id",    "ctime",     "etime", "permanent", "node",  "cnode" ),                                                                   last if ($hash{'t'} eq 'node_canonical');

      # snmp tables

      @tb_header = ( "id",    "mtime",     "permanent", "ng", "node", "class", "auth", "track_by_oid", "step", "rra" ),                             last if ($hash{'t'} eq 'snmp_node');
      @tb_header = ( "id",    "mtime",     "permanent", "auth","port","oid","version","version_poll","community","engineIDType","secname","seclevel","authproto","authpass","privproto","privpass" ), last if ($hash{'t'} eq 'snmp_auth');

      # auth tables

      @tb_header = ( "id",    "plugin",    "permanent", "password"               ),                                                                    last if ($hash{'t'} eq 'eapi_auth');

      @tb_header = ( "id",    "ctime",     "proc", "email_from", "email_to", "email_subject", "email_data"),                                           last if ($hash{'t'} eq 'email_sent');

      @tb_header = ( "id",    "ctime",     "permanent", "conf"),                                                                                       last if ($hash{'t'} eq 'conf_db');

      # Graphing

      @tb_header = ( "id",    "ctime",     "permanent", "gr", "template", "title", "ds", "m", "f", "u", "l", "low_water", "high_water", "stack", "o", "95th"), last if ($hash{'t'} eq 'rrd_gr');
      @tb_header = ( "id",    "ctime",     "permanent", "ds", "cdef",     "title", "units", "legend_only", "area", "stack"),                                   last if ($hash{'t'} eq 'rrd_ds');

      # Management Information Base

      @tb_header = ( "id",    "mtime",     "permanent", "class", "oid", "instance", "type", "min", "max", "alias", "ifdescr" ),                        last if ($hash{'t'} eq 'snmp_mib');
      @tb_header = ( "id",    "mtime",     "permanent", "class", "alias", "session", "request", "expect", "preparse", "length", "timeout" ),           last if ($hash{'t'} eq 'nsmt_mib');
      @tb_header = ( "id",    "mtime",     "permanent", "class", "mbean", "attribute", "alias", "type" ),                                              last if ($hash{'t'} eq 'jmx_mib');
      @tb_header = ( "id",    "mtime",     "permanent", "class", "wmi", "property", "alias", "type" ),                                                 last if ($hash{'t'} eq 'wmi_mib');

      # Data Queue 01/02

      @tb_header = ( "id",    "mtime",     "state", "node", "type", "filename" ),                                                                      last if ($hash{'t'} eq 'queue_rrd_update01');
      @tb_header = ( "id",    "mtime",     "state", "node", "type", "filename" ),                                                                      last if ($hash{'t'} eq 'queue_rrd_update02');

      # RRD definition tables

      @tb_header = ( "id",    "mtime",     "rra", "def" ),                                                                                             last if ($hash{'t'} eq 'rrd_create');
      @tb_header = ( "id",    "mtime",     "ds",  "title", "cdef", "units" ),                                                                          last if ($hash{'t'} eq 'rrd_ds');
      @tb_header = ( "id",    "mtime",     "gr", "template", "title", "ds", "m", "f", "u", "l", "low_water", "high_water", "stack", "o" ),             last if ($hash{'t'} eq 'rrd_gr');

      # Reporting tables

      @tb_header = ( "id",    "mtime",     "permanent", "ng", "node", "ds", "rollup" ),                                                                last if ($hash{'t'} eq 'nms_report_rule');

      return();

   }

   return(@tb_header);

}
sub manage_config_modify {

   my(%hash) = @_;

   my($sth,
      $ref,
      @tb_header,
      %tb_header,
      %pairs);

   @tb_header = manage_config_table_columns(t => $hash{'t'});
   %tb_header = map { $_ => 1 } @tb_header;

   if ($#tb_header == -1) {
      print "table not defined in management library, exiting.\n";
      exit;
   }

   %pairs = get_config_pairs($hash{'c'});

   $pairs{'permanent'} = '1'; # force all rows modified to be permanent

   if ((keys %pairs == 0) || ($hash{'c'} eq '')) {
      print "unable to parse config, exiting.\n";
      exit;
   }

   foreach (keys %pairs) {
      if (!$tb_header{$_}) {
         print "column '$_' not defined for table $hash{'t'}, exiting.\n";
         exit;
      }
   }

   if ($hash{'verbose'}) {
      debug("manage_config_modify: table $hash{'t'}");
   }

   if    ($hash{'feature'} eq 'insert') {

      config_insert(table => $hash{'t'}, %pairs);

   }
   elsif ($hash{'feature'} eq 'update') {

      config_update(table => $hash{'t'}, %pairs);

   }
   elsif ($hash{'feature'} eq 'delete') {

      config_delete(table => $hash{'t'}, %pairs);

   }

}
sub manage_config_select {

   my(%hash) = @_;

   my($sth,
      $ref,
      $WHERE,
      $ORDERBY,
      @tb_header,
      @tb_rows,
      @tb_data,
      @entries,
      $obj,
      $tb);

   @tb_header = manage_config_table_columns(t => $hash{'t'});

   return(0) if ($#tb_header == -1);

   $hash{'l'} = ($hash{'l'}) ? $hash{'l'} : $cfg{'manage:config_select_limit'};

   if ($hash{'o'}) {

      return(0) if ($hash{'o'} =~ /;/);

      $ORDERBY = "ORDER BY $hash{'o'}";

   }

   if ($hash{'w'}) {

      return(0) if ($hash{'w'} =~ /;/);

      $WHERE = "WHERE $hash{'w'}";

   }

   if ($hash{'feature'} eq 'count') {

      $sth = $dbh->prepare("SELECT count(*) count FROM $hash{'t'} $WHERE $ORDERBY");

      @tb_header = ('count');

   }
   else {
      $sth = $dbh->prepare("SELECT * FROM $hash{'t'} $WHERE $ORDERBY LIMIT $hash{'l'}");
   }

   $sth->execute();

   while ($ref = $sth->fetchrow_hashref()) {

      next if ($ref->{'name'} =~ /auth/);

      $ref->{'permanent'} = $ref->{'permanent'} ? 'yes' : 'no';

      push(@entries, $ref);

   }

   $sth->finish();

   if ($cfg{'program'} eq 'epic-cgi') {

      if ($hash{'dojo'}) {

         $obj->{'identifier'} = 'id';
         $obj->{'numRows'}    = scalar @entries;

         if (($hash{'start'} ne '') && ($hash{'count'} ne '')) {
            foreach (0 ... ($hash{'start'} - 1)) {
               shift(@entries);
            }
            $#entries = ($hash{'count'} - 1) if ($hash{'count'} < scalar @entries);
         }

         $obj->{'items'} = \@entries;

      }
      else {
         $obj->{'manage'}->{'identifier'}  = 'id';
         $obj->{'manage'}->{$hash{'t'}}    = \@entries;
      }

      return($obj);

   }
   else {

      print "WARNING: config select -t $hash{'t'} returned default limit rows; suggest using -l to increase limit.\n" if (( -t STDOUT ) && (scalar @entries == $cfg{'manage:config_select_limit'}));

      return() if ($#entries == -1);

      my ($c) = 0;

      foreach $ref (@entries) {
         foreach (@tb_header) {
            push(@{$tb_rows[$c]}, $ref->{$_});
         }
         $c++;
      }

      print make_table(
                       \@tb_header,
                       \@tb_rows
                      );


   }

}
sub manage_config_set {

   my(%hash) = @_;

   my($sth,
      $ref,
      $WHERE,
      $ORDERBY,
      @tb_header,
      %pairs);

   %pairs = get_config_pairs($hash{'c'});

   $pairs{'permanent'} = '1'; # force all rows modified to be permanent

   if ((keys %pairs == 0) || ($hash{'c'} eq '')) {
      print "unable to parse config, exiting.\n";
      exit;
   }

   @tb_header = manage_config_table_columns(t => $hash{'t'});

   return(0) if ($#tb_header == -1);

   if ($hash{'w'}) {

      return(0) if ($hash{'w'} =~ /;/);

      $WHERE = "WHERE $hash{'w'}";

   }

   $sth = $dbh->prepare("SELECT * FROM $hash{'t'} $WHERE $ORDERBY");

   $sth->execute();

   if ($sth->rows() > 1) {
      print "error during set, where clause returned more than one row.\n";
      return();
   }
   elsif ($sth->rows() == 0) {
      print "error during set, where clause returned zero rows.\n";
      return();
   }

   $ref = $sth->fetchrow_hashref();

   $sth->finish();

   $pairs{'id'} = $ref->{'id'};

   config_update(table => $hash{'t'}, %pairs);

   manage_config_select (%hash, argv => \@ARGV);

}
1;
