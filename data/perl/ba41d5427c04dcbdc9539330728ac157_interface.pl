#
# interface.pl:
#
#       Author:
#

### FIXME.
#if (&IsParam("useStrict")) { use strict; }

sub whatInterface {
    if (!&IsParam("Interface") or $param{'Interface'} =~ /IRC/) {
	return "IRC";
    } else {
	return "CLI";
    }
}

sub cliloop {
    &status("Using CLI...");
    &status("Now type what you want.");

    $nuh = "local!local\@local";
    $uh  = "local\@local";
    $who = "local";
    $orig{who} = "local";
    $ident = $param{'ircNick'};
    $talkchannel = "#CLI";
    $addressed = 1;

    print ">>> ";
    while (<STDIN>) {
	$orig{message} = $_;
	$_ = &process("local", 'public', $_);
	print ">>> ";
    }
}

1;
