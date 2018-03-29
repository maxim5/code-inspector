package IDS::Test;
$IDS::Test::VERSION = "1.0";

=head1 NAME

IDS::Test - An IDS test framework

=head1 SYNOPSIS

A usage synopsis would go here.  Since it is not here, read on.

=head1 DESCRIPTION

Create an IDS algorithm instance and do training and/or testing with
it.

=cut

use lib "/home/ingham/CS/phd/http/perl";
use strict;
use warnings;
use Carp qw(cluck carp confess);
use IDS::Algorithm;
use IDS::DataSource;
use IDS::Utils qw(fh_or_stdout);
use Getopt::Long;
use IO::Handle;

=over

=item new(datasource, algorithm)

=item new(datasource, algorithm, GetOpt::Long options)

Set up a new test object with the data source and algorithm specified.  

NOTES:  If the calling program needs to do command-line argument
processing, it must do it through this call.  Any GetOpt::Long options
supplied override those in the data source and algorithm; beware to ensure
that options are unique or that this behavior is what you want.

The data source and algorithm cannot be specified by command-line arguments;
the getopt processing happens after the object creation, and it must be
this way since other command-line args are data source- or algorithm-specific.

=back

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $source = shift or
        confess *new{PACKAGE} . "::new called without data source";
    my $algorithm = shift or
        confess *new{PACKAGE} . "::new called without algorithm";

    my $self = { };
    bless $self, $class;

    $self->default_parameters;

    $self->{"source_class"} = $source;
    $self->{"source"} = IDS::DataSource::new("IDS::DataSource::$source");
    $self->{"source"}->default_parameters;
    $self->{"source"}->parameters("msg_fh", *STDERR);

    $self->{"algorithm_class"} = $algorithm;
    $self->{"algorithm"} = IDS::Algorithm::new("IDS::Algorithm::$algorithm");
    $self->{"algorithm"}->default_parameters;
    $self->{"algorithm"}->parameters("msg_fh", *STDERR);

    $self->{"fpclasses"} = [];
    $self->{"fpclass"} = {};
    $self->{"fpcount"} = [];
 
    GetOptions($self->{"algorithm"}->param_options,
               $self->{"source"}->param_options, 
	       $self->param_options, 
	       @_ ); # allow local options for our caller

    return $self;
}

=over

=item parameters()

=item parameters(param)

=item parameters(param, value, ...)

Set and/or retrieve the current parameters (individual or group).  These
parameters may apply to the data source or to the algorithm (the assumption
is that either they are unique, or have the same meaning).  The return
value is always an hash (actually, an array of key-value pairs).

=back

=cut

sub parameters {
    my $self = shift;

    # All parameters
    if ($#_ == -1) {
        return ( %{$self->{"params"}},
	         $self->{"algorithm"}->parameters, 
	         $self->{"source"}->parameters);
    }

    # A specific parameter
    if ($#_ == 0) {
        return ( $_[0], $self->{"algorithm"}->parameters($_[0]),
	         $_[0], $self->{"source"}->parameters($_[0]));
    }

    # Set the parameter(s) in the data source and algorithm.
    for (my $i = 0; $i < $#_; $i+=2) {
        $self->{"algorithm"}->parameters($_[$i], $_[$i+1]);
    }
    return ();
}

=over

=item print_parameters()

=item print_parameters(filehandle)

=back

Print the values of all of the parameters (useful for recording the
configuration used for a training or test run).  If a filehandle is
provided, it is the destination for the dump.  Otherwise, the parameters
are printed to stdout.

=cut

sub print_parameters {
    my $self = shift;
    my $fh = fh_or_stdout(shift);
    my (%params, $k);

    print $fh "Test object:\n";
    %params = %{$self->{"params"}};
    for $k (sort keys %params) {
	defined($params{$k}) or $params{$k} = "undef";
        print $fh "    $k: ", $params{$k}, "\n";
    }

    print $fh "Data source: ", $self->{"source_class"}, "\n";
    %params = $self->{"source"}->parameters();
    for $k (sort keys %params) {
	defined($params{$k}) or $params{$k} = "undef";
        print $fh "    $k: ", $params{$k}, "\n";
    }

    print $fh "Algorithm: ", $self->{"algorithm_class"}, "\n";
    %params = $self->{"algorithm"}->parameters();
    for $k (sort keys %params) {
	defined($params{$k}) or $params{$k} = "undef";
        print $fh "    $k: ", $params{$k}, "\n";
    }
    print $fh "\n";
}

=over

=item param_options()

Returns the command-line parameter processing options.

=item default_parameters()

Set the default values for parameters that may be modified by
command-line options.

=back

For an IDS::Test object, the command-line options are:

=over

=item test_verbose

Whether and how much debugging output to produce.

=item clean_interval

How often, measured in number of data source items processed, should
IDS::Algorithm::clean be called.  0 means to not even try to call it.
Cleaning may be a form of generalization, or it may be something else
(it is up to the algorithm to determine the meaning).

Cleaning occurs in foreach(), so it can occur for training, testing,
or both.

=item generalize

Whether or not to call IDS::Algorithm::generalize() at the end of the
training.  The meaning of generalization is up to the algorithm.
Generalization occurs after all training, and before saving.

=item add_threshold

How similar should a test result be before adding in a tested data
source item.  0 means no adding will be attempted.  If the test results
in a similarity value >= the threshold, then the item will be added.

This option is one way to allow the IDS algorithm to handle a
non-stationary environment.

One implication of this dynamic adding is if add_threshold > 0, then
the algorithm *must* support adding into a loaded state.  It also implies
that you probably want to save the resulting state at the end of the
test run.

=item normal_threshold

Results below this value are considered abnormal.  This value only has
meaning if group_abnormal or try_alternates is enabled.

=item group_abnormal

If an item is considered abnormal, then try to group it with others
to reduce the overall false positive rate.  See the definition for
normal_threshold for the definition of abnormal.  The idea is that a
human admin could look at one example from a group and decide if it is
normal or not.  I added this ability after dealing with a web robot that
did not exist in the training data, and came online in the test data.
By grouping, I was able to cut the false positive count from nearly
10,000 for a week (for the web robot alone) to 22 for a week (considering
all sources).

=item abnormal_threshold

This parameter only has meaning if group_abnormal is enabled.

If an item is considered abnormal, this parameter controls how closely
should it match other items to be considered part of the class of false
positives.  Range is 0-1.

=item try_alternates

If an item is considered abnormal, see if the source can provide an
alternate version of the item that may be more normal.  I developed
this heuristic to try deleting various high-variability lines from a
HTTP request in an attempt to make it more normal (and hence
acceptable).  

IDS::DataSource::alternate() must return either a two-element list
(label, ref) consisting of a label describing the new object and a new
IDS::DataSource or undef.

=item add_alternates

Learn the alternate version if the similarity value of the alternate
version is >= add_threshold. 

=item twopass

The training process requires two passes across the training data, with
the second pass function being next_pass.

Biased comment: one-pass algorithms make for easier implementation in
real systems, especially ones that have nonstationary data.

=item learning_curve

Produce output during training showing how similar the added instance is
to the existing model.  This data may be useful for producing a learning
curve for the algorithm.

=back

=cut

sub param_options {
    my $self = shift;

    return (
	"test_verbose=i"     => \${$self->{"params"}}{"verbose"},
	"normal_threshold=f" => \${$self->{"params"}}{"normal_threshold"},
	"add_threshold=f"    => \${$self->{"params"}}{"add_threshold"},
	"group_abnormal!"    => \${$self->{"params"}}{"group_abnormal"},
	"abnormal_threshold=f"=> \${$self->{"params"}}{"abnormal_threshold"},
	"generalize!"        => \${$self->{"params"}}{"generalize"},
	"twopass!"           => \${$self->{"params"}}{"generalize"},
	"clean_interval=i"   => \${$self->{"params"}}{"clean_interval"},
	"try_alternates!"    => \${$self->{"params"}}{"try_alternates"},
	"add_alternates!"    => \${$self->{"params"}}{"add_alternates"},
	"learning_curve!"    => \${$self->{"params"}}{"learning_curve"},
	"learning_save_interval=i" => \${$self->{"params"}}{"learning_save_interval"},
	"learning_dir=s"     => \${$self->{"params"}}{"learning_dir"},
	"stop_after=i"       => \${$self->{"params"}}{"stop_after"},
   );
}

sub default_parameters {
    my $self = shift;
    $self->{"params"} = {
	"verbose"              => 0,
	"normal_threshold"     => 0,
	"add_threshold"        => 0,
	"group_abnormal"       => 0,
	"abnormal_threshold"   => 0.6,
	"generalize"           => 0,
	"twopass"              => 0,
	"clean_interval"       => 0,
	"try_alternates"       => 0,
	"add_alternates"       => 0,
	"learning_curve"       => 0,
	"learning_save_interval" => 0,
	"learning_dir"         => undef,
	"stop_after"           => 0,
    };
}

=over

=item train()

=item train(save)

Run the training process by calling IDS::Algorithn::add for each entry
returned by the IDS::DataSource.

If save is defined and "true" (in the perl sense of truth), it is the
file name or IO::Handle where the algorithm should save its state.

If curve is defined and "true" (in the perl sense of truth), then before
each instance is added, it is first tested.  The result is printed, and
can be used to produce a learning curve for the algorithm.

=back

=cut

sub train {
    my $self = shift;
    my $save = shift;
    my $curve = shift;

    my $generalize = ${$self->{"params"}}{"generalize"};

    my $addref = $self->{"algorithm"}->can("add");
    defined($addref) or
        confess "Algorithm ", $self->{"algorithm_class"}, " cannot add.";

    if (${$self->{"params"}}{"learning_curve"}) {
        $self->{"addref"} = $addref;
	$self->{"testref"} = $self->{"algorithm"}->can("test");
	my $funcref = \&learning_curve;
	$self->foreach($funcref, $self, 1, "train");
    } else {
	$self->foreach($addref, $self->{"algorithm"}, 0, "train");
    }

    if (defined($generalize) && $generalize) {
	confess "Algorithm ", $self->{"algorithm_class"},
	        " cannot generalize, but generalization was requested.\n"
	    unless $self->{"algorithm"}->can("generalize");
	$self->{"algorithm"}->generalize();
    }

    $self->{"algorithm"}->save($save) if defined($save) && $save;
}

=over

=item learning_curve(tokenref, string, obs)

Produce a learning curve by testing each instance before it is added to the
model.  The similarity value of the test instance is the return value, which
can be printed by foreach.  This function is primarily for internal use.

This function can also save the model every n observations.

=back

=cut

sub learning_curve {
    my $self = shift;
    my $tokens = shift;
    my $string = shift;
    my $n = shift;
    
    my $result = $self->testfunc($tokens, $string, $n);
    my $addref = $self->{"addref"};
    &{$addref}($self->{"algorithm"}, $tokens, $string, $n);

    # save the model if requested and it is time
    if (defined(${$self->{"params"}}{"learning_dir"}) &&
        defined(${$self->{"params"}}{"learning_save_interval"}) &&
        ${$self->{"params"}}{"learning_save_interval"}) {
	
	if ($n % ${$self->{"params"}}{"learning_save_interval"} == 0) {
	    ### Note no generalization call. See test() for alternate example
	    my $dest = ${$self->{"params"}}{"learning_dir"} . "/$n";
	    $self->{"algorithm"}->save($dest);
	}
    }

    # stop if requested and it is time
    if (${$self->{"params"}}{"stop_after"} &&
        $n >= ${$self->{"params"}}{"stop_after"}) {
	    ### a bit harsh?
	    exit(0);
    }

    return $result;
}

=over

=item test()

=item test(save?)

Run the testing process.  The if save is defined, it is the file name
(or handle?) where the algorithm should save its state.

=back

=cut

sub test {
    my $self = shift;
    my $save = shift;
    my ($scpu, $acpu, $n_processed);

    my $generalize = ${$self->{"params"}}{"generalize"};

    # Set up function references for the test function to use.
    my $testref = $self->{"algorithm"}->can("test");
    defined($testref) or
        confess "Algorithm ", $self->{"algorithm_class"}, " cannot test.";
    $self->{"testref"} = $testref;

    my $addref = $self->{"algorithm"}->can("add");
    defined($addref) or
        confess "Algorithm ", $self->{"algorithm_class"}, " cannot add.";
    $self->{"addref"} = $addref;

    # Load algorithm state; note that the load function must have a
    # default or cmdline param for the file to load.  We certainly have
    # no idea how to handle it here.
    $self->{"algorithm"}->load();

    $n_processed = $self->foreach(\&testfunc, $self, 1, "test");
    print "Processed $n_processed for test\n";

    if (defined($save) && $save) {
        # no use generalizing if we are not going to save
	if (defined($generalize) && $generalize) {
	    confess "Algorithm ", $self->{"algorithm_class"},
		    " cannot generalize, but generalization was requested.\n"
		unless $self->{"algorithm"}->can("generalize");
	    $self->{"algorithm"}->generalize();
	}

	$self->{"algorithm"}->save($save);
    }

    # summarize the group_abnormal if it is enabled
    if (${$self->{"params"}}{"group_abnormal"}) {
        print scalar(@{$self->{"fpclasses"}}), " FP classes generated\n";
    }
}

=over 

=item testfunc(tokenref, data, instance)

tokenref is a reference to a list of tokens representing the current
item.

data is a string representation of the current item.

instance is an instance number.

This function is for use within the IDS::Test object and not for general
use.

A testing function, called once per instance to test.  This function is
broken out of the actual test routine because we may need to do various
things when testing that we may not need to do in learning.  Examples
include handling the parameters add_threshold and try_alternates.

=back

=cut

sub testfunc {
    my $self = shift;
    my $tref = shift or
        confess *testfunc{PACKAGE} . "::testfunc missing tref\n";
    my $data = shift or
        confess *testfunc{PACKAGE} . "::testfunc missing data\n";
    my $n = shift or
        confess *testfunc{PACKAGE} . "::testfunc missing n\n";
    
    my ($result, $msg_fh, $best_result, $best_obj, $best_label);
    my ($try_result, $obj, $label);
    my $verbose = ${$self->{"params"}}{"verbose"};

    $msg_fh = ${$self->{"params"}}{"msg_fh"};

    $result = &{$self->{"testref"}}($self->{"algorithm"}, $tref, $data, $n);

    # Add the instance in as normal if it is close enough to normal
    if (${$self->{"params"}}{"add_threshold"} > 0 &&
        $result >= ${$self->{"params"}}{"add_threshold"}) {
	    &{$self->{"addref"}}($self->{"algorithm"}, $tref, $data, $n);
    }

    # if not normal, try alternate versions
    if (${$self->{"params"}}{"try_alternates"}) {
	$best_result = -1; $best_label = "None tried";
	if ($result < ${$self->{"params"}}{"normal_threshold"}) {
	    while ((($label, $obj) = $self->{"source"}->alternate) &&
	           defined($obj)) {
		$try_result = &{$self->{"testref"}}($self->{"algorithm"},
				scalar($obj->tokens),
				scalar($obj->data), $n);
		if ($best_result < $try_result) {
		    $best_result = $try_result;
		    $best_label = $label;
		    $best_obj = $obj;
		}
	    }
	    # Print the improved result, if it is notable.
	    print $self->{"source"}->source,
		  " alternate $best_label orig $result now $best_result\n"
		  if $best_result > $result;
	}

	# Add the instance in as normal if we should and it is close
	# enough to normal
	if (${$self->{"params"}}{"add_alternates"} && 
	    $best_result > $result &&
	    ${$self->{"params"}}{"add_threshold"} > 0 &&
	    $best_result >= ${$self->{"params"}}{"add_threshold"}) {
		&{$self->{"addref"}}($self->{"algorithm"},
		                     scalar($best_obj->tokens),
				     scalar($best_obj->data), $n);
	}

	$result = $best_result if $best_result > $result;
    }

    # if still not normal, try grouping abnormal
    if (${$self->{"params"}}{"group_abnormal"} && 
        $result < ${$self->{"params"}}{"normal_threshold"}) {

	my ($fpc, $fpsim, $fpclass, $fpbest_val, $fpbest_class, $fpbest_cat);

	print $self->{"source"}->source, " not normal, try grouping\n" if $verbose;

	# Find a best match (if one exists)
	$fpbest_val = 0;
	for $fpc (@{$self->{"fpclasses"}}) {
	    $fpsim = &{$self->{"testref"}}($self->{"algorithm"}, $tref, $data, $n);
	    if ($fpsim > $fpbest_val) {
	        $fpbest_val = $fpsim;
		$fpbest_class = ${$self->{"fpclass"}}{$fpc};
		$fpbest_cat = $fpc;
	    }
	}

	if ($fpbest_val > ${$self->{"params"}}{"abnormal_threshold"}) {
	    # best match was good enough, use it.
	    $fpbest_cat->add($tref, $data, $n);
	    ${$self->{"fpcount"}}[$fpbest_class]++;
	    print "    FP class $fpbest_class\n";
	} else {
	    # best match was not good enough, create a new class
	    if ($verbose) {
		my $c = @{$self->{"fpclasses"}};
		print "No existing group; count now $c\n";
	    }
	    $fpc = $self->{"algorithm"}->new;
	    $fpc->parameters($self->{"algorithm"}->parameters());
	    $fpc->add($tref, $data, $n);
	    push @{$self->{"fpclasses"}}, $fpc;
	    $fpclass = $#{$self->{"fpclasses"}};
	    ${$self->{"fpcount"}}[$fpclass] = 1;
	    ${$self->{"fpclass"}}{$fpc} = $fpclass;
	    print "    FP class $fpclass (new)\n";
	}
    }

    return $result;
}

=over

=item foreach(funcref, funcobj, print?, type)

For each item in the data source, call the function referenced by
funcref, which is a method for the funcobj.  The call is:
funcref(funcobj, tokens, string, n)

Print indicates whether or not to print a result line for this call.
Normally, you would want to print when testing and not when learning.

Type indicates whether this is a training or a testing foreach.  The
variable is passed without intepretation to the IDS::Algorithm::clean
function.

=back

=cut

sub foreach {
    my $self = shift;
    my $funcref = shift;
    defined($funcref) or
        confess *foreach{PACKAGE} . "::foreach missing function reference";
    my $obj = shift;
    defined($obj) or
        confess *foreach{PACKAGE} . "::foreach missing function object";
    my $print = shift;
    my $type = shift;

    my $clean_interval = ${$self->{"params"}}{"clean_interval"};

    my ($fh, $result, $param, $n);

    $fh = new IO::Handle;
    $fh->fdopen(fileno(STDIN),"r") or
	confess "Unable to fdopen STDIN: $!\n";

    $n = 1;
    while ($self->{"source"}->read_next($fh)) {
	$result = &{$funcref}($obj, scalar($self->{"source"}->tokens),
	                      scalar($self->{"source"}->data), $n);

	print $self->{"source"}->source, ": $result\n" if $print;

	# Clean if we should
	if ($clean_interval > 0 && $n % $clean_interval == 0) {
	    if ($self->{"algorithm"}->can("clean")) {
		$self->{"algorithm"}->clean($type)
	    } else {
		confess "Algorithm ", $self->{"algorithm_class"},
			" cannot clean, but cleaning was requested.\n"
	    }
	}

        $n++;
    }
    return $n - 1;
}

=head1 AUTHOR INFORMATION

Copyright 2005-2007, Kenneth Ingham.  All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

Address bug reports and comments to: ids_test at i-pi.com.  When sending
bug reports, please provide the versions of IDS::Test.pm, IDS::Algorithm.pm,
IDS::DataSource.pm, the version of Perl, and the name and version of the
operating system you are using.  Since Kenneth is a PhD student, the
speed of the reponse depends on how the research is proceeding.

=head1 BUGS

Please report them.

=head1 SEE ALSO

L<IDS::Algorithm>, L<IDS::DataSource>

=cut

1;
