package Module::Implementation;

use strict;
use warnings;

use Module::Runtime 0.012 qw( require_module );
use Try::Tiny;

my %Implementation;

sub build_loader_sub {
    my $caller = caller();

    return _build_loader( $caller, @_ );
}

sub _build_loader {
    my $package = shift;
    my %args    = @_;

    my @implementations = @{ $args{implementations} };
    my @symbols = @{ $args{symbols} || [] };

    my $implementation;
    my $env_var = uc $package;
    $env_var =~ s/::/_/g;
    $env_var .= '_IMPLEMENTATION';

    return sub {
        my ( $implementation, $found ) = _load_implementation(
            $package,
            $ENV{$env_var},
            \@implementations,
        );

        $Implementation{$package} = $implementation;

        _copy_symbols( $found->{$implementation}, $package, \@symbols, $found );

        return $found->{$implementation};
    };
}

sub implementation_for {
    my $package = shift;

    return $Implementation{$package};
}

sub _load_implementation {
    my $package         = shift;
    my $env_value       = shift;
    my $implementations = shift;

    if ($env_value) {
        _croak( "$env_value is not a valid implementation for $package" )
            unless grep { $_ eq $env_value } @{$implementations};

        my $requested = "${package}::$env_value";

        # Values from the %ENV hash are tainted. We know it's safe to untaint
        # this value because the value was one of our known implementations.
        ($requested) = $requested =~ /^(.+)$/;

        if (my $exc = _module_load_exception($requested)) {
            _croak("Could not load $requested: $exc");
        };

        return ( $env_value, { $env_value => $requested } );
    }
    else {
        my ($err, $first_choice, $found);

        for my $possible ( @{$implementations} ) {
            my $try = "${package}::$possible";

            if (my $exc = _module_load_exception($try)) {
                $err .= "\n$exc";
            }
            else {
                $first_choice = $possible unless defined $first_choice;
                $found->{$possible} = $try;
            }
        }

        return ( $first_choice, $found ) if defined $first_choice;

        _croak(
            "Could not find a suitable $package implementation: $err"
        );
    }
}

sub _module_load_exception {
    my $mod = shift;
    try {
        require_module($mod);
        return;
    }
    catch {
        my $internal_error;

        # weird pathological cases, originally claimed here:
        # http://git.urth.org/cgit.cgi/Module-Implementation.git/commit/?id=bbaa0bac
        if (! defined $_) {
            $internal_error = 'left $@ undefined';
        }
        elsif (! $_) {
            $internal_error = "threw the false exception '$_'";
        }
        else {
            return $_;
        }

        # we had an internal error if we got this far
        return sprintf (
            'Module::Runtime::require_module() failed to load %s but %s. '
          . 'Theoretically this can not happen, and is a likely indication '
          . 'of a bug in Module::Runtime. Please contact the author of '
          . 'either Module::Implementation or Module::Runtime to investigate '
          . 'this further.'
        , $mod, $internal_error );
    };
}

my %globslot_check = (
    '@' => 'ARRAY',
    '%' => 'HASH',
    '&' => 'CODE',
);

sub _copy_symbols {
    my $from_package = shift;
    my $to_package   = shift;
    my $symbols      = shift;
    my $found        = shift;

    my %symbols;
    for my $sym (@$symbols) {
        if (ref $sym eq 'Regexp') {
            for my $mod (values %$found) {
                no strict 'refs';
                $symbols{$_}++ for grep {
                    $_ =~ $sym and defined *{"${mod}::$_"}{CODE}
                } keys %{"${mod}::"}
            }
        }
        else {
            $symbols{$sym}++;
        }
    }

    for my $sym ( keys %symbols ) {
        my $type = $sym =~ s/^([\$\@\%\&\*])// ? $1 : '&';

        my $from = "${from_package}::$sym";
        my $to   = "${to_package}::$sym";

        {
            no strict 'refs';
            no warnings 'once';

            if ( $globslot_check{$type} ) {

                my %missing_in_implementation = map {
                    ( defined *{"${_}::$sym"}{$globslot_check{$type}} )
                      ? ()
                      : ( $_ => 1 )
                } values %$found;

                _croak (
                    "Can't copy nonexistent symbol $type$sym from $from_package to $to_package"
                ) if $missing_in_implementation{$from_package};

                _croak (
                    "Symbol import mismatch - $type$sym does not exist in alternative implementation(s): "
                  .  join ', ', sort keys %missing_in_implementation
                ) if keys %missing_in_implementation;
            }

            # Copied from Exporter
            *{$to}
                = $type eq '&' ? \&{$from}
                : $type eq '$' ? \${$from}
                : $type eq '@' ? \@{$from}
                : $type eq '%' ? \%{$from}
                : $type eq '*' ? *{$from}
                : _croak(
                    "Can't copy symbol from $from_package to $to_package: $type$sym"
                );
        }
    }
}

sub _croak {
    require Carp;
    Carp::croak(@_);
}


1;

# ABSTRACT: Loads one of several alternate underlying implementations for a module

__END__

=head1 SYNOPSIS

  package Foo::Bar;

  use Module::Implementation;

  BEGIN {
      my $loader = Module::Implementation::build_loader_sub(
          implementations => [ 'XS',  'PurePerl' ],
          symbols         => [ 'run', 'check', qr/^start_/ ],
      );

      $loader->();
  }

  package Consumer;

  # loads the first viable implementation
  use Foo::Bar;

=head1 DESCRIPTION

This module abstracts out the process of choosing one of several underlying
implementations for a module. This can be used to provide XS and pure Perl
implementations of a module, or it could be used to load an implementation for
a given OS or any other case of needing to provide multiple implementations.

This module is only useful when you know all the implementations ahead of
time. If you want to load arbitrary implementations then you probably want
something like a plugin system, not this module.

=head1 API

This module provides two subroutines, neither of which are exported.

=head2 Module::Implementation::build_loader_sub(...)

This subroutine takes the following arguments.

=over 4

=item * implementations

This should be an array reference of implementation names. Each name should
correspond to a module in the caller's namespace.

In other words, using the example in the L</SYNOPSIS>, this module will look
for the C<Foo::Bar::XS> and C<Foo::Bar::PurePerl> modules.

This argument is required.

=item * symbols

A list of symbols to copy from the implementation package to the calling
package.

These can be prefixed with a variable type: C<$>, C<@>, C<%>, C<&>, or
C<*)>. If no prefix is given, the symbol is assumed to be a subroutine (C<&>).

You can also specify one or several regular expressions (via C<qr//>). In
this case all implementation namespaces will be scanned for B<subroutine
names> matching at least one of the regexes, and matches will be imported
as if they were listed explicitly.

This argument is optional.

=back

This subroutine I<returns> the implementation loader as a sub reference.

It is up to you to call this loader sub in your code.

I recommend that you I<do not> call this loader in an C<import()> sub. If a
caller explicitly requests no imports, your C<import()> sub will not be run at
all, which can cause weird breakage.

=head2 Module::Implementation::implementation_for($package)

Given a package name, this subroutine returns the implementation that was
loaded for the package. This is not a full package name, just the suffix that
identifies the implementation. For the L</SYNOPSIS> example, this subroutine
would be called as C<Module::Implementation::implementation_for('Foo::Bar')>,
and it would return "XS" or "PurePerl".

=head1 HOW THE IMPLEMENTATION LOADER WORKS

The implementation loader works like this ...

First, it checks for an C<%ENV> var specifying the implementation to load. The
env var is based on the package name which loads the implementations. The
C<::> package separator is replaced with C<_>, and made entirely
upper-case. Finally, we append "_IMPLEMENTATION" to this name.

So in our L</SYNOPSIS> example, the corresponding C<%ENV> key would be
C<FOO_BAR_IMPLEMENTATION>.

If this is set, then the loader will B<only> try to load this one
implementation.

If the env var requests an implementation which doesn't match one of the
implementations specified when the loader was created, an error is thrown.

If this one implementation fails to load then loader throws an error. This is
useful for testing. You can request a specific implementation in a test file
by writing something like this:

  BEGIN { $ENV{FOO_BAR_IMPLEMENTATION} = 'XS' }
  use Foo::Bar;

If the environment variable is I<not> set, then the loader simply tries the
implementations originally passed to C<Module::Implementation>. The
implementations are tried in the order in which they were originally passed.

The loader will use the first implementation that loads without an error. It
will copy any requested symbols from this implementation.

If none of the implementations can be loaded, then the loader throws an
exception.

The loader returns the name of the package it loaded.
