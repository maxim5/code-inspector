#Copyright (c) 1997-2000, Damian Conway. All Rights Reserved.
#Copyright (c) 2000-2001, C. Garrett Goebel. All Rights Reserved.

# this code is a modified version from conway, modified by mdupont 
#################################################################
# Changes by    : James Michael DuPont
# Status        :  to update
# Generation    :  second Generation
# Category      :  Perl Meta Programmming
# 
#
# LICENCE STATEMENT
#    This file is part of the GCC XML Node Introspector Project
#    Copyright (C) 2001-2002  James Michael DuPont
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.     
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.     
# 
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#    Or see http://www.gnu.org/licenses/gpl.txt
###############################################################################

package Class::Contract; 

# $attr->{'gentype'}  can be (SCALAR,ARRAY,HASH) and is used to implemnt get and set checking for attributes
#           $attr->{'post'}, # the array of 
#           $attr_ref,       # a reference to the value
#           $name,           # the name of the attreiubte
#package Contract::PostSCALAR;
#package Contract::PostARRAY;
#package Contract::PostHASH;
#        Contract::__ANON__
#        Contract::FormerState # sub check

#  ########################################################################
#  # Used by class contract
#  ########################################################################
#  our $attr;                # attributes
#  our $classname;           #
#  our $clause;              #
#  our $ctor;                #
#  our $current;             # Current object field  that is being defined in  the contract
#  our $depth;     # dcopy
#  our @class_dtors;
#  our @context;
#  our @value;

# spec->{msg}   # message
# spec->{loc}   # location
# spec->{code}  # code
# spec->{opt}   # code
# spec->{owner} # code
# spec->{parents} # code
#    bless {'name'     => $name,# NAME OF THE MEMBER
#           'type'     => $type || $def_type{$kind},  # type of the NODE, the default for an attribute is SCALAR
#$attr->{$attrname}{'type'})   # ARRAY,HASH,SCALAR,OBJECT

#           'gentype'  => $type || $def_type{$kind},  # NOT IN PRODUCTION
#$attr->{$attrname}{'type'})   # ARRAY,HASH,SCALAR,OBJECT

#           'loc'      => $location, # LINE NUMBER

#           'shared'   => 0, # BOOL
#$attr->{$attrname}{'shared'}) # BOOL
# is a class member or a object member

#           'private'  => 0, # BOOL
#           'abstract' => 0, # BOOL
#           'pre'      => 
#           'post'     => 
#          }, "Contract::$kind";

# these are the different "KIND" of nodes
#Contract::attr
#Contract::current
#Contract::ctor
#Contract::dtor
#Contract::clone


#$contract{$classname}{"attr"} # HASHREF
#      $class_spec->{
#                    $kind       # What kind of member (attr)
#                   }{
#                     $name      # the name of member
#                    }{
#                      $type     # the type of member
#                     }

# the set of contracts
#  if (defined $contract{$owner      # the name of the class
#			}{$kind     # the kind of member
#			  }{$name   # name of the field
#			    }) { # DUPLICATE CHECK




use strict;
use vars qw( $VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS );
require Exporter;
use Carp;
use Introspector::DebugPrint;
$VERSION = '1.14';

@ISA = qw(Exporter);
##### INCLUDES INTROSPECTING METHODS from MDUPONT
@EXPORT = qw(contract ctor dtor attr method pre impl post invar inherits
             self value class abstract private optional check callstate
             failmsg clon 
	     GetContracts 
	     GetOwnContract
	     IsAbstract 
	     GetParentNames GetParents
	     GetDestructors 
	     GetConstructors
	     GetMethodNames GetMethods
	     GetAttrNames GetAttrs 
	     PrintMetaInfo
	     GetClassName
	     DebugContract
	     );

@EXPORT_OK = qw(scalar_attrs array_attrs hash_attrs methods old);
%EXPORT_TAGS = (DEFAULT  => \@EXPORT,
                EXTENDED => \@EXPORT_OK,
                ALL      => [@EXPORT, @EXPORT_OK]);

my %contract;
#########################################
# INTROSPECTING METHODS
#########################################
sub DebugContract
{
    my $name = shift;
    return $contract{$name};
}
#keys %{Class::Contract::GetContracts->{"node_record_type"}->{"attr"}}
#x map {Class::Contract::GetContracts->{"node_record_type"}->{"attr"}->{$_}->{"type"} }  keys %{Class::Contract::GetContracts->{"node_record_type"}->{"attr"}}

sub GetContracts
{
    return \%contract;
}
sub GetOwnContract
{
    my $self = shift;
    my $type = ref $self;
    return $contract{$type};
}
sub GetClassName
{
    my $self = shift;
    return ref $self;
}
sub GetAttrNames
{
    my $self = shift;
    return keys %{Class::Contract::GetOwnContract($self)->{attr}}
}
sub GetAttrs
{
    my $self = shift;
    return Class::Contract::GetOwnContract($self)->{attr}
}
sub GetMethodNames
{
    my $self = shift;
    return keys %{Class::Contract::GetOwnContract($self)->{method}}
}
sub GetMethods
{
    my $self = shift;
    return Class::Contract::GetOwnContract($self)->{method};
}
sub GetConstructors
{
    my $self = shift;
    return Class::Contract::GetOwnContract($self)->{ctor};
}
sub GetDestructors
{
    my $self = shift;
    return Class::Contract::GetOwnContract($self)->{dtor};
}
sub GetParents
{
    my $self = shift;
    return Class::Contract::GetOwnContract($self)->{parents};
}
sub GetParentNames
{
    my $self = shift;
    my $ret =Class::Contract::GetOwnContract($self)->{parents};
    if ($ret)
    {
	return @{$ret};
    }
    else
    {
	return ();
    }
}
sub IsAbstract
{
    my $self = shift;
    return Class::Contract::GetOwnContract($self)->{abstract};
}
######
#####
sub GetAttrMetaInfo
{
    my $self = shift;
    my $classname= GetClassName($self);
    return map {
	my $attrname = $_;
	my $type = $contract{$classname}{"attr"}{$attrname}->{type}; # HASHREF
	"$attrname->$type"; # return this !
    }
    keys %{
      Class::Contract::GetOwnContract($self)->{attr}
    }
}
sub GetMethodMetaInfo
{
    my $self = shift;
    return keys %{Class::Contract::GetOwnContract($self)->{method}}
}
sub PrintMetaInfo
{
    my $self = shift;    
    debugprint "################## METAINFO #################################################\n";
    debugprint "my \$Class= { name =>" . Class::Contract::GetClassName($self) . ",\n";
    debugprint "\tIsAbstract   =>" . Class::Contract::IsAbstract($self) . ",\n";
    debugprint "\tParents      =>\['" . join ("',\n'", Class::Contract::GetParentNames($self) ) . "'\],\n";

# now we will also get a
    debugprint "\tAttributes   =>\['" . join ("',\n'", Class::Contract::GetAttrMetaInfo($self) ) . "'\],\n";

    debugprint "\tMethods      =>\['" . join ("',\n'", Class::Contract::GetMethodMetaInfo($self) ) . "'\]\n";    
    debugprint "};\n";
    debugprint "############################################################################\n";
};

# NOT PUBLISHED
#5  'use_old'
#7  'clone'


my %data;        # the data of the object 
my %class_attr;
my $current;     # the parse tree, basically
my $msg_target;
my %no_opt;    # NOT IN PRODUCTION
# $Class::Contract::hook = \%data; # for testing GC # NOT IN PRODUCTION

my @class_dtors;
END { $_->()  foreach (@class_dtors) }

my ($carp, $croak) = (
  sub {
    my (@c) = caller(0);
    ($c[3] eq 'Class::Contract::__ANON__')
      ? print STDERR (@_, " at $c[1] line $c[2]\n") : &carp
  },
  sub {
    my (@c) = caller(0);
    ($c[3] eq 'Class::Contract::__ANON__')
      ? die(@_, " at $c[1] line $c[2]\n") : &croak 
  }
);

sub import {
  my $class = $_[0];
  my $caller = GetLocation();
  $contract{$caller}{use_old} = grep(/^old$/, @_) ? 1 : 0;
  push @_, @EXPORT;
  no strict 'refs';
  INIT {
#    *{$caller .'::croak'} = $croak  if defined *{$caller .'::croak'}{'CODE'};
#    *{$caller .'::carp'}  = $carp   if defined *{$caller .'::carp'}{'CODE'};
  }
  goto &Exporter::import;
}

sub unimport {
  my $class = shift;
  my $caller = GetLocation();
  $contract{$caller}{use_old} = 0  if grep /^old$/, @_; 
}

sub contract(&) {  $_[0]->();  _build_class(GetLocation()) }

=head1

check

=cut

sub check(\%;$) {
# NOT IN PRODUCTION...
  my $state = !$#_ ? 0 : $_[1] ? 1 : 0;
  defined $_
    or croak("Usage:\n\tcheck \%sentinel",
       ($#_ ? " => $state" : ""),
       " for ( \@classes );\n\n");

  my $forclass = $_;
  $_[0]->{$forclass} =
    bless { 'prev'     => $no_opt{$forclass},
      'forclass' => $forclass }, 'Class::Contract::FormerState';
  $no_opt{$forclass} = $state;
# ...NOT IN PRODUCTION
}

# NOT IN PRODUCTION...
sub Class::Contract::FormerState { # No function signature?
  $no_opt{$_[0]->{'forclass'}} = $_[0]->{'prev'}; #  my ($self) = @_;
}

sub no_opt { # my ($class) = @_;
  return   exists $no_opt{$_[0]}     ? $no_opt{$_[0]}
         : exists $no_opt{'__ALL__'} ? $no_opt{'__ALL__'}
         : 0;
}
# ...NOT IN PRODUCTION

my $location;
sub SetLocation
{
    $location = shift;
}
sub GetLocation
{
    if (defined($location))
    {
	return ($location,0);
    }
    else
    {	
	my ($ret ) =_location();
	return $ret;
    }
}

sub _location { # scalar context returns file and line of external code
                # array context returns package aka 'owner', file and line

  return ($location,0) if defined($location);

  my ($i, @c, $owner);
  while (@c = (caller($i++))[0..2]) {
    if ($c[0] !~ /^Class::Contract$/) {
      $owner = $c[0]  if !$owner;
      if ($c[1] !~ /^\(eval \d+\)$/) {
        return (wantarray ? $owner : (), join ' line ', @c[1,2]);
      }
    }
  }
}

# here are the different kinds of types that can be used
# and thier default types
my %def_type = ( 
  'attr'   => 'SCALAR',  # data attributes
  'method' => '',        # methods
  'ctor'   => '',        # constructors
  'dtor'   => '',        # destructors
  'clon'   => '',        # clone operators
);

# used for defining a member of a class 
sub _member {
  my (
      $kind, # what is the subclass of the member?
      $name, # name of the member
      $type  # type of the member
      ) = @_;
  my (
      $owner,    # Filename
      $location  # Line Number
      ) = _location;

  $name = ''  unless $name; # NAME OF THE MEMBER

  if (defined $contract{$owner      # the name of the class
			}{$kind     # the kind of member
			  }{$name   # name of the field
			    }) { # DUPLICATE CHECK
    croak "\u$kind ${owner}::$name redefined"  if $name;
    croak "Unnamed $kind redefined";
  }
  
  $contract{$owner}{$kind}{$name} = $current =
    bless {'name'     => $name,# NAME OF THE MEMBER
           'type'     => $type || $def_type{$kind},  # type of the NODE
           'gentype'  => $type || $def_type{$kind},  # NOT IN PRODUCTION
           'loc'      => $location,
           'shared'   => 0,
           'private'  => 0,
           'abstract' => 0,
           'pre'      => [], # NOT IN PRODUCTION
           'post'     => [], # NOT IN PRODUCTION
          }, "Class::Contract::$kind";

  # NOT IN PRODUCTION...
  $current->{'gentype'} = 'OBJECT'
    unless $current->{'gentype'} =~ /\A(SCALAR|ARRAY|HASH)\z/;
  # ...NOT IN PRODUCTION
  return $current;
}

sub attr($;$) { _member('attr'   => @_) }# name type
sub method($) { _member('method' => @_) }# name
sub ctor(;$)  { _member('ctor'   => @_) }# name
sub dtor()    { _member('dtor') }        #
sub clon()    { _member('clone') }       # 

# shortcuts for creating 
sub scalar_attrs(@) { map _member('attr', $_, 'SCALAR'), @_ }# create a list of  scalar attribute
sub array_attrs(@)  { map _member('attr', $_, 'ARRAY'),  @_ }# create a list of  array attribute
sub hash_attrs(@)   { map _member('attr', $_, 'HASH'),   @_ }# create a list of  hash attribute
sub methods(@)      { map _member('attr', $_),           @_ }# create a list of  empty methods

sub class(@)    { $_->{'shared'}   = 1  foreach(@_); @_ }    # create a class attribute
sub abstract(@) { $_->{'abstract'} = 1  foreach(@_); @_ }    # create an abstract method
sub private(@)  { $_->{'private'}  = 1  foreach(@_); @_ }    # create a private attribute

#
# here are the messages for the different types of checks
# 

my %def_msg = (                
  'pre'   => 'Pre-condition at %s failed',
  'post'  => 'Post-condition at %s failed',
  'invar' => 'Class invariant at %s failed',
  'impl'  => undef
);

# $current is the current member
sub _current {

  my (
      $field, # gets the current class
      $code   
      ) = @_;

  croak "Unattached $field"  unless defined $current;# is part of a class


  croak "Attribute cannot have implementation"
    if $current->isa('Class::Contract::attr') && $field eq 'impl';

  my $descriptor = bless {
    'code'  => $code,
    'opt'   => 0,    # NOT IN PRODUCTION
    'msg'   => $def_msg{$field},            # 
  }, 
  'Class::Contract::current';

  @{$descriptor}{qw(owner loc)} = _location;

  if ($field eq 'impl' && !( $current->isa('Class::Contract::ctor') 
                          || $current->isa('Class::Contract::dtor') 
                          || $current->isa('Class::Contract::clone') )) { 
    $current->{$field} = $descriptor
  } else {
    push @{$current->{$field}}, $descriptor
  }
  
  $msg_target = $descriptor;
}

sub failmsg {
  croak "Unattached failmsg"  unless $msg_target;
  $msg_target->{'msg'} = shift;
}

sub pre(&)  { _current('pre'  => @_) }
sub post(&) { _current('post' => @_) }
sub impl(&) { _current('impl' => @_) }

sub optional { # my (@descriptors) = @_;
  $_->{'opt'} = 1  foreach(@_); @_ # NOT IN PRODUCTION
}

sub invar(&) {
  my ($code) = @_;

  my $descriptor = {
    'code'  => $code,
    'opt'   => 0,    # NOT IN PRODUCTION
    'msg'   => $def_msg{'invar'},
  };
  @{$descriptor}{qw(owner loc)} = _location;

  push @{$contract{$descriptor->{'owner'}}{'invar'}}, $descriptor;
  $msg_target = $descriptor;
}


sub inherits(@)  {
  my ($owner) = _location;
  foreach (@_) {
    croak "Can't create circular reference in inheritence\n$_ is a(n) $owner" 
      if $_->isa($owner)
  }
  push @{$contract{$owner}{'parents'}}, @_;
}

sub _build_class($) {
  my ($class) = @_;
  my $spec = $contract{$class};

#  debugprint "Class::Contract We are building a class named :$class\n";
  _inheritance($class, $spec);
  _attributes($class, $spec);
  _methods($class, $spec);
  _constructors($class, $spec);
  _destructors($class, $spec);
  _clones($class, $spec);
  _GetData($class, $spec); # install a data getter!
  1;
}

localscope: {
  my @context;   # THE CONTEXT DATA
  # it is a #stack of references to the objects

  my %clear; # NOT IN PRODUCTION;
  sub _set_context  { 
      # PARAM1 SELF stored in __SELF__ and in $proto
      # 
    push @context, {'__SELF__' => shift};  # STORE SELF IN CONTEXT

    # NOT IN PRODUCTION...
    my $proto = $context[-1]{__SELF__};# GET THE TOP OF THE CONTEXT STACK and the self stored there
    my (
	$class    # THE CLASS is the ref of the object
	, 
	$obj      # The object that is a parameter
	) = ref($proto)    # $class is the type of object
	    ? (ref($proto), $proto)
		: ($proto, undef);


    return  if $class =~ /^Class::Contract::Old::_/; # is it a Old?  not needed
    if ($contract{$class}{'use_old'}) {
      my $class_old = "Class::Contract::Old::_$#context";
      _pkg_copy($class, $class_old);
      my $old = $class_old;
      if ($obj) {
        # Like generic_clone but into the cloned class
        my $old_key = \ my $undef;
        $old = bless \ $old_key, $class_old;
        $data{$$old} = _dcopy($data{$$obj})  if exists $data{$$obj};
      }
      $context[-1]{__OLD__} = $old;
    }
    # ...NOT IN PRODUCTION
  }
  sub _free_context {
    return pop @context # pops one off the context
  }
  sub old() {
    croak "No context. Can't call &old"  unless @context;# TOP OF THE STACK
    my $self = $context[-1]{__SELF__};
    my $class = ref($self) || $self;
    croak "Support for &old has been toggled off"
      unless ($contract{$class}{'use_old'});
    $context[-1]{__OLD__} # NOT IN PRODUCTION
  }

  my @value;
  sub _set_value  { push @value, \@_ }  # called to hold the return of a method
  sub _free_value { my $v = pop @value; wantarray ? @$v : $v->[0] }

#Note that within the pre- and post-conditions of an attribute, the
#special C<value> subroutine returns a reference to the attribute itself,
#so that conditions can check properties of the attribute they guard.

  sub value { 
    croak "Can't call &value "  unless @value;
    return $value[-1];
  }

  sub self() {
    if (@_) {
      # NOT IN PRODUCTION...
      croak "Usage:\tself(\$class_or_object)"
        unless defined *{join(ref($_[0])||$_[0], '::')};
      # ...NOT IN PRODUCTION
      $context[-1]{__SELF__} = shift;
    }
    croak "No context. Can't call &self"  unless @context;# TOP OF STACK
    $context[-1]{__SELF__}
  }

  sub callstate() {
    croak "No context. Can't call &callstate"  unless @context;# TOP OF STACK
    return $context[-1];
  }
}

sub _inheritance {                                  #  A  D  Invokation order
# Inheritence is left-most depth-first. Destructors #  /\ |   
# are called in reversed order as the constructors  # B C E    ctor: ABCDEF
# Diamond patterns in inheritence are 'handled' by  #  \//     dtor: FEDCBA
# looking for and skipping duplicate anonymous refs #   F

  my ($classname, $spec) = @_;
  my (%inherited_clause, %inherited_impl);
  foreach my $ancestor ( reverse @{$spec->{'parents'} || [] } ) {
    my $parent = $contract{$ancestor} || next;
    if ($parent->{'use_old'} and not $spec->{'use_old'}) {
      croak("Derived class $classname, has not toggled on support for ->old\n",
            "which is required by ancestor $ancestor. Did you forget to\n",
            "declare: use Contract 'old'; ?");
    }
    foreach my $clause ( qw( attr method ctor clone dtor ) ) {
      foreach my $name ( keys %{ $parent->{$clause} || {} } ) {
        # Inherit each clause from ancestor unless defined
        if (! defined $spec->{$clause}{$name}
            and not exists $inherited_clause{$name}) {
          $inherited_clause{$name}++;
          %{$spec->{$clause}{$name}} = (%{$parent->{$clause}{$name}});
          $spec->{$clause}{$name}{'pre'}  = []; # NOT IN PRODUCTION
          next;
        }

        # Inherit ctor/clone/dtor invokation from ancestors
        if ($clause =~ /^(ctor|clone|dtor)$/) {
          if (defined $parent->{$clause}{$name}{'impl'}
              and @{$parent->{$clause}{$name}{'impl'}}) {
            my (@impl, %seen) = (@{$spec->{$clause}{$name}{'impl'}});
            if (@impl) {
              $seen{$impl[$_]} = $_  foreach (0..$#impl);
              foreach my $item ( @{$parent->{$clause}{$name}{'impl'}} ) {
                splice(@{$spec->{$clause}{$name}{'impl'}}, $seen{$item}, 1)
                   if exists $seen{$item};
              }
            }
            $clause ne 'dtor'
            ? unshift(@{$spec->{$clause}{$name}{'impl'}},
                      @{$parent->{$clause}{$name}{'impl'}})
            : push(@{$spec->{$clause}{$name}{'impl'}},
                   @{$parent->{$clause}{$name}{'impl'}});
          }
        }

        # Get implementation from ancestor if derived but not redefined
        if ($clause eq 'method') {
          if (! defined $spec->{$clause}{$name}{'impl'}
              or $inherited_impl{$name}) {
            $inherited_impl{$name}++;
            $spec->{$clause}{$name}{'impl'}=$parent->{$clause}{$name}{'impl'};
          }
          croak("Forget 'private'? $classname inherits private $name from ",
                "$ancestor\n")
            if ($parent->{$clause}{$name}{'private'} 
                and not $spec->{$clause}{$name}{'private'})
        }
        # NOT IN PRODUCTION...
        # Inherit all post-conditions from ancestors
        if (@{$parent->{$clause}{$name}{'post'}||[]}) {
          my (@post, %seen) = (@{$spec->{$clause}{$name}{'post'}});
          if (@post) {
            $seen{$post[$_]} = $_  foreach (0..$#post);
            foreach my $item ( @{$parent->{$clause}{$name}{'post'}} ) {
              splice(@{$spec->{$clause}{$name}{'post'}}, $seen{$item}, 1)
                if exists $seen{$item};
            }
          }
          push(@{$spec->{$clause}{$name}{'post'}},
               @{$parent->{$clause}{$name}{'post'}});
        }
        # ...NOT IN PRODUCTION
      }
    }
    # NOT IN PRODUCTION...
    # Inherit all class invariants from ancestors
    if (defined $parent->{'invar'} and @{$parent->{'invar'}}) {
      defined $spec->{'invar'} or $spec->{'invar'} = [];
      my (@invar, %seen) = (@{$spec->{'invar'}});
      if (@invar) {
        $seen{$invar[$_]} = $_  foreach (0..$#invar);
        foreach (@{$parent->{'invar'}}) {
          splice(@{$spec->{'invar'}}, $seen{$_}, 1)  if exists $seen{$_}
        }
      }
      push @{$spec->{'invar'}}, @{$parent->{'invar'}};
    } 
    # ...NOT IN PRODUCTION
  }

  no strict 'refs';
  unshift @{"${classname}::ISA"}, @{ $spec->{'parents'} || [] };
}

sub _attributes {
  my ($classname, $spec) = @_;

  while ( my ($name, $attr) = each %{$spec->{'attr'}} ) {
    if ($attr->{'shared'}) {
      my $ref = $class_attr{$classname}{$name} = 
        $attr->{'type'} eq 'ARRAY'  ? []
      : $attr->{'type'} eq 'HASH'   ? {}
      : $attr->{'type'} eq 'SCALAR' ? do { \ my $scalar }
      : eval { $attr->{'type'}->new }
        || croak "Unable to create $attr->{'type'} object ",
                 "for class attribute $name";
    }

    localscope: {
      no strict 'refs';
      local $^W;
      *{"${classname}::$name"} = sub { # the GET/SET Routine
        croak(qq|Can\'t access object attr w/ class reference |,$attr->{'loc'})
          unless ($attr->{'shared'} or ref($_[0]));

        my $caller = caller;
#        croak "attribute ${classname}::$name inaccessible from package $caller"
#          unless 
#	      $caller->isa($classname) 
#		  or 
#	      $classname->isa($caller); # it seems that derived attributes are put in the leaf classes

        my $self = shift;
        _set_context(($attr->{'shared'} ? ref($self)||$self : $self),
                     join ' line ', [caller]->[1,2]);

	# 
        my $attr_ref = ($attr->{'shared'})# is it shared
          ? $class_attr{$classname}{$name}# class attribute
          : $data{$$self}{$name}; # the data of this objects self is stored in an hash!!!!! WOW!

        _set_value $attr_ref;  # 

	# $name has the name of the attr
	# $$self is the pointer to the object
	# $data{$$self} is the hash of objects
        # NOT IN PRODUCTION...
        my @fail = generic_check('pre', 'attr' => $name, $spec);
        croak @fail  if @fail;
        # ...NOT IN PRODUCTION
        
        _free_context;
        
        # NOT IN PRODUCTION...
        return "Class::Contract::Post$attr->{'gentype'}"->new(
          $attr->{'post'}, $attr_ref, $name,
        )  if @{$attr->{'post'}};
        # ...NOT IN PRODUCTION

        scalar _free_value;
        return $attr_ref;
      };
    }
  }
}

# spec -> method
# method
#    impl
#    abstract

sub _methods {
  my ($classname, $spec) = @_;

  while ( 
	  my (
	      $name,    # 
	      $method   #
	      ) 
	  = 
	  each 
	  %{
	      $spec->{'method'}
	  } 
	  ) 
  { # FOR EACH METHOD TO INSTALL!
      $spec->{'abstract'} ||= $method->{'abstract'};
      unless ($method->{'impl'}) {
	  if ($method->{'abstract'}) {
	      $method->{'impl'} = {
		  'code' => sub {
		      croak "Can't call abstract method ${classname}::$name"
		      } 
	      }
	  } 
	  else 
	  {
	      croak 
		  qq{No implementation for method $name at $method->{'loc'}.\n}
	      ,
	      qq{(Did you forget to declare it 'abstract'?)\n}
	  }
      }


   # METHOD CALL
   # $name      is the name of the method
   # $classname is the information
   # $caller    of the function
   # $method    - the method object
   #             $method->{'private'}      # is a private method?
   #             $method->{'shared'}       # is a shared method?
   #             $method->{'impl'}{'code'} # the code-
    # CONTEXT?
    # VALUE  ?
    local_scope: { 
      local $^W;
      no strict 'refs';

      # here the method is installed
      # $classname    name of the class
      # $name         name of the method
      
      *{"${classname}::$name"} = sub {
        my $caller = caller;
        croak("private method ${classname}::$name inaccessible from ",
              scalar caller)
          if ($method->{'private'}
              and not ($classname->isa($caller))); # or $caller->isa($classname)));

        my $self = shift;
        _set_context(($method->{'shared'} ? ref($self)||$self : $self),
                     join ' line ', [caller]->[1,2]);
  
        # NOT IN PRODUCTION...
        croak(qq|Can\'t invoke object method w/ class name |, $method->{'loc'})
          unless ($method->{'shared'} or ref($self));

        my $no_opt = no_opt($classname);
        my @fail = generic_check('pre', 'method' => $name, $spec, @_);
        croak @fail  if @fail;
        # ...NOT IN PRODUCTION
	
	# here the implementaion is checked and called
        _set_value wantarray # IF THE SUB WANTS AN ARRAY
          ? $method->{'impl'}{'code'}->(@_)        #  CALL THE CODE as Array
          : scalar $method->{'impl'}{'code'}->(@_);#  CALL THE CODE as Scalar
        
	# CHECK THE POST INVARIANT
        # NOT IN PRODUCTION...
        generic_check('post',  'method' => $name, $spec, @_);

        # THE CLASS INVARIANT
        generic_check('invar', 'method' => $name, $spec, @_) 
          if (caller ne $classname); # if not called from outside
        # ...NOT IN PRODUCTION

        _free_context;    
        _free_value;      # RETURN THE VALUE
      }; # END OF AUTOGENERATED METHOD
  } # END OF LOCAL SCOPE

  } # NEXT METHOD
}

# NOT IN PRODUCTION...
sub generic_check {
  return  if (ref(self)||self) =~ /^Class::Contract::Old::_/;
    
  my (
      $type,        # what is the type of the member
      $kind,        # what kind of member is it?
      $name,        # what is the name of the member
      $class_spec,  # what is the class in question
      @args         # what are the args to the check?
      ) = @_;
  my @specs = @{
      $class_spec->{$kind}{$name}{$type}
      ||
	  []
      };

  my @errors;

  foreach my $spec ( @specs ) {
    next  if $spec->{'opt'} && no_opt($spec->{'owner'})
      || $spec->{'code'}->(@args);
    push @errors, 
    sprintf(
	    $spec->{'msg'},
	    $spec->{'loc'}
	    )."\n";
  }
  
  @errors ? croak @errors : return   unless $type eq 'pre';
  return  if @specs && !@errors;

  # OTHERWISE SATISFY AT LEAST ONE PARENT?
  foreach my $ancestor ( @{$class_spec->{'parents'}||[]} ) {
    my $parent = $contract{$ancestor};
    next  unless exists $parent->{$kind}{$name};

    # modified by mdupont
    my $has_pre_raw= $parent->{$kind}{$name}{'pre'};
    my $has_pre=0;
    if (!$has_pre_raw)# check the parents
    {
	$has_pre = scalar @{$has_pre_raw};
	foreach my $p (@{$parent->{'parents'}||[]}) {
	    $has_pre++ and last  if _hasa($p, $kind, $name, 'pre');
	}
    }

    {
	if ($has_pre) {
	    my @par_err = generic_check($type, $kind, $name, $parent, @args);
	    return  unless @par_err;
	    push @errors, @par_err;
	}
    }
    }
  return @errors;
}

sub _hasa {
  my ($class, $kind, $name, $type) = (@_);
  return 0  unless defined $contract{$class}{$kind}{$name};

  my $has = @{$contract{$class}{$kind}{$name}{$type} || []} ? 1 : 0;
  unless ($has) {
    foreach my $ancestor (@{$contract{$class}{'parents'} || []}) {
      $has++ and last  if _hasa($ancestor, $kind, $name, $type);
    }
  }
  return $has;
}
# ...NOT IN PRODUCTION


# this is a generic constructor
sub generic_ctor {
  my ($class) = @_;

  croak "Class $class has abstract methods. Can't create $class object"
    if $contract{$class}{'abstract'};

  my $key = \ my $undef;
  my $obj = \ $key;
  bless $obj, $class;

  my $attr = $contract{$class}{'attr'};# THE ATTRIBUTE HASHREF$
        
  foreach my $attrname ( keys %$attr ) {# FOR EACH ATTRIBUTE
    unless ($attr->{$attrname} && $attr->{$attrname}{'shared'}) {
      my $ref = $data{$key}{$attrname}
      = $attr->{$attrname}{'type'} eq 'ARRAY'  ? []
      : $attr->{$attrname}{'type'} eq 'HASH'   ? {}
      : $attr->{$attrname}{'type'} eq 'SCALAR' ? do { \my $scalar }
      : eval {  # OBJECT CONSTRUCTOR
	  $attr->{$attrname}{type}->new 
	  }
      || croak "Unable to create $attr->{$attrname}{'type'} ",
               "object for attribute $attrname";
    }
  }

  return $obj;
}

sub generic_clone ($) {
  my $self = shift;
  my $ref = ref($self);
  croak "usage: \$object->clone -Invalid arg $self"
    unless ($ref and 
            $ref !~ /^(HASH|ARRAY|SCALAR|GLOB|FORMAT|CODE|Regexp|REF)$/);
  my $key  = \ my $undef;
  my $obj  = bless \$key, $ref;
  $data{$key} = _dcopy($data{$$self})  if exists $data{$$self};
  return $obj;
}


sub _constructors {
  my ($classname, $spec) = @_;
  my $noctor = 1;

  while ( my ($name, $ctor) = each %{$spec->{'ctor'}} ) {
    $noctor &&= $ctor->{'shared'}
  }

  $spec->{'ctor'}{'new'} = bless {
    'name'     => 'new',
    'shared'   => 0,
    'abstract' => 0,
    'loc'      => '<implicit>'
  }, 'Class::Contract::ctor'
    if $noctor;

  while ( my ($name, $ctor) = each %{$spec->{'ctor'}} ) {
    $spec->{'abstract'} ||= $ctor->{'abstract'};

    if ($ctor->{'shared'}) {
      localscope: {
        local $^W;
        no strict 'refs';
        my $classctor = sub {
          my $self = shift;
          _set_context ref($self)||$self; 
                                
          # NOT IN PRODUCTION...
          my @fail = generic_check('pre', 'ctor' => $name, $spec, @_);
          croak @fail  if @fail;
          # ...NOT IN PRODUCTION

          $_->{'code'}->(@_)  foreach ( @{$ctor->{'impl'}} );      

          # NOT IN PRODUCTION...
          generic_check('post', 'ctor' => $name, $spec, @_);
          generic_check('invar','ctor' => $name, $spec, @_)
            if (caller ne $classname);
          # ...NOT IN PRODUCTION

          _free_context;
        };
        $classname->$classctor();
#        *{"${classname}::$name"} = $classctor  if $name;
      }
    } else {
      localscope:{
        local $^W;
        no strict 'refs';
        *{"${classname}::$name"} = sub { # here the package function is created
          my $proto = shift;
          my $class = ref($proto)||$proto;

          my $self = Class::Contract::generic_ctor($class); # GENERIC CONSTRUCTORs

          _set_context $self;
      
          # NOT IN PRODUCTION...
          my @fail = generic_check('pre', 'ctor' => $name, $spec, @_); # CHECKS
          croak @fail  if @fail;
          # ...NOT IN PRODUCTION

	  # here the implementations of all the implementations of the constructors are called
          $_->{'code'}->(@_)  foreach ( @{$ctor->{'impl'}} );
      
          # NOT IN PRODUCTION...
          generic_check('post', 'ctor' => $name, $spec, @_);
          generic_check('invar','ctor' => $name, $spec, @_)
            if (caller ne $classname);
          # ...NOT IN PRODUCTION
      
          _free_context;
          return $self;
        }
      }
    }
  }
}

# mdupont
sub _GetData
{
    my ($classname, $spec) = @_;    

  localscope:{
      local $^W;
      no strict 'refs';
      *{"${classname}::GetData"} = sub { # the GET/SET Routine
	  my $caller = caller;
	  my $self = shift;
	  _set_context($self);       
	  my $objectdata = $data{$$self};	
	  return $objectdata;
	  _free_context;	
      }    
  }
}

sub _destructors {

  my ($classname, $spec) = @_;
  my $dtorcount = 0;

  while ( my ($name, $dtor) = each %{$spec->{'dtor'}} ) {
    $spec->{'abstract'} ||= $dtor->{'abstract'};
    
    if ($dtor->{'shared'}) {
      localscope: {
        local $^W;
        no strict 'refs';
        my $classdtor = sub {
          croak "Illegal explicit invokation of class dtor", $dtor->{'loc'}
            if caller() ne 'Contract';
          my $self = shift;
          $self = ref $self  if ref $self;
          
          _set_context $self;
          
          # NOT IN PRODUCTION...
          my @fail = generic_check('pre', 'dtor' => $name, $spec, @_);
          croak @fail  if @fail;
          # ...NOT IN PRODUCTION
          
          $_->{'code'}->(@_)  foreach ( @{$dtor->{'impl'}} );
          
          generic_check('post', 'dtor' => $name, $spec, @_);# NOT IN PRODUCTION
          _free_context;
        };
        
        push @class_dtors, sub { $classname->$classdtor() };
      }
    } else {
      croak "Class $classname has too many destructors"  if $dtorcount++;
      
      localscope: {
        local $^W;
        no strict 'refs';
        my $objdtor = sub {
          croak "Illegal explicit invokation of object dtor", $dtor->{'loc'}
            if caller() ne 'Contract';
          
          my $self = shift;
          _set_context $self;
          
          # NOT IN PRODUCTION...
          my @fail = generic_check('pre', 'dtor' => $name, $spec, @_);
          croak @fail  if @fail;
          # ...NOT IN PRODUCTION
          
          $_->{'code'}->(@_)  foreach ( @{$dtor->{'impl'}||[]} );
          
          # NOT IN PRODUCTION...
          generic_check('post',  'dtor' => $name, $spec, @{[@_]});
          generic_check('invar', 'dtor' => $name, $spec, @{[@_]})
            if (caller ne $classname);
          # ...NOT IN PRODUCTION
          
          _free_context;
          return;
        };
        
        *{"${classname}::DESTROY"} = sub {
          $_[0]->$objdtor();
          delete $data{${$_[0]}}  if exists $data{${$_[0]}};
        };
      }
    }
  }
  unless (defined &{"${classname}::DESTROY"}) {
    local $^W;
    no strict 'refs';
    *{"${classname}::DESTROY"} = sub {
      delete $data{${$_[0]}}  if exists $data{${$_[0]}};
    };
  }
}

sub _clones {
  my ($classname, $spec) = @_;
  my $clone_count = 0;
  
  $spec->{'clone'}{''} = bless {
    'name'     => '',
    'shared'   => 0,
    'abstract' => 0,
    'loc'      => '<implicit>'
  }, 'Class::Contract::clone'
    unless $spec->{'clone'};

  while ( my ($name, $clause) = each %{$spec->{'clone'}} ) {
    
    $spec->{'abstract'} ||= $clause->{'abstract'};
    croak "'class' clause can not be used to qualify 'clon'"
      if $clause->{'shared'};
    croak "too many clon clauses"  if $clone_count++;
  
    localscope: {
      local $^W;
      no strict 'refs';
      *{"${classname}::clone"} = sub {
        my $self = shift;
        $self = generic_clone($self);
        _set_context $self;
          
        # NOT IN PRODUCTION...
        my @fail = generic_check('pre', 'dtor' => $name, $spec, @_);
        croak @fail  if @fail;
        # ...NOT IN PRODUCTION
        
        $_->{'code'}->(@_)  foreach ( @{$clause->{'impl'}||[]} );
          
        # NOT IN PRODUCTION...
        generic_check('post',  $clause => $name, $spec, @{[@_]});
        generic_check('invar', $clause => $name, $spec, @{[@_]})
          if (caller ne $classname);
        # ...NOT IN PRODUCTION
          
        _free_context;
        return $self;
      };
    }
  }
}

localscope: {
  my ($a,$z) = (qr/(^|^.*?=)/, qr/\(.*?\)$/);
  my %seen = ();
  my $depth = 0;
  sub _dcopy { # Dereference and return a deep copy of whatever's passed
    my ($r, $ref, $rval);
    $ref = ref($_[0])   or return $_[0];
    exists $seen{$_[0]} and return $seen{$_[0]};
    $depth++;

    $r =
      ($_[0] =~ /${a}HASH$z/)   ? {map _dcopy($_), (%{$_[0]})}
    : ($_[0] =~ /${a}ARRAY$z/)  ? [map _dcopy($_), @{$_[0]} ]
    : ($_[0] =~ /${a}SCALAR$z/) ? do { my $v = _dcopy(${$_[0]}); \$v }
    : ($_[0] =~ /${a}FORMAT$z/) ? $_[0]
    : ($_[0] =~ /${a}CODE$z/)   ? $_[0]
    : ($_[0] =~ /${a}Regexp$z/) ? $_[0]
    : ($_[0] =~ /${a}REF$z/)    ? $_[0]
    : ($_[0] =~ /${a}GLOB$z/)   ? $_[0]
    : $_[0]->can('clone') ? $_[0]->clone : $_[0];

    $rval = $ref =~ /^(HASH|ARRAY|SCALAR|GLOB|FORMAT|CODE|Regexp|REF)$/ 
             ? $r
             : bless $r, $ref;

    --$depth 
      and $seen{$_[0]} = $rval
      or  %seen = (); 

    return $rval;
  }
}

# NOT IN PRODUCTION...
sub _pkg_copy ($$) { # $from_package, $to_package
  no strict 'refs';
  defined *{$_[0] . '::'}
    or croak "_pkg_copy() Can't clone from non-existant package $_[0]";
  defined *{$_[1] . '::'} and *{$_[1] . '::'} = {};
  
  foreach my $glob (values %{*{$_[0] . '::'}}) {
    my ($varname) = $glob =~ /^\*$_[0]::(.*)/ or next;
    foreach my $slot (qw(SCALAR ARRAY HASH CODE FORMAT)) {
      my $ref = _dcopy(*{"$_[0]::$varname"}{$slot});
      *{"$_[1]::$varname"} = $ref  if defined $ref;
    }
  }
}

sub _pkg_clear ($) {
  no strict 'refs';
  my ($package) = shift;
  my $stash = *{$package . '::'}{HASH};
  foreach my $name (keys %$stash) {
    $name = join('::', $package, $name);
#    print "undef $name\n";
    undef $$name;
    undef @$name;
    undef %$name;
    
    undef &$name;
    undef *$name;
  }
  undef %{$package . '::'};
}

sub Class::Contract::PostOBJECT::new {
  my ($class, $posts, $original, $name) = @_;
  my $objclass = ref $original;
  carp("Warning: cannot check post-condition",
       (@$posts==1?"":'s'),
       " on $objclass attribute '$name'")
    if $^W;
  _free_value;
  return $original;
}

package Class::Contract::PostSCALAR;

sub new {
  my $proxy;
  tie $proxy, 'Class::Contract::PostSCALAR', @_;
  return \$proxy;
}

sub TIESCALAR {
  my ($class, $self, $postsubs, $original) = @_;
  return bless {
    'orig' => $original,
    'post' => $postsubs,
  }, $class;
}

sub FETCH { return ${$_[0]->{'orig'}} }
sub STORE { ${$_[0]->{'orig'}} = $_[1] }

sub DESTROY {
  Class::Contract::generic_check('post', 'attr', @{self()}{qw(orig spec)}, @_);
  Class::Contract::_free_value();
}

package Class::Contract::PostARRAY;

sub new {
  my @proxy;
  tie @proxy, 'Class::Contract::PostARRAY', @_;
  if ($_[3]) { bless \@proxy, ref $_[2] }
  return \@proxy;
}

sub TIEARRAY {
  my ($class, $self, $postsubs, $original) = @_;
  return bless { 'orig' => $original,
     'post' => $postsubs,
         }, $class;
}

sub FETCH       { $_[0]->{'orig'}->[$_[1]] }
sub FETCHSIZE   { scalar @{$_[0]->{'orig'}} }
sub STORE       { $_[0]->{'orig'}->[$_[1]] = $_[2] }
sub STORESIZE   { $#{$_[0]->{'orig'}} = $_[1]-1 }
sub EXTEND      { $#{$_[0]->{'orig'}} = $_[1]-1 }
sub CLEAR       { @{$_[0]->{'orig'}} = () }
sub PUSH        { push @{$_[0]->{'orig'}}, @_[1..$#_] }
sub POP         { pop @{$_[0]->{'orig'}} }
sub UNSHIFT     { unshift @{$_[0]->{'orig'}}, @_[1..$#_] }
sub SHIFT       { shift @{$_[0]->{'orig'}} }

sub DESTROY {
  Class::Contract::generic_check('post', 'attr', @{self()}{qw(orig spec)}, @_);
  Class::Contract::_free_value();
}


package Class::Contract::PostHASH;

sub new {
  my %proxy;
  tie %proxy, 'Class::Contract::PostHASH', @_;
  if ($_[3]) { bless \%proxy, ref $_[2] }
  return \%proxy;
}

sub TIEHASH {
  my ($class, $self, $postsubs, $original) = @_;
  return bless { 'orig' => $original,
     'post' => $postsubs,
         }, $class;
}

sub FETCH       { $_[0]->{'orig'}->{$_[1]} }
sub STORE       { $_[0]->{'orig'}->{$_[1]} = $_[2] }
sub EXISTS      { exists $_[0]->{'orig'}->{$_[1]} }
sub DELETE      { delete $_[0]->{'orig'}->{$_[1]} }
sub CLEAR       { %{$_[0]->{'orig'}} = () }
sub FIRSTKEY    { keys %{$_[0]->{'orig'}}; each %{$_[0]->{'orig'}} }
sub NEXTKEY     { each %{$_[0]->{'orig'}} }

sub DESTROY {
  Class::Contract::generic_check('post', 'attr', @{self()}{qw(orig spec)}, @_);
  Class::Contract::_free_value();
}
# ...NOT IN PRODUCTION

1;

__END__

=head1 NAME

Contract - Design-by-Contract OO in Perl.

=head1 VERSION

This document describes version 1.10 of Contract,
released February  9, 2001.

=head1 SYNOPSIS

    package ClassName
    use Contract;

    contract {
      inherits 'BaseClass';

      invar { ... };

      attr 'data1';
      attr 'data2' => HASH;

      class attr 'shared' => SCALAR;

      ctor 'new';

      method 'methodname';
        pre  { ... };
          failmsg 'Error message';

        post  { ... };
          failmsg 'Error message';

        impl { ... };

      method 'nextmethod';
        impl { ... };

      class method 'sharedmeth';
        impl { ... };

      # etc.
    };


=head1 DESCRIPTION

=head2 Background

Design-by-contract is a software engineering technique in which each
module of a software system specifies explicitly what input (or data or
arguments) it requires, and what output (or information or results) it
guarantees to produce in response.

These specifications form the "clauses" of a contract between a
module and the client software that uses it. If the client software
abides by the input requirements, the module guarantees to produce
the correct output. Hence by verifying these clauses at each
interaction with a module, the overall behaviour of the system can
be confidently predicted.

Design-by-contract reinforces the benefits of modular design techniques
by inserting explicit compile-time or run-time checks on a contract.
These checks are most often found in object-oriented languages
and are typically implemented as pre-conditions and post-conditions
on methods, and invariants on classes.

Note that these features differ from simple verification statements
such as the C C<assert> statement. Conditions and invariants are
properties of a class, and are inherited by derived classes.

An additional capacity that is often provided in design-by-contract
systems is the ability to selectively disable checking in production
code. This allows the contractual testing to be carried out
during implementation, without impinging on the performance of
the final system.

=head2 Adding design-by-contract to Perl

The Contract module provides a framework for specifying
methods and attributes for a class (much like the existing class
definition modules Class::Struct, Class::MethodMaker, and 
Class::Generate). Contract allows both per-object and per-class
methods and attributes to be defined. Attributes may be scalar-, array-,
hash-, or object-based.

Contract differs from other class-specification modules (except
Class::Generate) in that it also provides the ability to specify
invariant conditions on classes, and pre- and post-conditions on methods
and attributes. All of these clauses are fully inheritable, and may be
selectively disabled. It differs from all other modules in that it has a
cleaner, simpler specification syntax, and -- more importantly -- it
enforces encapsulation of object attributes, thereby ensuring that the
class contract cannot be subverted.


=head2 Defining classes

Contract provides an explicit syntax for defining the attributes,
methods, and constructors of a class. The class itself is defined using
the C<contract> subroutine. C<contract> takes a single argument -- a
subroutine reference or a block. That block is executed once and the
results used to construct and install the various components of the
class in the current package:

        package Queue;
        contract {
          # specification of class Queue attributes and methods here
        };

=head2 Defining attributes

Attributes are defined within the C<contract> block via the C<attr> subroutine.
Attributes must be given a name, and may also be given a type: C<SCALAR>,
C<ARRAY>, C<HASH>, or a class name:

        contract {
                attr 'last';                   # Scalar attribute (by default)
                attr 'lest' => SCALAR;         # Scalar attribute
                attr 'list' => ARRAY;          # Array attribute
                attr 'lost' => HASH;           # Hash attribute
                attr 'lust' => MyClass;        # Object attribute
        };

For each attribute so declared, Contract creates an I<accessor> -- a
method that returns a reference to the attribute in question. Code using these
accessors might look like this:

        ${$obj->last}++;
        push @{$obj->list}, $newitem;
        print $obj->lost->{'marbles'};
        $obj->lust->after('technology stocks');

Attributes are normally object-specific, but it is also possible to define
attributes that are shared by all objects of a class. Class objects are
specified by prefixing the call to C<attr> with a call to the C<class>
subroutine:

        class Queue;
        contract {
                class attr 'obj_count';
        };

The accessor for this shared attribute can now be called either as an
object method:

        print ${$obj->obj_count};

or as a class method:

        print ${Queue->obj_count};

In order to ensure that the clauses of a class' contract (see below)
are honoured, both class and object attributes are only accessible via
their accessors, and those accessors may only be called within methods
belonging to the same class hierarchy. Objects are implemented as
"flyweight scalars" in order to ensure this strict encapsulation is
preserved.

=head2 Defining methods

Methods are defined in much the same way as attributes. The C<method>
subroutine is used to specify the name of a method, then the C<impl>
subroutine is used to provide an implementation for it:

        contract {
                attr list => ARRAY;

                method 'next';
                    impl { shift @{self->list} };

                method 'enqueue';
                    impl { push @{self->list}, $_[1] };
        };

C<impl> takes a block (or a reference to a subroutine), which is used as
the implementation of the method named by the preceding C<method> call.
Within that block, the subroutine C<self> may be used to return a
reference to the object on which the method was called. Unlike, regular
OO Perl, the object reference is not passed as the method's first argument.
(Note: this change occurred in version 1.10)

Like attributes, methods normally belong to -- and are accessed via -- a
specific object. To define methods that belong to the entire class, the
C<class> qualifier is once again used:

        contract {
                class attr 'obj_count';

                class method 'inc_count';
                        impl { ${self->obj_count}++ };
        };

Note that the C<self> subroutine can still be used -- within a class
method it returns the appropriate class name, rather than an object
reference.

=head2 Defining constructors

Contract requires constructors to be explicitly defined using
the C<ctor> subroutine:

        contract {
                ctor 'new';
                    impl { @{self->list} = ( $_[0] ) }
        };

Note that the implementation section of a constructor I<doesn't> specify
code to build or bless the new object. That is taken care of
automatically (in order to ensure the correct "flyweight"
implementation of the object).

Instead, the constructor implementation is invoked I<after> the object
has been created and blessed into the class. Hence the implementation
only needs to initialize the various attributes of the C<self> object.
In addition, the return value of the implementation is ignored:
constructor calls always return a reference to the newly created object.

Any attribute that is not initialized by a constructor is
automatically "default initialized". By default, scalar attributes
remain C<undef>, array and hash attributes are initialized to an empty
array or hash, and object attributes are initialized by having their
C<new> constructor called (with no arguments). This is the only
reasonable default for object attributes, but it is usually advisable to
initialize them explicitly in the constructor.

It is also possible to define a "class constructor", which may be used
to initialize class attributes:

        contract {
                class attr 'obj_count';

                class ctor;
                        impl { ${self->obj_count} = 0 };
        };

The class constructor is invoked at the very end of the call to
C<contract> in which the class is defined.

Note too that the class constructor does not require a name. It may,
however, be given one, so that it can be explicitly called again (as a
class method) later in the program:

        class MyClass;
        contract {
                class attr 'obj_count';

                class ctor 'reset';
                        impl { ${self->obj_count} = 0 };
        };

        # and later...

        MyClass->reset;


=head2 Defining destructors

Destructors are also explicitly defined under Contract,
using the C<dtor> subroutine:

        contract {
                dtor;
                    impl { print STDLOG "Another object died\n" }
        };

As with the constructor, the implementation section of a destructor
doesn't specify code to clean up the "flyweight" implementation of
the object. Contract takes care of that automatically.

Instead, the implementation is invoked I<before> the object is
deallocated, and may be used to clean up any of the internal structure
of the object (for example to break reference cycles).

It is also possible to define a "class destructor", which may be used
to clean up class attributes:

        contract {
                class attr 'obj_count';

                class dtor;
                    impl { print STDLOG "Total was ${self->obj_count}\n" };
        };

The class destructor is invoked from an C<END> block within Contract
(although the implementation itself is a closure, so it executes in the
namespace of the original class).


=head2 Constraining class elements

As described so far, Contract doesn't provide any features that
differ greatly from those of any other class definition module. But
Contract does have one significant difference: it allows the
class designer to specify "clauses" that implement and enforce a
contract on the class's interface.

Contract clauses are specified as labelled blocks of code, associated
with a particular class, method, or attribute definition. 

=head2 Class invariants

Classes may be given I<invariants>: clauses than must be satisfied at
the end of any method call that is invoked from outside the class
itself. For example, to specify that a class's object count attribute
must never fall below zero:

        contract {
                invar { ${self->obj_count} >= 0 };
        };

The block following C<invar> is treated as if it were a class method
that is automatically invoked after every other method invocation. If the
method returns false, C<croak> is invoked with the error message:
C<'Class invariant at %s failed'> (where the C<'%s'> is replaced by the file
and line number at which the invariant was defined).

This error message can be customized, using the C<failmsg> subroutine:

        contract {
                invar { ${self->obj_count} >= 0 };
                    failmsg 'Anti-objects detected by invariant at %s';
        };

Once again, the C<'%s'> is replaced by the appropriate file name and
line number. A C<failmsg> can be specified after other types of clause
too (see below).

A class may have as many invariants as it requires, and
they may be specified anywhere throughout the the body of the C<contract>.

=head2 Attribute and method pre- and post-conditions

Pre- and post-conditions on methods and attributes are specified
using the C<pre> and C<post> subroutines respectively.

For attributes, pre-conditions are called before the attribute's
accessor is invoked, and post-conditions are called after the reference
returned by the accessor is no longer accessible. This is
achieved by having the accessor return a tied scalar whose C<DESTROY>
method invokes the post-condition.

Method pre-conditions are tested before their method's implementation is
invoked; post-conditions are tested after the implementation finishes
(but before the method's result is returned). Constructors are (by
definition) class methods and may have pre- and post-conditions, just
like any other method.

Both types of condition clause receive the same argument list as the
accessor or method implementation that they constrain. Both are expected
to return a false value if they fail:

        contract {
                class attr 'obj_count';
                    post { ${&value} > 0 };
                      failmsg 'Anti-objects detected by %s';

                method 'inc_count';
                    post { ${self->obj_count} < 1000000 };
                      failmsg 'Too many objects!';
                    impl { ${self->obj_count}++ };
        };

Note that within the pre- and post-conditions of an attribute, the
special C<value> subroutine returns a reference to the attribute itself,
so that conditions can check properties of the attribute they guard.

Methods and attributes may have as many distinct pre- and
post-conditions as they require, specified in any convenient order.


=head2 Checking state changes.

Post-conditions and invariants can access the previous state of an object or
the class, via the C<old> subroutine. Within any post-condition or invariant,
this subroutine returns a reference to a copy of the object or class
state, as it was just before the current method or accessor was called.

For example, an C<append> method might use C<old> to verify the appropriate
change in size of an object:

        contract {
            method 'append';
                post { @{self->queue} == @{old->queue} + @_ }
                impl { push @{self->queue}, @_ };
        };

Note that the implementation's return value is also available in the
method's post-condition(s) and the class's invariants, through the
subroutine C<value>. In the above example, the implementation of C<append>
returns the new size of the queue (i.e. what C<push> returns), so the
post-condition could also be written:

        contract {
            method 'append';
                post { ${&value} == @{old->queue} + @_ }
                impl { push @{self->queue}, @_ };
        };

Note that C<value> will return a reference to a scalar or to
an array, depending on the context in which the method was originally
called.


=head2 Clause control

Any type of clause may be declared optional:

        contract {
                optional invar { @{self->list} > 0 };
                failmsg 'Empty queue detected at %s after call';
        };

By default, optional clauses are still checked every time a method or
accessor is invoked, but they may also be switched off (and back on) at
run-time, using the C<check> method:

        local $_ = 'Queue';         # Specify in $_ which class to disable
        check my %contract => 0;    # Disable optional checks for class Queue

This (de)activation is restricted to the scope of the hash that is passed as
the first argument to C<check>. In addition, the change only affects the
class whose name is held in the variable $_ at the time C<check> is called.
This makes it easy to (de)activate checks for a series of classes:

        check %contract => 0 for qw(Queue PriorityQueue DEQueue);  # Turn off
        check %contract => 1 for qw(Stack PriorityStack Heap);     # Turn on


The special value C<'__ALL__'> may also be used as a (pseudo-)class name:

        check %contract => 0 for __ALL__;

This enables or disables checking on every class defined using
Contract. But note that only clauses that were originally
declared C<optional> are affected by calls to C<check>. Non-optional
clauses are I<always> checked.

Optional clauses are typically universally disabled in production code,
so Contract provides a short-cut for this. If the module is 
imported with the single argument C<'production'>, optional clauses
are universally and irrevocably deactivated. In fact, the C<optional>
subroutine is replaced by:

        sub Class::Contract::optional {}

so that optional clauses impose no run-time overhead at all.

In production code, contract checking ought to be disabled completely,
and the requisite code optimized away.  To do that, simply change:

  use Contract;

to

  use Class::Contract::Production;


=head2 Inheritance

The semantics of class inheritance for Contract classes
differ in several respects from those of normal object-oriented Perl.

To begin with, classes defined using Contract have a I<static
inheritance hierarchy>. The inheritance relationships of contracted classes
are defined using the C<inherits> subroutine within the class's C<contract>
block:

        package PriorityQueue;
        contract {
                inherits qw( Queue OrderedContainer );
        };


That means that ancestor classes are fixed at compile-time
(rather than being determined at run-time by the @ISA array). Note
that multiple inheritance is supported.

Method implementations are only inherited if they are not explicitly
provided. As with normal OO Perl, a method's implementation is inherited
from the left-most ancestral class that provides a method of the same name
(though with Contract, this is determined at compile-time).

Constructors are a special case, however. Their "constructive"
behaviour is always specific to the current class, and hence involves
no inheritance under any circumstances. However, the "initialising"
behaviour specified by a constructor's C<impl> block I<is> inherited. In
fact, the implementations of I<all> base class constructors are
called automatically by the derived class constructor (in left-most,
depth-first order), and passed the same argument list as the invoked
constructor. This behaviour is much more like that of other OO
programming languages (for example, Eiffel or C++).

Methods in a base class can also be declared as being I<abstract>:

        contract {
            abstract method 'remove';
                post { ${self->count} == ${old->count}-1 };
        };

Abstract methods act like placeholders in an inheritance hierarchy.
Specifically, they have no implementation, existing only to reserve
the name of a method and to associate pre- and post-conditions with it.

An abstract method cannot be directly called (although its associated
conditions may be). If such a method is ever invoked, it immediately
calls C<croak>. Therefore, the presence of an abstract method in a base
class requires the derived class to redefine that method, if the
derived class is to be usable. To ensure this, any constructor built by
Contract will refuse to create objects belonging to classes with
abstract methods.

Methods in a base class can also be declared as being I<private>:

        contract {
            private method 'remove';
                impl { pop @{self->queue} };
        };

Private methods may only be invoked by the class or one of its 
descendants. 

=head2 Inheritance and condition checking

Attribute accessors and object methods inherit I<all> post-conditions of
every ancestral accessor or method of the same name. Objects and classes
also inherit all invariants from any ancestor classes. That is,
methods accumulate all the post- and invariant checks that their
ancestors performed, as well as any new ones they define for themselves,
and must satisfy I<all> of them in order to execute successfully.

Pre-conditions are handled slightly differently. The principles of
design-by-contract programming state that pre-conditions in derived
classes can be no stronger than those in base classes (and may well be
weaker). In other words, a derived class must handle every case that
its base class handled, but may choose to handle other cases as well,
by being less demanding regarding its pre-conditions.

Meyers suggests an efficient way to achieve this relaxation of
constraints without the need for detailed logical analysis of
pre-conditions. His solution is to allow a derived class method or
accessor to run if I<either> the pre-conditions it inherits are
satisfied I<or> its own pre-conditions are satisfied. This is precisely
the semantics that Contract uses when checking pre-conditions in
derived classes.

=head2 A complete example

The following code implements a PriorityStack class, in which elements pushed
onto the stack "sink" until they encounter an element with lower priority.
Note the use of C<old> to check that object state has changed correctly, and
the use of explicit dispatch (e.g. C<self-E<gt>Stack::pop>) to invoke
inherited methods from the derived-class methods that redefine them.

        package PriorityStack;
        use Contract;

        contract {
            # Reuse existing implementation...
            inherits 'Stack';

            # Name the constructor (nothing special to do, so no implementation)
            ctor 'new';

            method 'push';
                # Check that data to be added is okay...
                pre  { defined $_[0] };
                    failmsg 'Cannot push an undefined value';
                pre  { $_[1] > 0 };
                    failmsg 'Priority must be greater than zero';

                # Check that push increases stack depth appropriately...
                post { self->count == old->count+1 };

                # Check that the right thing was left on top...
                post { old->top->{'priority'} <= self->top->{'priority'} };

                # Implementation reuses inherited methods: pop any higher
                # priority entries, push the new entry, then re-bury it...
                impl {
                    my ($newval, $priority) = @_[0,1];
                    my @betters;
                    unshift @betters, self->Stack::pop 
                        while self->count
                           && self->Stack::top->{'priority'} > $priority;
                    self->Stack::push( {'val'=>$newval, priority=>$priority} );
                    self->Stack::push( $_ )  foreach @betters;
                };

            method 'pop';
                # Check that pop decreases stack depth appropriately...
                post { self->count == old->count-1 };

                # Reuse inherited method...
                impl {
                    return  unless self->count;
                    return self->Stack::pop->{'val'};
                };

            method 'top';
                post { old->count == self->count }
                impl {
                    return  unless self->count;
                    return self->Stack::top->{'val'};
                };
        };


=head1 FUTURE WORK

Future work on Contract will concentrate on three areas:

=over 4

=item 1.  Improving the attribute accessor mechanism 

Lvalue subroutines will be introduced in perl version 5.6. They will allow
a return value to be treated as an alias for the (scalar) argument of a
C<return> statement. This will make it possible to write subroutines whose
return value may be assigned to (like the built-in C<pos> and C<substr>
functions).

In the absence of this feature, Contract accessors of all types
return a reference to their attribute, which then requires an explicit
dereference:

        ${self->value} = $newval;
        ${self->access_count}++;

When this feature is available, accessors for scalar attributes will be
able to return the actual attribute itself as an lvalue. The above code
would then become cleaner:

        self->value = $newval;
        self->access_count++;


=item 2.  Providing better software engineering tools.

Contracts make the consequences of inheritance harder to predict, since
they significantly increase the amount of ancestral behaviour (i.e.
contract clauses) that a class inherits.

Languages such as Eiffel provide useful tools to help the
software engineer make sense of this extra information. In
particular, Eiffel provides two alternate ways of inspecting a
particular class -- flat form and short form.

"Flattening" a class produces an equivalent class definition without any
inheritance. That is, the class is modified by making explicit all the
attributes, methods, conditions, and invariants it inherits from other
classes. This allows the designer to see every feature a class possesses
in one location.

"Shortening" a class, takes the existing class definition and removes all 
implementation aspects of it -- that is, those that have no bearing on its
public interface. A shortened representation of a class therefore has all
attribute specifications and method implementations removed. Note that
the two processes can be concatenated: shortening a flattened class
produces an explicit listing of its complete public interface. Such a
representation can be profitably used as a basis for documenting the
class.

It is envisaged that Contract will eventually provide a mechanism to 
produce equivalent class representations in Perl.


=item 3.  Offering better facilities for retrofitting contracts.

At present, adding contractual clauses to an existing class requires a
major restructuring of the original code. Clearly, if design-by-contract
is to gain popularity with Perl programmers, this transition cost must
be minimized.

It is as yet unclear how this might be accomplished, but one possibility
would be to allow the implementation of certain parts of a
Contract class (perhaps even the underlying object implementation
itself) to be user-defined.

=back

=head1 AUTHOR

Damian Conway (damian@conway.org)

=head1 MAINTAINER

C. Garrett Goebel (ggoebel@cpan.org)

=head1 BUGS

There are undoubtedly serious bugs lurking somewhere in code this funky :-)
Bug reports and other feedback are most welcome.

=head1 COPYRIGHT

Copyright (c) 1997-2000, Damian Conway. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the terms of the Perl Artistic License
  (see http://www.perl.com/perl/misc/Artistic.html)

Copyright (c) 2000-2001, C. Garrett Goebel. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the terms of the Perl Artistic License
  (see http://www.perl.com/perl/misc/Artistic.html)
