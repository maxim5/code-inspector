package IC::Manage;

use strict;
use warnings;

use JSON ();

use IC::Exceptions;
use IC::M::ManageClass;

use Moose;
use MooseX::ClassAttribute;

class_has '_class'                     => ( is => 'ro', default => undef );
class_has '_root_model_class'          => ( is => 'ro', default => 'IC::M' );
class_has '_model_class'               => ( is => 'ro', default => undef );
# TODO: can this be set based on _model_class?
class_has '_model_class_mgr'           => ( is => 'ro', default => undef );
class_has '_model_display_name'        => ( is => 'ro', default => undef );
class_has '_model_display_name_plural' => ( is => 'ro', default => undef );
class_has '_field_adjustments'         => ( is => 'ro', default => undef );
class_has '_role_class'                => ( is => 'ro', default => 'IC::M::Role' );
class_has '_object_load_with'          => ( is => 'ro', default => undef );

# order matters here, by making "DetailView" the first in this list
# it then becomes the default unless an override of _record_actions
# happens
class_has '_default_record_actions'    => ( is => 'ro', default => sub { [ qw( DetailView Drop ) ] } );

#
# TODO: the following groups still need to be factored
#

# TODO: should these four be included with a ManageRole?
class_has '_icon_path'                 => ( is => 'ro', default => '/ic/images/icons/file.png' );
class_has '_file_class'                => ( is => 'ro', default => 'IC::M::File' );
class_has '_file_resource_class'       => ( is => 'ro', default => 'IC::M::FileResource' );
class_has '_file_resource_class_mgr'   => ( is => 'ro', default => 'IC::M::FileResource::Manager' );

# TODO: include these in a ManageRole role? perhaps brought in if using the model mixin?
class_has '_upload_target_directory'   => ( is => 'ro', default => undef );
class_has '_upload_requires_object'    => ( is => 'ro', default => undef );

# TODO: make these a ManageRole?
class_has '_parent_manage_class'       => ( is => 'ro', default => undef );
class_has '_parent_model_link_field'   => ( is => 'ro', default => undef );

#
# make sure that the context which is provided as part of the argument list
# includes an 'object' and the 'used_params' properties, if not then use the
# controller (either argument or self) to build those and put them in the
# context literal
#
my $setup_object = sub {
    #warn "IC::Manage::object_ui_meta_struct|config(before)";
    my $self = shift;
    my %args = @_;

    my $context = $args{context};

    if (defined $context->{object}) {
        unless (defined $context->{used_params}) {
            # set used_params based on PK which we know will always work
            for my $pk_field (@{ $context->{object}->meta->primary_key_columns }) {
                my $field_name = $pk_field->name;
                $context->{used_params}->{"_pk_$field_name"} = $context->{object}->$field_name . '';
            }
        }
    }
    else {
        my $params;
        if (defined $context->{params}) {
            $params = $context->{params};
        }
        else {
            if (defined $context->{controller}) {
                $params = $context->{controller}->parameters; 
            }
            else {
                IC::Exception->throw("Can't determine controller to access parameters (and not passed)");
            }
        }

        my ($object, $used_params) = $self->object_from_params($params);

        $context->{object}      = $object;
        $context->{used_params} = $used_params;
    }

    return;
};

#
# newer Moose's can take an array as the first arg to before,
# so switch this when we've upgraded
#
before 'object_ui_meta_struct'        => $setup_object;
before 'object_ui_meta_struct_config' => $setup_object;

sub ui_meta_struct {
    #warn "IC::Manage::ui_meta_struct";
    my $self = shift;
    my %args = @_;

    $args{context}->{struct}->{'IC::Manage::ui_meta_struct'} = 1;

    if (ref $self) {
        inner();
    }
    else {
        $self->_class_ui_meta_struct(@_);
    }

    return;
}

sub class_ui_meta_struct_config {
    #warn "IC::Manage::class_ui_meta_struct_config";
    my $self = shift;
    my $args = { @_ };

    my $struct = $args->{context}->{struct};

    $struct->{'IC::Manage::class_ui_meta_struct_config'} = 1;

    inner();

    # this is one case where we pass the struct we want used as opposed
    # to getting it from the context args
    $self->_class_ui_meta_struct_config( $struct, @_ );

    return;
}

sub _class_ui_meta_struct {
    #warn "IC::Manage::_class_ui_meta_struct";
    my $class = shift;
    my %args  = @_;

    unless (defined $class->_class) {
        IC::Exception->throw("Sub class has not overridden _class: $class");
    }

    $args{context}->{struct}->{'IC::Manage::_class_ui_meta_struct'} = 1;

    my $renderer = $args{context}->{struct}->{renderer} ||= {};

    $renderer->{type}   = 'Tile';
    $renderer->{config} = {};

    # this is one case where we pass the struct we want used as opposed
    # to getting it from the context args
    $class->_class_ui_meta_struct_config( $renderer->{config}, @_ ),

    return;
}

sub _class_ui_meta_struct_config {
    #warn "IC::Manage::_class_ui_meta_struct_config";
    my $class  = shift;
    my $struct = shift;
    my %args   = @_;

    my $class_model_obj = $class->get_class_model_obj;

    my $action_models = $class_model_obj->find_actions(
        query => [
            is_primary => 1,
        ],
    );

    my $actions = {};
    for my $action_model (@$action_models) {
        my $action_class = $action_model->load_lib;
        my $action       = $action_class->new();

        my $sub_struct = $actions->{$action_model->code} = {};
        $action->ui_meta_struct(
            context => {
                struct     => $sub_struct,
                controller => $args{context}->{controller}
            },
        );
    }

    $struct->{'IC::Manage::_class_ui_meta_struct_config'} = 1;

    $struct->{title}   ||= $class->_model_display_name_plural; 
    $struct->{actions} ||= $actions;
    $struct->{url}     ||= $args{context}->{controller}->url(
        controller => 'manage',
        action     => 'run_action_method',
        parameters => {
            _class  => $class->_class,
            _method => 'class_ui_meta_struct_config',
        },
        get        => {
            # TODO: need to pass format through from params
            _format => 'json',
        },
        secure     => 1,
    );

    return;
}

sub object_ui_meta_struct {
    #warn "IC::Manage::object_ui_meta_struct";
    my $class = shift;
    my %args  = @_;

    my $used_params = $args{context}->{used_params};
    my $struct      = $args{context}->{struct};

    $struct->{'IC::Manage::object_ui_meta_struct'} = 1;

    $struct->{renderer}->{type}          ||= 'Tile';
    $struct->{renderer}->{config}->{url} ||= $args{context}->{controller}->url(
        controller => 'manage',
        action     => 'run_action_method',
        parameters => {
            _class  => $class->_class,
            _method => 'object_ui_meta_struct_config',
        },
        get        => {
            # TODO: need to pass format through from params
            _format => 'json',
            %{ $used_params },
        },
        secure     => 1,
    );

    inner();

    # this is one case where we pass the struct we want used as opposed
    # to getting it from the context args
    $class->_object_ui_meta_struct_config( $struct->{renderer}->{config}, @_ );

    return;
}

sub object_ui_meta_struct_config {
    #warn "IC::Manage::object_ui_meta_struct_config";
    my $class = shift;
    my %args  = @_;

    my $struct = $args{context}->{struct};

    $struct->{'IC::Manage::object_ui_meta_struct_config'} = 1;

    inner();

    # this is one case where we pass the struct we want used as opposed
    # to getting it from the context args
    $class->_object_ui_meta_struct_config( $struct, @_ );

    return;
}

#
# this is split out to allow getting it from the primary action,
# as well from a config only action used for reloading the config
#
sub _object_ui_meta_struct_config {
    #warn "IC::Manage::_object_ui_meta_struct_config";
    my $class  = shift;
    my $config = shift;
    my %args   = @_;

    $args{context}->{struct}->{'IC::Manage::_object_ui_meta_struct_config'} = 1;

    my $model_object = $args{context}->{object};

    $config->{title} ||= $class->_model_display_name . ': ' . $model_object->manage_description;

    my @actions = $class->_record_actions($model_object);
    for my $action (@actions) {
        $config->{actions}->{$action} ||= {};
    }

    #
    # post process the list of actions provided by the sub class
    #
    # post processing will set the label and meta information in
    # the case that they aren't already defined
    #
    if (defined $config->{actions}) {
        push @actions, keys %{ $config->{actions} };
    }

    my $class_model_obj = $class->get_class_model_obj;

    my $action_models = $class_model_obj->find_actions(
        query => [
            is_primary => 0,
            code       => \@actions,
        ],
    );

    for my $action_model (@$action_models) {
        my $action_class = $action_model->load_lib;
        my $action       = $action_class->new();

        my $action_ref = $config->{actions}->{ $action_model->code } ||= {};
        unless (defined $action_ref->{label}) {
            $action_ref->{label} = $action_model->display_label; 
        }

        my $sub_struct_key = $action->display_type;
        if (! defined $action_ref->{$sub_struct_key}) {
            my $sub_struct = $action_ref->{$sub_struct_key} = {};
            $action->ui_meta_struct(
                context => {
                    object     => $model_object,
                    struct     => $sub_struct,
                    controller => $args{context}->{controller}
                },
            );
        }
    }

    return;
}

no Moose;
no MooseX::ClassAttribute;

sub get_class_model_obj {
    my $class = shift;

    my $class_obj = IC::M::ManageClass->new(
        code => $class->_class,
    );
    unless ($class_obj->load( speculative => 1 )) {
        IC::Exception->throw('Unrecognized class: ' . $class->_class);
    }

    return $class_obj;
}

sub load_class {
    my $self = shift;
    my $_class = shift;
    my $_subclass = shift;
      
    my $model_class = 'IC::M::ManageClass';
    my %model_args;
    if (defined $_subclass) {
        $model_class .= '::Action';

        %model_args = (
            class_code => $_class,
            code       => $_subclass,
        );
    }
    else {
        %model_args = (
            code => $_class,
        );
    }

    my $model = $model_class->new(%model_args);
    unless ($model->load( speculative => 1 )) {
        IC::Exception->throw("Can't load $model_class model object: $_class ($_subclass)");
    }

    my $class = $model->load_lib;

    return wantarray ? ($class, $model) : [ $class, $model ];
}

sub object_from_params {
    my $self = shift;
    my $params = shift;
    my %args = @_;

    my $_model_class = $self->_model_class;

    my @pk_fields  = @{ $_model_class->meta->primary_key_columns };
    my @_pk_fields = map { "_pk_$_" } @pk_fields;

    for my $_pk_field (@_pk_fields) {
        unless (defined $params->{$_pk_field}) {
            IC::Exception::MissingValue->throw( "PK argument ($_pk_field): Unable to retrieve object" );
        }
    }

    my %object_params;
    my %used_params;
    for my $pk_field (@pk_fields) {
        $object_params{$pk_field->name} = $params->{'_pk_' . $pk_field->name};
        $used_params{'_pk_' . $pk_field->name} = $params->{'_pk_' . $pk_field->name};
    }

    my $object = $_model_class->new( %object_params );
    unless (defined $object) {
        IC::Exception::ModelInstantiateFailure->throw( $self->_model_display_name );
    }

    my %load_params = (
        speculative => 1,
        (defined $self->_object_load_with ? (with => $self->_object_load_with) : ()),
        (defined $args{load_params} ? %{ $args{load_params} } : ()),
    );

    unless ($object->load(%load_params)) {
        IC::Exception::ModelLoadFailure->throw( 'Unrecognized ' . $self->_model_display_name . ': ' . (join ' => ', %object_params) );
    }

    return wantarray ? ($object, \%used_params) : $object;
}

#
# given a manage class action code (name) determine whether a given or derived role
# has access rights to execute the function
#
sub check_priv {
    my $self = shift;
    my $check_action_name = shift;
    my $args = { @_ };

    my $check_role;
    if (defined $args->{role} and $args->{role}->isa($self->_role_class)) {
        $check_role = $args->{role};
    }
    else {
        my ($package, $filename, $line) = caller(0);
        warn "$package called check_priv() but was unable to determine 'role' properties at line $line\n";
        return '';
    }

    # TODO: I'm not wild about this
    my ($class, $model) = $self->load_class($self->_class, $check_action_name);

    return ($check_role->check_right( 'execute' => $model ) ? 1 : 0);
}

#
# takes an object and a set of field names and returns a configuration
# structure that can be plugged directly into a KeyValue renderer's 
# data property
#
# TODO: cache some of this on a per manage class basis so we don't
#       need to rebuild the structures at least within a request
#
sub get_KeyValue_data {
    my $self = shift;
    my $object = shift;
    my $args = { @_ };

    my @all_fields = ($self->_model_class->meta->columns, $self->_model_class->meta->nonpersistent_columns);

    my $select_fields;
    if (defined $args->{field_names}) {
        my $fields_by_name = {
            map { $_->name => $_ } @all_fields
        };
        $select_fields = [
            map {
                $fields_by_name->{$_}
            } @{ $args->{field_names} }
        ];
    }
    else {
        $select_fields       = \@all_fields;
        $args->{field_names} = [ map { $_->name } @all_fields ];
    }

    my $data_kvs = $self->_fields_to_kv_defs(
        object => $object,
        fields => $select_fields,
    );

    my $data = [
        map {
            {
                label => $data_kvs->{$_}->{label},
                value => $data_kvs->{$_}->{value},
            }
        } @{ $args->{field_names} },
    ];

    return $data;
}

#
# takes a set of named args, specifically a list of fields that we want to display 
# in a common key/value pair manner and the object used to derive the values
#
sub _fields_to_kv_defs {
    my $self = shift;
    my $args = { @_ };

    #
    # return a hash keyed on the field name passed in with a value
    # that is the corresponding definition as a hashref
    #
    my $return = {};

    #
    # get the structure used to override default behaviour for the fields,
    # it is keyed by DB field name
    #
    my $adjustments = $self->_field_adjustments || {};

    for my $field (@{ $args->{fields} }) {
        unless (defined $field) {
            warn "IC::Manage::_fields_to_kv_defs - Undefined field requested\n";
            next;
        }

        my $field_name = $field->name;
        my $adjust     = $adjustments->{$field_name} || {};

        my $ref = $return->{$field_name} = {
            code => $field_name,
        };

        my $label;
        if (defined $adjust->{label}) {
            $label = $adjust->{label};
        }
        else {
            $label = join ' ', map { $_ eq 'id' ? 'ID' : ucfirst } split /_/, $field_name;
        }
        $ref->{label} = $label;

        if (defined $args->{object}) {
            my $value;

            if (defined $adjust->{value_mapping}) {
                my $alt_object = $args->{object};
                if (defined $adjust->{value_mapping}->{object_accessor}) {
                    my $alt_object_method = $adjust->{value_mapping}->{object_accessor};
                    $alt_object = $args->{object}->$alt_object_method;
                }

                if (defined $adjust->{value_mapping}->{value_accessor}) {
                    my $sub_method = $adjust->{value_mapping}->{value_accessor};
                    $value = $alt_object->$sub_method;
                }
                else {
                    $value = $alt_object->$field_name;
                }
            }
            else {
                if ($field->type eq 'date') {
                    $value = $args->{object}->$field_name( format => '%Y-%m-%d' );
                }
                else {
                    $value = $args->{object}->$field_name;
                }
            }

            # force stringification with a concat
            $ref->{value} = defined($value) ? $value . '' : '';
        }
        else {
            # TODO: add ability to pull default value using an adjustment sub
        }
    }

    return $return;
}

#
# takes a set of named args, specifically a list of fields that we want form 
# definitions for and an object that should be used for determining values
#
sub _fields_to_field_form_defs {
    my $self = shift;
    my $args = { @_ };

    $args->{values} ||= {};

    #
    # return a hash keyed on the field name passed in with a value
    # that is the corresponding form definition as a hashref
    #
    my $return = {};

    #
    # get the structure used to override default behaviour for the fields,
    # it is keyed by DB field name
    #
    my $adjustments = $self->_field_adjustments || {};

    for my $field (@{ $args->{fields} }) {
        my $field_name = $field->name;
        my $adjust     = $adjustments->{$field_name} || {};

        #
        # boilerplate fields by their nature are automatically handled, therefore they aren't
        # editable, so skip them
        #
        next if grep { $field_name eq $_ } keys %{ { $self->_model_class->boilerplate_columns } };

        #
        # in the case of a passed in object we know it is edit, so skip fields
        # that can't be edited, otherwise it is add and skip those that shouldn't
        # be used in an add
        #
        if (defined $args->{object}) {
            next if (defined $adjust->{is_editable} and not $adjust->{is_editable});

            if (ref $adjust->{is_editable} eq 'CODE') {
                next if not $adjust->{is_editable}->($self, $args->{object});
            }
        }
        else {
            next if (defined $adjust->{is_addable} and not $adjust->{is_addable});
        }

        #
        # by default we make single field PKs that are integers with the name 'id'
        # not be editable, this can be overridden by specifying an is_editable adjustment
        #
        next if (
            $field_name eq 'id' 
            and $field->is_primary_key_member
            and $field->type eq 'serial'
            and not defined $adjust->{can_edit} 
        );

        #
        # fields is an array to allow for multiple fields to make up a single value
        # for the database field, i.e. password resets are handled through two values
        # 'new' and 'confirmed' even though it gets saved to the DB as a single value
        #
        my $def = {
            controls => [],
        };
        $return->{$field_name} = $def;

        if (defined $adjust->{controls}) {
            $def->{controls} = $adjust->{controls};
        }
        else {
            # TODO: give date, time, datetime, timestamp multiple controls, or can be done with
            #       one field type that is rendered using multiple client side controls?

            # TODO: add auto handling of FK relationships when it is trivial to handle them
            my $name = $field_name;

            # this is irritating, and necessary because IC eats "id" parameters
            if ($field_name eq 'id') {
                $name = '_work_around_ic_id';
            }

            my $control_ref = {
                name => $name,
            };
            push @{ $def->{controls} }, $control_ref;

            # see if a value was provided, if so, maintain it
            if (defined $args->{values}->{$name}) {
                $control_ref->{value} = $args->{values}->{$name};
            }
            elsif (defined $args->{object}) {
                $control_ref->{value} = $args->{object}->$field_name();
            }

            if (defined $adjust->{field_type}) {
                $control_ref->{type} = $adjust->{field_type};
            }
            else {
                if ($field->type eq 'text') {
                    $control_ref->{type} = 'TextareaField';
                }
                elsif ($field->type eq 'boolean') {
                    # use our own wrapper class because gallery-form's choicefield doesn't
                    # allow for pre-selected value
                    $control_ref->{type} = 'Radio';
                }
                elsif ($field->type eq 'date') {
                    # TODO: does this imply a validator?
                    $control_ref->{type} = 'Calendar';

                    if (ref $control_ref->{value}) {
                        $control_ref->{value} = $control_ref->{value}->strftime('%Y-%m-%d');
                    }
                }
                elsif ($field->type eq 'time') {
                    # TODO: does this imply a validator?
                    $control_ref->{type} = 'Time';

                    if (ref $control_ref->{value}) {
                        $control_ref->{value} = $control_ref->{value}->format('%H:%M:%S');
                    }
                }
                elsif ($field->type eq 'timestamp') {
                    # TODO: does this imply a validator?
                    $control_ref->{type} = 'CalendarWithTime';
                }
                else {
                    $control_ref->{type} = 'TextField';
                }
            }

            # force stringification, can't do this with only objects because
            # to the JSON encoder an integer is encoded differently if it
            # hasn't been stringified too
            $control_ref->{value} ||= '';
            $control_ref->{value} = $control_ref->{value} . '';

            if (grep { $control_ref->{type} eq $_ } qw( Radio CheckboxField SelectField )) {
                if (defined $adjust->{get_choices}) {
                    $control_ref->{choices} = $adjust->{get_choices}->($self, $args->{object});
                }
                elsif (grep { $_ eq $control_ref->{type} } qw( Radio ) and $field->type eq 'boolean') {
                    # these choices take strings instead of 0/1 because gallery-form doesn't handle 0/1 well ATM
                    $control_ref->{choices} = [
                        {
                            value => 'true', #1,
                            label => 'Yes',
                            ($control_ref->{value} ? (checked => JSON::true()) : ()),
                        },
                        {
                            value => 'false', #0,
                            label => 'No',
                            (! $control_ref->{value} ? (checked => JSON::true()) : ()),
                        },
                    ];
                }
                else {
                    $control_ref->{choices} = [];
                }
            }
            if (defined $adjust->{client_validator}) {
                $control_ref->{validator} = $adjust->{client_validator};
            }
        }
    }

    return $return;
}

sub _record_actions {
    my $self = shift;
    my $object = shift;

    return (
        @{ $self->_default_record_actions },
        @{ $self->_custom_record_actions($object) },
    );
}

sub _custom_record_actions { [] }

1;

__END__

=pod

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008-2010 End Point Corporation, http://www.endpoint.com/

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see: http://www.gnu.org/licenses/ 

=cut
