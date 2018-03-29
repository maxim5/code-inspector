=pod

This class should be subclassed by an app specific variant that will
do the registration of the controller name.

=cut
package IC::C::Manage;

use strict;
use warnings;

use JSON ();

use IC::M::Right;
use IC::Manage;

use IC::C::Manage::Widget::Menu;

use Moose;
extends 'IC::C';

has '+layout' => ( default => 'layouts/standard' );

has custom_js => (
    is      => 'rw',
    default => sub { {} },
);
has custom_css => (
    is      => 'rw',
    default => sub { {} },
);

no Moose;

# the application subclass should register itself as the provider of the 'manage' controller
#__PACKAGE__->registered_name('manage');

sub index {
    my $self = shift;

    # TODO: move this check into an 'around'
    return $self->forbid unless $self->check_right('access_site_mgmt');
    
    my $remote_function_url_template;
    my $remote_record_url_template;
    {
        #
        # prevent IC's normal URL processing from escaping the '{' and '}' characters
        # that we need in the template URL for post processing by the client libs
        #
        no warnings 'redefine';
        local *Vend::Util::escape_chars_url = sub { return $_[0]; };

        $remote_function_url_template = $self->url(
            controller => 'manage',
            action     => 'run_action_method',
            parameters => {
                _class  => '{clazz}',
                _method => 'ui_meta_struct',
            },
            get        => {
                _format => 'json',
            },
            match_security => 1,
        );
        $remote_record_url_template = $self->url(
            controller => 'manage',
            action     => 'run_action_method',
            parameters => {
                _class  => '{clazz}',
                _method => 'object_ui_meta_struct',
            },
            get        => {
                _format => 'json',
            },
            match_security => 1,
        );
    }

    #
    # build a configuration structure that will then be turned into JSON
    # and dropped into the view directly as a JSON object
    #
    my $ic_manage_config = {
        remote_function_url_template => $remote_function_url_template,
        remote_record_url_template   => $remote_record_url_template,
        dashboard_config             => {
            data_path => $self->url(
                controller     => 'manage/widget/dashboard',
                action         => 'data',
                match_security => 1,
            ),
        },
        window_config                => {
            menu_config => {
                config_path => $self->url(
                    controller     => 'manage/widget/menu',
                    action         => 'config',
                    match_security => 1,
                ),
            },
            tools_config => {
                common_actions => {
                    data_path => $self->url(
                        controller     => 'manage/widget/tools/common_actions',
                        action         => 'data',
                        match_security => 1,
                    ),
                },
                quick_access => {
                    data_path => $self->url(
                        controller     => 'manage/widget/tools/quick_access',
                        action         => 'data',
                        match_security => 1,
                    ),
                },
            },
        },
    };

    my $custom_groups;
    for my $method qw( custom_js custom_css ) {
        my $value = $self->$method;
        if (defined $value) {
            $custom_groups->{$method} = $value;
        }
    }
    if (defined $custom_groups) {
        $ic_manage_config->{YUI_config_additional_groups} = $custom_groups;
    }

    $self->render(
        layout  => '',
        context => {
            IC_manage_config => JSON::to_json(
                $ic_manage_config,
                {
                    utf8   => 1,
                    pretty => 1,
                },
            ),
        },
    );

    return;
}

#
# "action" here is a bit of a misnomer, it is really any function of a manage class/action
#
sub run_action_method {
    my $self = shift;
    my $log_prefix = 'Cannot run action method';

    return $self->forbid unless $self->check_right('access_site_mgmt');

    my $params = $self->parameters;

    my $_method = $params->{_method};
    unless (defined $_method and $_method ne '') {
        IC::Exception->throw("$log_prefix: not provided");
    }

    my ($class, $model);
    eval {
        ($class, $model) = IC::Manage->load_class(
            $params->{_class},
            $params->{_subclass},
        );
    };
    if (my $e = Exception::Class->caught) {
        IC::Exception->throw(
            sprintf '%s: failed to load class (%s:%s) - %s (%s)', $log_prefix, $params->{_class}, $params->{_subclass}, $e, $e->trace,
        );
    }

    unless (defined $class) {
        IC::Exception->throw("$log_prefix: load_class returned nothing ($params->{_class}:$params->{_subclass})");
    }

    my $invokee;
    if (defined $params->{_subclass}) {
        unless ($self->role->check_right('execute' => $model)) {
            IC::Exception->throw("$log_prefix: permission denied");
        }

        eval {
            $invokee = $class->new();
        };
        if (my $e = Exception::Class->caught) {
            IC::Exception->throw("$log_prefix: failed to instantiate manage class ($class): $e");
        }
    }
    else {
        # TODO: do we need to handle class method permission checks?
        $invokee = $class;
    }

    unless (defined $invokee) {
        IC::Exception->throw("$log_prefix: Unable to determine invokee ($params->{_class}:$params->{_subclass}:$_method)"); 
    }

    my $result = eval {
        #
        # we use a hash reference here so that method modifiers
        # have a location where they can munge arguments that get
        # seen further down the processing chain
        #
        my $context = {
            controller => $self,
        };
        my $struct = $context->{struct} = {};

        $invokee->$_method(
            context => $context,
        );

        my $status       = delete $struct->{_status} // '200 OK';
        my $content_type = delete $struct->{_content_type} // 'text/plain';

        my $formatted;
        if (! defined $params->{_format}) {
            $formatted = $struct;
        }
        elsif ($params->{_format} eq 'json') {
            $formatted = JSON::encode_json($struct);
        }
        else {
            IC::Exception->throw("$log_prefix: unrecognized struct format ($params->{_format})");
        }

        my $response = $self->response;
        $response->headers->status($status);
        $response->headers->content_type($content_type);
        $response->buffer( $formatted );
    };
    if (my $e = IC::Exception->caught()) {
        IC::Exception->throw("$log_prefix: failed manage method ($_method) execution (explicitly): $e (" . $e->trace . ')');
    }
    elsif ($@) {
        IC::Exception->throw("$log_prefix: failed manage method ($_method) execution: $@");
    }

    return;
}

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
