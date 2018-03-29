package IC::Routes::Manage;

use strict;
use warnings;

use IC::C::Manage;

use base qw( IC::Controller::Route );

IC::Controller::Route->route(
    pattern     => 'manage/index',
    controller  => 'manage',
    action      => 'index',
);

IC::Controller::Route->route(
    pattern     => 'manage/:_class/action/:_subclass/:_method',
    controller  => 'manage',
    action      => 'run_action_method',
);

IC::Controller::Route->route(
    pattern     => 'manage/:_class/:_method',
    controller  => 'manage',
    action      => 'run_action_method',
);

IC::Controller::Route->route(
    pattern     => 'manage/widget/menu/config',
    controller  => 'manage/widget/menu',
    action      => 'config',
);

IC::Controller::Route->route(
    pattern     => 'manage/widget/dashboard/config',
    controller  => 'manage/widget/dashboard',
    action      => 'config',
);

IC::Controller::Route->route(
    pattern     => 'manage/widget/dashboard/data',
    controller  => 'manage/widget/dashboard',
    action      => 'data',
);

IC::Controller::Route->route(
    pattern     => 'manage/widget/tools/common_actions/data',
    controller  => 'manage/widget/tools/common_actions',
    action      => 'data',
);

IC::Controller::Route->route(
    pattern     => 'manage/widget/tools/quick_access/data',
    controller  => 'manage/widget/tools/quick_access',
    action      => 'data',
);

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
