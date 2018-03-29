# vim: sw=4 sts=4 et tw=75 wm=5
package EventBot::WWW::C::Manage;
use strict;
use warnings;
use parent 'Catalyst::Controller';
use MIME::Lite;
use feature ':5.10';

=head1 NAME

EventBot::WWW::C::Manage

=head1 DESCRIPTION

Controller for managing events:
 * Nominate pubs.
 * Endorse pubs.
 * Select birthday pubs and other special events.

=head1 METHODS

=cut

=head2 confirm

Accept a returning confirmation URL and then confirm the related object.

=cut

sub confirm :Local {
    my ($self, $c, $code) = @_;

    my $confirm;
    if ($code =~ /^([\w\d]{4,32})\s*$/) {
        $confirm = $c->model('DB::Confirmations')->find( { code => $1 } );
    }
    if (not $confirm) {
        $c->response->body('Error - invalid confirmation code');
        $c->response->status(404);
        return;
    }

    my $foreign = $confirm->object;
    # TODO: This will need to be expanded to a more complex system later..
    given ($confirm->action) {
        when ('create') {
            $foreign->confirmed(1);
            $foreign->update;
        }
    }
    $confirm->delete;
    # TODO: Redirect to destination based on $confirm's object type:
    $c->response->redirect('/manage/specialevent/' . $foreign->id);
}

sub manage_base : Chained PathPart('manage') CaptureArgs(0) {
    my ($self, $c) = @_;
    # Just a chain start-point for all management chains.
}

sub manage : Chained('manage_base') PathPart('') Args(0) {
    my ($self, $c) = @_;
    # Just cause the template to be displayed..
}

1;
