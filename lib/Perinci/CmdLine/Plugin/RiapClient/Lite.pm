package Perinci::CmdLine::Plugin::RiapClient::Lite;

use strict;
use warnings;
use Log::ger;
use parent 'Perinci::CmdLine::PluginBase';

# AUTHORITY
# DATE
# DIST
# VERSION

sub meta {
    return {
        summary => 'Use Perinci::Access::Lite as the Riap client',
        description => <<'_',

This plugin sets, in the `before_run` event, the default for
<pm:Perinci::CmdLine::Plugin>'s `riap_client` attribute to an instance of
<pm:Perinci::Access::Lite>.

This plugin is included by default.

There is also <pm:Perinci::CmdLine::Plugin::RiapClient::Full> which uses
<pm:Perinci::Access> as the <pm:Riap> client.

_
        conf => {
        },
        prio => 50, # normal
        tags => ['category:riap-client'],
    };
}

sub before_run {
    my ($self, $r) = @_;

    if (!$self->cmdline->{riap_client}) {
        #log_trace "Initializing Riap client ...";
        require Perinci::Access::Lite;
        my %rcargs = (
            riap_version => $self->cmdline->{riap_version} // 1.1,
            %{ $self->cmdline->{riap_client_args} // {} },
        );
        $self->cmdline->{riap_client} = Perinci::Access::Lite->new(%rcargs);
    }
    [200];
}

1;
# ABSTRACT:

=head1 SEE ALSO

L<Perinci::CmdLine::Plugin::RiapClient::Full>

L<Perinci::CmdLine::Plugin>

L<Riap>
