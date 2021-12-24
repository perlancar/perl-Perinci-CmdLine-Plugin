package Perinci::CmdLine::Plugin::Action::subcommands;

use 5.010001;
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
        summary => 'Handle the "subcommands" action',
        conf => {
        },
        prio => 50, # normal
        tags => ['category:action-handler'],
    };
}

sub on_action {
    my ($self, $r) = @_;

    return [100] unless $r->{action} eq 'subcommands';

    if (!$self->cmdline->subcommands) {
        say "There are no subcommands.";
        return [200];
    }

    say "Available subcommands:";
    my $scs = $self->cmdline->list_subcommands;
    my $longest = 6;
    for (keys %$scs) { my $l = length; $longest = $l if $l > $longest }
    $r->{res} = [
        200, "OK",
        join("",
             (map { sprintf("  %-${longest}s  %s\n",$_,$scs->{$_}{summary}//"") }
              sort keys %$scs),
         )];
    [200];
}

1;
# ABSTRACT:

=head1 SEE ALSO
