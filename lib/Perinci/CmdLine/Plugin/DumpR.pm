package Perinci::CmdLine::Plugin::DumpR;

# AUTHORITY
# DATE
# DIST
# VERSION

# IFUNBUILT
use strict;
use warnings;
# END IFUNBUILT
use Log::ger;

use parent 'Perinci::CmdLine::PluginBase';

sub meta {
    return {
        summary => 'Dump request stash ($r), by default after action',
        conf => {
        },
    };
}

sub after_action {
    require Data::Dump::Color;

    my ($self, $r) = @_;

    Data::Dump::Color::dd($r);
    [200, "OK"];
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

In the environment variable:

 PERINCI_CMDLINE_PLUGINS=-DumpR

In code instantiating L<Perinci::CmdLine>:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => ["DumpR"],
 );

If you want to dump at different events:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => [
         'DumpArgs@before_end',
     ],
 );

=head1 DESCRIPTION
