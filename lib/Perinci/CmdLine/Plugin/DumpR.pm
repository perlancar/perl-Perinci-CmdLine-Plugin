package Perinci::CmdLine::Plugin::DumpR;

# put pragmas + Log::ger here
use strict;
use warnings;
use Log::ger;
use parent 'Perinci::CmdLine::PluginBase';

# put other modules alphabetically here

# put global variables alphabetically here
# AUTHORITY
# DATE
# DIST
# VERSION

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

To use, either specify in environment variable:

 PERINCI_CMDLINE_PLUGINS=-DumpR

or in code instantiating L<Perinci::CmdLine>:

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
