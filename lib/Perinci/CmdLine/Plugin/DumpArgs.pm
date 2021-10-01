package Perinci::CmdLine::Plugin::DumpArgs;

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
        summary => 'Dump command-line arguments ($r->{args}), by default after argument validation',
        conf => {
        },
    };
}

sub after_validate_args {
    require Data::Dump::Color;

    my ($self, $r) = @_;

    Data::Dump::Color::dd($r->{args});
    [200, "OK"];
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

To use, either specify in environment variable:

 PERINCI_CMDLINE_PLUGINS=-DumpArgs

or in code instantiating L<Perinci::CmdLine>:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => ["DumpArgs"],
 );

If you want to dump at different events:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => [
         'DumpArgs@before_validate_args',
         'DumpArgs@before_output',
     ],
 );

For list of plugin events available, see L<Perinci::CmdLine::Base/"Plugin
events">.


=head1 DESCRIPTION
