package Perinci::CmdLine::Plugin::DumpRes;

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
        summary => 'Dump result ($r->{res}), by default after action',
        conf => {
        },
        tags => ['category:debugging'],
    };
}

sub after_action {
    require Data::Dump::Color;

    my ($self, $r) = @_;

    Data::Dump::Color::dd($r->{res});
    [200, "OK"];
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

To use, either specify in environment variable:

 PERINCI_CMDLINE_PLUGINS=-DumpRes

or in code instantiating L<Perinci::CmdLine>:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => ["DumpRes"],
 );

By default this plugin acts after the C<action> event. If you want to dump at a
different event:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => [
         'DumpArgs@after_format_res',
     ],
 );

For list of plugin events available, see L<Perinci::CmdLine::Base/"Plugin
events">.


=head1 DESCRIPTION
