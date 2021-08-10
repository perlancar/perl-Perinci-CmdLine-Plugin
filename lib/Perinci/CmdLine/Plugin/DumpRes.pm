package Perinci::CmdLine::Plugin::DumpRes;

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
        summary => 'Dump result ($r->{res}), by default after action',
        conf => {
        },
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

If you want to dump at different events:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => [
         'DumpArgs@after_format_res',
     ],
 );


=head1 DESCRIPTION
