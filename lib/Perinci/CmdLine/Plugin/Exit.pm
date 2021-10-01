package Perinci::CmdLine::Plugin::Exit;

# put pragmas + Log::ger here
use 5.010001; # for defined-or
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
        summary => 'Exit program',
        prio => 99, # by default very low, run after other plugins
        conf => {
            exit_code => {
                schema => 'byte*',
                default => 1,
            },
        },
        tags => ['category:debugging'],
    };
}

sub after_action {
    require Data::Dump::Color;

    my ($self, $r) = @_;
    my $exit_code = $self->{exit_code} // 1;
    exit $exit_code;
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

To use, either specify in environment variable:

 PERINCI_CMDLINE_PLUGINS=-Exit

or in code instantiating L<Perinci::CmdLine>:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => ["Exit"],
 );

By default this plugin acts after the C<action> event. If you want to use at
a different event:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => [
         'Exit@after_validate_args',
     ],
 );

For list of plugin events available, see L<Perinci::CmdLine::Base/"Plugin
events">.


=head1 DESCRIPTION
