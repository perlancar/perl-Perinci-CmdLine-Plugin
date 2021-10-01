package Perinci::CmdLine::Plugin::DisablePlugin;

# put pragmas + Log::ger here
use strict;
use warnings;
use parent 'Perinci::CmdLine::PluginBase';
use Log::ger;

# put other modules alphabetically here

# put global variables alphabetically here
# AUTHORITY
# DATE
# DIST
# VERSION

sub meta {
    return {
        summary => 'Prevent the loading (activation) of other plugins',
        conf => {
            plugins => {
                summary => 'List of plugin names or regexes',
                description => <<'_',

Plugins should be an array of plugin names or regexes, e.g.:

    ['Foo', 'Bar', qr/baz/]

To make it easier to specify via environment variable (PERINCI_CMDLINE_PLUGINS),
a semicolon-separated string is also accepted. A regex should be enclosed in
"/.../". For example:

    Foo;Bar;/baz/

_
                schema => ['any*', of=>[
                    'str*',
                    ['array*', of=>['any*', of=>['str*', 're*']]],
                ]],
                req => 1,
            },
        },
    };
}

sub new {
    my ($class, %args) = (shift, @_);
    $args{plugins} or die "Please specify plugins to disable";
    unless (ref $args{plugins} eq 'ARRAY') {
        $args{plugins} =
            [map { m!\A/(.*)/\z! ? qr/$1/ : $_ } split /;/, $args{plugins}];
    }
    $class->SUPER::new(%args);
}

sub before_activate_plugin {
    my ($self, $r) = @_;

    for my $el (@{ $self->{plugins} }) {
        if (ref $el eq 'Regexp') {
            next unless $r->{plugin_name} =~ $el;
        } else {
            next unless $r->{plugin_name} eq $el;
        }
        log_info "[pericmd DisablePlugin] Disabling loading of Perinci::CmdLine plugin '$r->{plugin_name}'";
        return [601, "Cancel"];
    }
    [200, "OK"];
}

1;
# ABSTRACT:

=for Pod::Coverage ^(.+)$

=head1 SYNOPSIS

To use, either specify in environment variable:

 PERINCI_CMDLINE_PLUGINS='-DisablePlugin,plugins,Foo;/^Bar/'

or in code instantiating L<Perinci::CmdLine>:

 my $app = Perinci::CmdLine::Any->new(
     ...
     plugins => [DisablePlugin => {plugins=>["Foo", qr/^Bar/]}],
 );

For list of plugin events available, see L<Perinci::CmdLine::Base/"Plugin
events">.


=head1 DESCRIPTION
