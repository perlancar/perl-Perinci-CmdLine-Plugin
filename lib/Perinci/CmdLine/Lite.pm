package Perinci::CmdLine::Lite;

# DATE
# VERSION

use 5.010001;
# use strict; # already enabled by Mo
# use warnings; # already enabled by Mo
use Log::Any '$log';

use Mo qw(build default);
#use Moo;
extends 'Perinci::CmdLine::Base';

# when debugging, use this instead of the above because Mo doesn't give clear
# error message if base class has errors.
#use parent 'Perinci::CmdLine::Base';

has default_prompt_template => (
    is=>'rw',
    default => 'Enter %s: ',
);
has log => (
    is=>'rw',
    default => sub {
        if (defined $ENV{LOG}) {
            return $ENV{LOG};
        } elsif ($ENV{LOG_LEVEL} && $ENV{LOG_LEVEL} =~ /^(off|none)$/) {
            return 0;
        } elsif ($ENV{LOG_LEVEL} || $ENV{TRACE} || $ENV{DEBUG} ||
                     $ENV{VERBOSE} || $ENV{QUIET}) {
            return 0;
        }
        0;
    },
);
has log_level => (
    is=>'rw',
    default => sub {
        if ($ENV{LOG_LEVEL}) {
            return $ENV{LOG_LEVEL};
        } elsif ($ENV{TRACE}) {
            return 'trace';
        } elsif ($ENV{DEBUG}) {
            return 'debug';
        } elsif ($ENV{VERBOSE}) {
            return 'info';
        } elsif ($ENV{QUIET}) {
            return 'error';
        }
        'warning';
    },
);

my $formats = [qw/text text-simple text-pretty json json-pretty/];

sub BUILD {
    my ($self, $args) = @_;

    if (!$self->{riap_client}) {
        require Perinci::Access::Lite;
        my %rcargs = (
            riap_version => $self->{riap_version} // 1.1,
            %{ $self->{riap_client_args} // {} },
        );
        $self->{riap_client} = Perinci::Access::Lite->new(%rcargs);
    }

    if (!$self->{actions}) {
        $self->{actions} = {
            call => {},
            version => {},
            subcommands => {},
            help => {},
        };
    }

    my $_t = sub {
        no warnings;
        my $co_name = shift;
        my $href = $Perinci::CmdLine::Base::copts{$co_name};
        %$href;
    };

    if (!$self->{common_opts}) {
        my $copts = {};

        $copts->{version}   = { $_t->('version'), };
        $copts->{help}      = { $_t->('help'), };
        $copts->{format}    = {
            $_t->('format'),
            schema => ['str*' => in => $formats],
        };
        $copts->{json}      = { $_t->('json'), };
        $copts->{naked_res} = { $_t->('naked_res'), };
        if ($self->subcommands) {
            $copts->{subcommands} = { $_t->('subcommands'), };
        }
        if ($self->default_subcommand) {
            $copts->{cmd} = { $_t->('cmd') };
        }
        if ($self->read_config) {
            $copts->{config_path}    = { $_t->('config_path') };
            $copts->{no_config}      = { $_t->('no_config') };
            $copts->{config_profile} = { $_t->('config_profile') };
        }
        if ($self->log) {
            $copts->{log_level} = { $_t->('log_level'), };
            $copts->{trace}     = { $_t->('trace'), };
            $copts->{debug}     = { $_t->('debug'), };
            $copts->{verbose}   = { $_t->('verbose'), };
            $copts->{quiet}     = { $_t->('quiet'), };
        }
        $self->{common_opts} = $copts;
    }

    $self->{formats} //= $formats;

    $self->{per_arg_json} //= 1;
}

my %ph; # patch handles
my $setup_progress;
sub _setup_progress_output {
    my $self = shift;

    return unless $ENV{PROGRESS} // (-t STDOUT);

    require Progress::Any::Output;
    my $out = Progress::Any::Output->set("TermProgressBarColor");
    $setup_progress = 1;

    # we need to patch the logger adapters so it won't interfere with
    # progress meter's output
    require Monkey::Patch::Action;
    for my $meth (Log::Any->logging_methods) {
        $ph{$meth} = Monkey::Patch::Action::patch_package(
            'Log::Any::Adapter::ScreenColoredLevel', $meth,
            'wrap', sub {
                my $ctx = shift;
                #my ($self, $msg, @params) = @_;
                my $self = $_[0];

                return unless $Log::Any::Adapter::ScreenColoredLevel::logging_levels{$meth} <
                    $Log::Any::Adapter::ScreenColoredLevel::logging_levels{$self->{min_level}};

                # clean currently displayed progress bar first
                if ($out->{lastlen}) {
                    print
                        "\b" x $out->{lastlen},
                            " " x $out->{lastlen},
                                "\b" x $out->{lastlen};
                    undef $out->{lastlen};
                }

                # force output update so progress bar is displayed again
                # immediately
                $Progress::Any::output_data{"$out"}{force_update} = 1;

                $ctx->{orig}->(@_);
            },
        ) if defined &{"Log::Any::Adapter::ScreenColoredLevel::$meth"};
    }
}

sub _unsetup_progress_output {
    my $self = shift;

    return unless $setup_progress;
    my $out = $Progress::Any::outputs{''}[0];
    $out->cleanup if $out->can("cleanup");
    delete $ph{$_} for keys %ph;
    $setup_progress = 0;
}

sub hook_before_run {}

sub hook_after_parse_argv {
    my ($self, $r) = @_;

    # since unlike Perinci::CmdLine, we don't wrap the function (where the
    # wrapper assigns default values for arguments), we must do it here
    # ourselves.
    my $ass  = $r->{meta}{args} // {};
    my $args = $r->{args};
    for (keys %$ass) {
        next if exists $args->{$_};
        my $as = $ass->{$_};
        if (exists $as->{default}) {
            $args->{$_} = $as->{default};
        } elsif ($as->{schema} && exists $as->{schema}[1]{default}) {
            $args->{$_} = $as->{schema}[1]{default};
        }
    }

    # set up log adapter
    if ($self->log) {
        require Log::Any::Adapter;
        Log::Any::Adapter->set(
            'ScreenColoredLevel',
            min_level => $r->{log_level} // $self->log_level,
            formatter => sub { $self->program_name . ": $_[1]" },
        );
    }
}

sub hook_format_result {
    my ($self, $r) = @_;

    my $res    = $r->{res};
    my $format = $r->{format} // 'text';
    my $meta   = $r->{meta};

    if ($format =~ /\Atext(-simple|-pretty)?\z/) {
        my $is_pretty = $format eq 'text-pretty' ? 1 :
            $format eq 'text-simple' ? 0 : (-t STDOUT);
        no warnings 'uninitialized';
        if ($res->[0] !~ /^(2|304)/) {
            my $fres = "ERROR $res->[0]: $res->[1]";
            if (my $prev = $res->[3]{prev}) {
                $fres .= " ($prev->[0]: $prev->[1])";
            }
            return "$fres\n";
        } elsif ($res->[3] && $res->[3]{"x.hint.result_binary"}) {
            return $res->[2];
        } else {
            require Data::Check::Structure;
            my $data = $res->[2];
            my $max = 5;
            if (!ref($data)) {
                $data //= "";
                $data .= "\n" unless !length($data) || $data =~ /\n\z/;
                return $data;
            } elsif (ref($data) eq 'ARRAY' && !@$data) {
                return "";
            } elsif (Data::Check::Structure::is_aos($data, {max=>$max})) {
                return join("", map {"$_\n"} @$data);
            } elsif (Data::Check::Structure::is_aoaos($data, {max=>$max})) {
                if ($is_pretty) {
                    require Text::Table::Tiny;
                    return Text::Table::Tiny::table(rows=>$data) . "\n";
                } else {
                    return join("", map {join("\t", @$_)."\n"} @$data);
                }
            } elsif (Data::Check::Structure::is_hos($data, {max=>$max})) {
                if ($is_pretty) {
                    require Text::Table::Tiny;
                    $data = [map {[$_, $data->{$_}]} sort keys %$data];
                    unshift @$data, ["key", "value"];
                    return Text::Table::Tiny::table(rows=>$data) . "\n";
                } else {
                    return join("", map {"$_\t$data->{$_}\n"} sort keys %$data);
                }
            } elsif (Data::Check::Structure::is_aohos($data, {max=>$max})) {
                # collect all mentioned fields
                my %fieldnames;
                for my $row (@$data) {
                    $fieldnames{$_}++ for keys %$row;
                }
                my @fieldnames = sort keys %fieldnames;
                my $newdata = [];
                for my $row (@$data) {
                    push @$newdata, [map {$row->{$_}} @fieldnames];
                }
                if ($is_pretty) {
                    unshift @$newdata, \@fieldnames;
                    require Text::Table::Tiny;
                    return Text::Table::Tiny::table(rows=>$newdata) . "\n";
                } else {
                    return join("", map {join("\t", @$_)."\n"} @$newdata);
                }
            } else {
                $format = 'json-pretty';
            }
        }
    }

    $res = $res->[2] if $r->{naked_res};

    warn "Unknown format '$format', fallback to json-pretty"
        unless $format =~ /\Ajson(-pretty)?\z/;
    state $cleanser = do {
        require Data::Clean::JSON;
        Data::Clean::JSON->get_cleanser;
    };
    $cleanser->clean_in_place($res);
    state $json = do {
        require JSON;
        JSON->new->canonical(1)->allow_nonref;
    };
    if ($format eq 'json') {
        return $json->encode($res) . "\n";
    } else {
        return $json->canonical(1)->pretty->encode($res);
    }
}

sub hook_format_row {
    my ($self, $r, $row) = @_;

    if (ref($row) eq 'ARRAY') {
        return join("\t", @$row) . "\n";
    } else {
        return ($row // "") . "\n";
    }
}

sub hook_display_result {
    my ($self, $r) = @_;
    $self->display_result($r);
}

sub hook_after_run {
    my ($self, $r) = @_;
    $self->_unsetup_progress_output;
}

sub hook_after_get_meta {
    my ($self, $r) = @_;

    require Perinci::Object;
    if (Perinci::Object::risub($r->{meta})->can_dry_run) {
        $self->common_opts->{dry_run} = {
            getopt  => 'dry-run',
            summary => "Run in simulation mode (also via DRY_RUN=1)",
            handler => sub {
                my ($go, $val, $r) = @_;
                $r->{dry_run} = 1;
                #$ENV{VERBOSE} = 1;
            },
        };
    }
}

sub run_subcommands {
    my ($self, $r) = @_;

    if (!$self->subcommands) {
        say "There are no subcommands.";
        return 0;
    }

    say "Available subcommands:";
    my $scs = $self->list_subcommands;
    my $longest = 6;
    for (keys %$scs) { my $l = length; $longest = $l if $l > $longest }
    [200, "OK",
     join("",
          (map { sprintf("  %-${longest}s  %s\n",$_,$scs->{$_}{summary}//"") }
               sort keys %$scs),
      )];
}

sub run_version {
    my ($self, $r) = @_;

    my $meta = $r->{meta} = $self->get_meta($r, $self->url);

    [200, "OK",
     join("",
          $self->get_program_and_subcommand_name($r),
          " version ", ($meta->{entity_v} // "?"),
          ($meta->{entity_date} ? " ($meta->{entity_date})" : ''),
          "\n",
          "  ", __PACKAGE__,
          " version ", ($Perinci::CmdLine::Lite::VERSION // "?"),
          ($Perinci::CmdLine::Lite::DATE ?
               " ($Perinci::CmdLine::Lite::DATE)":''),
      )];
}

sub run_help {
    require Perinci::CmdLine::Help;

    my ($self, $r) = @_;

    my @help;
    my $scn    = $r->{subcommand_name};
    my $scd    = $r->{subcommand_data};

    # XXX use 'delete local' when we bump minimal perl to 5.12
    my $common_opts = { %{$self->common_opts} };
    # hide usage '--subcommands' if we have subcommands but user has specified a
    # subcommand to use
    my $has_sc_no_sc = $self->subcommands && !length($r->{subcommand_name});
    delete $common_opts->{subcommands} if $self->subcommands && !$has_sc_no_sc;

    my $meta = $self->get_meta($r, $scd->{url} // $self->{url});

    my $res = Perinci::CmdLine::Help::gen_help(
        program_name => $self->get_program_and_subcommand_name($r),
        program_summary => ($scd ? $scd->{summary}:undef ) // $meta->{summary},
        program_description => $scd ? $scd->{description} : undef,
        meta => $meta,
        subcommands => $has_sc_no_sc ? $self->subcommands : undef,
        common_opts => $common_opts,
        per_arg_json => $self->per_arg_json,
        per_arg_yaml => $self->per_arg_yaml,
    );

    $res->[3]{"cmdline.skip_format"} = 1;
    $res;
}

sub run_call {
    my ($self, $r) = @_;

    my %extra;
    if ($r->{send_argv}) {
        $log->tracef("[pericmd] Sending argv to server: %s", $extra{argv});
        $extra{argv} = $r->{orig_argv};
    } else {
        $extra{args} = $r->{args};
    }

    $extra{stream_arg} = 1 if $r->{stream_arg};

    my $url = $r->{subcommand_data}{url};

    # currently we don't log args because it's potentially large
    $log->tracef("[pericmd] Riap request: action=call, url=%s", $url);

    # setup output progress indicator
    if ($r->{meta}{features}{progress}) {
        $self->_setup_progress_output;
    }

    $self->riap_client->request(
        call => $url, \%extra);
}

1;
# ABSTRACT: A lightweight Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(BUILD|get_meta|hook_.+|run_.+)$

=head1 SYNOPSIS

See L<Perinci::CmdLine::Manual::Examples>.


=head1 DESCRIPTION

Perinci::CmdLine::Lite (hereby P::C::Lite) is a lightweight (low startup
overhead, minimal dependencies) alternative to L<Perinci::CmdLine> (hereby
P::C). It offers a subset of functionality and a compatible API. Unless you use
the unsupported features of P::C, P::C::Lite is a drop-in replacement for P::C
(also see L<Perinci::CmdLine::Any> for automatic fallback).

P::C::Lite stays lightweight by avoiding the use of libraries that have large
dependencies or add too much to startup overhead. This includes
L<Perinci::Access> for metadata access, L<Data::Sah> for validator generation,
L<Text::ANSITable> for formatting results, and L<Log::Any::App> (which uses
L<Log::Log4perl>) for logging.

P::C::Lite attributes default to condition of low startup overhead. For example,
C<log> is by default off instead of on like in P::C.

I first developed P::C::Lite mainly for CLI applications that utilize shell tab
completion as their main feature, e.g. L<App::PMUtils>, L<App::ProgUtils>,
L<App::GitUtils>.

Below is summary of the differences between P::C::Lite and P::C:

=over

=item * P::C::Lite starts much faster

The target is under 0.04-0.05s to make shell tab completion convenient. On the
other hand, P::C application can spend between 0.20-0.50s before displaying
output, due to various abovementioned overheads: loading more libraries,
validator code generation, etc.

=item * P::C::Lite uses simpler formatting

Instead of L<Perinci::Result::Format> (especially for 'text*' formats which use
L<Data::Format::Pretty::Console> and L<Text::ANSITable>), to keep dependencies
minimal and formatting quick, P::C::Lite uses the following simple rules that
work for a significant portion of common data structures:

1) if result is undef, print nothing.

2) if result is scalar, print it (with newline automatically added).

3) if result is an array of scalars (check at most 5 first rows), print it one
line for each element.

4) if result is a hash of scalars (check at most 5 keys), print a two column
table, first column is key and second column is value. Keys will be sorted.

5) if result is an array of hashes of scalars (check at most 5 elements), print
as table.

6) if result is an array of arrays of scalars (check at most 5 elements), print
as table.

7) otherwise print as JSON (after cleaning it with L<Data::Clean::JSON>).

YAML and the other formats are not supported.

Table is printed using the more lightweight and much faster
L<Text::Table::Tiny>.

=item * No support for some protocols

Instead of L<Perinci::Access>, this module uses the more lightweight alternative
L<Perinci::Access::Lite> which does not support some URL schemes. http/https and
local are supported though.

=item * No automatic validation from schema in P::C::Lite

Since code wrapping and schema code generation done by L<Perinci::Sub::Wrapper>
and L<Data::Sah> (which are called automatically by Perinci::Access, but not by
Perinci::Access::Lite) adds too much startup overhead.

=item * P::C::Lite does not support color themes

=item * P::C::Lite does not support undo

=item * P::C::Lite currently has simpler logging

Only logging to screen is supported, using
L<Log::Any::Adapter::ScreenColoredLevel>.

=item * P::C::Lite does not support I18N

=item * P::C::Lite does not yet support these environment variables

 PERINCI_CMDLINE_COLOR_THEME
 PERINCI_CMDLINE_SERVER
 COLOR
 UTF8

=item * In passing command-line object to functions, P::C::Lite object is passed

Some functions might expect a L<Perinci::CmdLine> instance.

=back


=head1 REQUEST KEYS

All those supported by L<Perinci::CmdLine::Base>, plus:

=over

=item * naked_res => bool

Set to true if user specifies C<--naked-res>.

=back


=head1 ATTRIBUTES

All the attributes of L<Perinci::CmdLine::Base>, plus:

=head2 log => bool (default: 0, or from env)

Whether to enable logging. This currently means setting up L<Log::Any::Adapter>
to display logging (set in C<hook_after_parse_argv>, so tab completion skips
this step). To produce log, you use L<Log::Any> in your code.

The default is off. If you set LOG=1 or LOG_LEVEL or TRACE/DEBUG/VERBOSE/QUIET,
then the default will be on. It defaults to off if you set LOG=0 or
LOG_LEVEL=off.

=head2 log_level => str (default: warning, or from env)

Set default log level. The default can also be set via
LOG_LEVEL/TRACE/DEBUG/VERBOSE/QUIET.


=head1 METHODS

All the methods of L<Perinci::CmdLine::Base>, plus:


=head1 ENVIRONMENT

All the environment variables that L<Perinci::CmdLine::Base> supports, plus:

=head2 DEBUG

Set log level to 'debug'.

=head2 VERBOSE

Set log level to 'info'.

=head2 QUIET

Set log level to 'error'.

=head2 TRACE

Set log level to 'trace'.

=head2 LOG_LEVEL

Set log level.

=head2 PROGRESS => BOOL

Explicitly turn the progress bar on/off.


=head1 RESULT METADATA

All those supported by L<Perinci::CmdLine::Base>, plus:

=head2 x.hint.result_binary => bool

If set to true, then when formatting to C<text> formats, this class won't print
any newline to keep the data being printed unmodified.


=head1 SEE ALSO

L<Perinci::CmdLine>, L<Perinci::CmdLine::Manual>

L<Perinci::CmdLine::Any>

=cut
