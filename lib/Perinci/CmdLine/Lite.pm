package Perinci::CmdLine::Lite;

# put pragmas + Log::ger here
use 5.010001;
# use strict; # already enabled by Mo
# use warnings; # already enabled by Mo
use Log::ger;

# put other modules alphabetically here
use IO::Interactive qw(is_interactive);
use List::Util qw(first);
use Moo;

#use Moo;
extends 'Perinci::CmdLine::Base';

# put global variables alphabetically here
# AUTHORITY
# DATE
# DIST
# VERSION

# when debugging, use this instead of the above because Mo doesn't give clear
# error message if base class has errors.
#use parent 'Perinci::CmdLine::Base';

has default_prompt_template => (
    is=>'rw',
    default => 'Enter %s: ',
);
has validate_args => (
    is=>'rw',
    default => 1,
);

my $formats = [qw/text text-simple text-pretty json json-pretty csv termtable html html+datatables perl/];

sub BUILD {
    my ($self, $args) = @_;

    if (!$self->{actions}) {
        $self->{actions} = {
            call => {},
            version => {},
            subcommands => {},
            help => {},
        };
    }

    my $_copy = sub {
        no warnings;
        my $co_name = shift;
        my $href = $Perinci::CmdLine::Base::copts{$co_name};
        %$href;
    };

    if (!$self->{common_opts}) {
        my $copts = {};

        $copts->{version}   = { $_copy->('version'), };
        $copts->{help}      = { $_copy->('help'), };

        unless ($self->skip_format) {
            $copts->{format}    = {
                $_copy->('format'),
                schema => ['str*' => in => $formats],
            };
            $copts->{json}        = { $_copy->('json'), };
            $copts->{naked_res}   = { $_copy->('naked_res'), };
            $copts->{page_result} = { $_copy->('page_result'), };
            $copts->{view_result} = { $_copy->('view_result'), };
        }
        if ($self->subcommands) {
            $copts->{subcommands} = { $_copy->('subcommands'), };
        }
        if ($self->default_subcommand) {
            $copts->{cmd} = { $_copy->('cmd') };
        }
        if ($self->read_config) {
            $copts->{config_path}    = { $_copy->('config_path') };
            $copts->{no_config}      = { $_copy->('no_config') };
            $copts->{config_profile} = { $_copy->('config_profile') };
        }
        if ($self->read_env) {
            $copts->{no_env} = { $_copy->('no_env') };
        }
        if ($self->log) {
            $copts->{log_level} = { $_copy->('log_level'), };
            $copts->{trace}     = { $_copy->('trace'), };
            $copts->{debug}     = { $_copy->('debug'), };
            $copts->{verbose}   = { $_copy->('verbose'), };
            $copts->{quiet}     = { $_copy->('quiet'), };
        }
        $self->{common_opts} = $copts;
    }

    $self->{formats} //= $formats;

    $self->{per_arg_json} //= 1;
}

my $setup_progress;
sub _setup_progress_output {
    my $self = shift;

    return unless $ENV{PROGRESS} // is_interactive(*STDOUT);

    require Progress::Any::Output;
    Progress::Any::Output->set("TermProgressBarColor");
    $setup_progress = 1;
}

sub _unsetup_progress_output {
    my $self = shift;

    return unless $setup_progress;
    no warnings 'once';
    my $out = $Progress::Any::outputs{''}[0];
    $out->cleanup if $out->can("cleanup");
    $setup_progress = 0;
}

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
}

sub hook_before_parse_argv {
    my ($self, $r) = @_;

    # in this hook, we want to add several shortcut options (e.g. -C for
    # --no-config, etc) if the function is not using those shortcut options. but
    # to do this, we need to get meta first and this is only possible when there
    # is no subcommand
    return if $r->{subcommands};
    $self->get_meta($r, $self->{url});

    my $copts = $self->common_opts;

    # XXX cache
    require Perinci::Sub::GetArgs::Argv;
    my $ggls_res = Perinci::Sub::GetArgs::Argv::gen_getopt_long_spec_from_meta(
        meta               => $r->{meta},
        meta_is_normalized => 1,
        args               => $r->{args},
        common_opts        => $copts,
        per_arg_json       => $self->{per_arg_json},
        per_arg_yaml       => $self->{per_arg_yaml},
    );

    my $meta_uses_opt_P = 0;
    my $meta_uses_opt_c = 0;
    my $meta_uses_opt_C = 0;
    {
        last unless $ggls_res->[0] == 200;
        my $opts = $ggls_res->[3]{'func.opts'};
        if (grep { $_ eq '-P' } @$opts) { $meta_uses_opt_P = 1 }
        if (grep { $_ eq '-c' } @$opts) { $meta_uses_opt_c = 1 }
        if (grep { $_ eq '-C' } @$opts) { $meta_uses_opt_C = 1 }
    }

    #say "D:meta_uses_opt_P=<$meta_uses_opt_P>";
    #say "D:meta_uses_opt_c=<$meta_uses_opt_c>";
    #say "D:meta_uses_opt_C=<$meta_uses_opt_C>";

    # add -P shortcut for --config-profile if no conflict
    if ($copts->{config_profile} && !$meta_uses_opt_P) {
        $copts->{config_profile}{getopt} = 'config-profile|P=s';
    }

    # add -c shortcut for --config-path if no conflict
    if ($copts->{config_path} && !$meta_uses_opt_c) {
        $copts->{config_path}{getopt} = 'config-path|c=s';
    }

    # add -P shortcut for --no-config if no conflict
    if ($copts->{no_config} && !$meta_uses_opt_C) {
        $copts->{no_config}{getopt} = 'no-config|C';
    }
}

sub hook_before_action {

    my ($self, $r) = @_;

  VALIDATE_ARGS: {
        last unless $self->validate_args;

        # unless we're feeding the arguments to function, don't bother
        # validating arguments
        last unless $r->{action} eq 'call';

        my $meta = $r->{meta};

        # function says it's already already wrapped and the wrapper adds
        # validator.
        last if $meta->{'x.perinci.sub.wrapper.logs'} &&
            (grep { $_->{validate_args} }
             @{ $meta->{'x.perinci.sub.wrapper.logs'} });

        # function can validate its args, so we don't have to do validation for
        # it
        last if $meta->{features} && $meta->{features}{validate_vars};

        $self->_plugin_run_event(
            name => 'validate_args',
            r => $r,
            on_success => sub {
                no strict 'refs'; ## no critic: TestingAndDebugging::ProhibitNoStrict

                # to be cheap, we simply use "$ref" as key as cache key. to be
                # proper, it should be hash of serialized content.
                my %validators_by_arg; # key = argname

              USE_VALIDATORS_FROM_SCHEMA_V: {
                    my $url = $r->{subcommand_data}{url};
                    $url =~ m!\A/(.+)/(\w+)\z! or last;
                    my $func = $2;
                    (my $mod = $1) =~ s!/!::!g;
                    my $schemav_mod = "Sah::SchemaV::$mod";
                    (my $schemav_mod_pm = "$schemav_mod.pm") =~ s!::!/!g;
                    eval { require $schemav_mod_pm };
                    last if $@;

                    #say "D:we have pre-compiled validator codes";

                    for my $arg (sort keys %{ $meta->{args} // {} }) {
                        ### para1, sync with all others
                        my $argspec = $meta->{args}{$arg};
                        my $argschema = $argspec->{schema};
                        next unless $argschema;
                        my $schema_has_default = defined $argschema->[1]{default} ||
                            $argschema->[1]{'x.perl.default_value_rules'} && @{ $argschema->[1]{'x.perl.default_value_rules'} };
                        next unless exists($r->{args}{$arg}) || $schema_has_default;

                        # we don't support validation of input stream because
                        # this must be done after each 'get item' (but periswrap
                        # does)
                        next if $meta->{args}{$arg}{stream};

                        my $v = ${"$schemav_mod\::Args_Validators"}{$func}{$arg}
                            or next;
                        #say "D:using precompiled validator for arg $arg";
                        $validators_by_arg{$arg} = $v;
                    }
                }

              GEN_VALIDATORS: {
                    my %validators_by_schema; # key = "$schema"
                    require Data::Sah;
                    for my $arg (sort keys %{ $meta->{args} // {} }) {
                        ### para1, sync with all others
                        my $argspec = $meta->{args}{$arg};
                        my $argschema = $argspec->{schema};
                        next unless $argschema;
                        my $schema_has_default = defined $argschema->[1]{default} ||
                            $argschema->[1]{'x.perl.default_value_rules'} && @{ $argschema->[1]{'x.perl.default_value_rules'} };
                        next unless exists($r->{args}{$arg}) || $schema_has_default;

                        # we don't support validation of input stream because
                        # this must be done after each 'get item' (but periswrap
                        # does)
                        next if $meta->{args}{$arg}{stream};

                        unless ($validators_by_schema{"$argschema"}) {
                            my $v = Data::Sah::gen_validator($argschema, {
                                return_type => 'str+val',
                                schema_is_normalized => 1,
                            });
                            $validators_by_schema{"$argschema"} = $v;
                            $validators_by_arg{$arg} = $v;
                        }
                    }
                }

              DO_VALIDATE: {
                    for my $arg (sort keys %{ $meta->{args} // {} }) {
                        my $v = $validators_by_arg{$arg} or next;

                        my $argspec = $meta->{args}{$arg};
                        my $argschema = $argspec->{schema};
                        my $schema_has_default = defined $argschema->[1]{default} ||
                            $argschema->[1]{'x.perl.default_value_rules'} && @{ $argschema->[1]{'x.perl.default_value_rules'} };

                        # we want to get default value from schema, but do not
                        # want to make unspecified args spring into existence
                        # with 'undef' values. so we record the existence first
                        # here.
                        my $arg_exists = $r->{args}{"-set_$arg"} =
                            exists($r->{args}{$arg}) ? 1:0;

                        if ($schema_has_default) {
                            my $res = $v->(undef);
                            $r->{args}{"-default_$arg"} = $res->[1];
                        }

                        if ($arg_exists || $schema_has_default) {
                            my $res = $v->($r->{args}{$arg});
                            if ($res->[0]) {
                                die [400, "Argument '$arg' fails validation: $res->[0]"];
                            }
                            my $val0 = $r->{args}{$arg};
                            my $coerced_val = $res->[1];
                            $r->{args}{$arg} = $coerced_val;
                            $r->{args}{"-orig_$arg"} = $val0;
                        }
                    }
                } # DO_VALIDATE

              DO_VALIDATE_ARGS_RELS: {
                    last unless $meta->{args_rels};

                    # we haven't precompiled validator for args_rels yet
                    require Data::Sah;

                    my $schema = [hash => $meta->{args_rels}];
                    my $sah = Data::Sah->new;
                    my $hc  = $sah->get_compiler("human");
                    my $cd  = $hc->init_cd;
                    $cd->{args}{lang} //= $cd->{default_lang};
                    my $v = Data::Sah::gen_validator($schema, {
                        return_type => 'str',
                        human_hash_values => {
                            field  => $hc->_xlt($cd, "argument"),
                            fields => $hc->_xlt($cd, "arguments"),
                        },
                    });
                    my $res = $v->($r->{args});
                    if ($res) {
                        die [400, $res];
                    }
                } # DO_VALIDATE_ARGS_RELS

            },
        );
    } # VALIDATE_ARGS
}

sub hook_format_result {
}

sub hook_format_row {
    my ($self, $r, $row) = @_;

    if (ref($row) eq 'ARRAY') {
        return join("\t", @$row) . "\n";
    } else {
        return ($row // "") . "\n";
    }
}

sub hook_after_get_meta {
    my ($self, $r) = @_;

    my $copts = $self->common_opts;

    # note: we cannot cache this to $r->{_ggls_res} because we produce this
    # without dry-run
    require Perinci::Sub::GetArgs::Argv;
    my $ggls_res = Perinci::Sub::GetArgs::Argv::gen_getopt_long_spec_from_meta(
        meta               => $r->{meta},
        meta_is_normalized => 1,
        args               => $r->{args},
        common_opts        => $copts,
        per_arg_json       => $self->{per_arg_json},
        per_arg_yaml       => $self->{per_arg_yaml},
    );

    my $meta_uses_opt_n = 0;
    my $meta_uses_opt_N = 0;
    {
        last unless $ggls_res->[0] == 200;
        my $opts = $ggls_res->[3]{'func.opts'};
        if (grep { $_ eq '-n' } @$opts) { $meta_uses_opt_n = 1 }
        if (grep { $_ eq '-N' } @$opts) { $meta_uses_opt_N = 1 }
    }

    require Perinci::Object;
    my $metao = Perinci::Object::risub($r->{meta});

    # delete --format, --json, --naked-res if function does not want its output
    # to be formatted
    {
        last if $self->skip_format; # already doesn't have those copts
        last unless $r->{meta}{'cmdline.skip_format'};
        delete $copts->{format};
        delete $copts->{json};
        delete $copts->{naked_res};
    }

    # add --dry-run (and -n shortcut, if no conflict)
    {
        last if $copts->{dry_run} || $copts->{no_dry_run}; # sometimes we are run more than once?
        last unless $metao->can_dry_run;
        my $default_dry_run = $metao->default_dry_run // $self->default_dry_run;
        $r->{dry_run} = 1 if $default_dry_run;
        $r->{dry_run} = ($ENV{DRY_RUN} ? 1:0) if defined $ENV{DRY_RUN};

        if ($default_dry_run) {
            my $optname = 'no-dry-run' . ($meta_uses_opt_N ? '' : '|N');
            $copts->{no_dry_run} = {
                getopt  => $optname,
                summary => "Disable simulation mode (also via DRY_RUN=0)",
                handler => sub {
                    my ($go, $val, $r) = @_;
                    log_debug("[pericmd] Dry-run mode is deactivated");
                    $r->{dry_run} = 0;
                },
            };
        } else {
            my $optname = 'dry-run' . ($meta_uses_opt_n ? '' : '|n');
            $copts->{dry_run} = {
                getopt  => $optname,
                summary => "Run in simulation mode (also via DRY_RUN=1)",
                handler => sub {
                    my ($go, $val, $r) = @_;
                    log_debug("[pericmd] Dry-run mode is activated");
                    $r->{dry_run} = 1;
                }
            };
        }
    }

    # check deps property. XXX this should be done only when we don't wrap
    # subroutine, because Perinci::Sub::Wrapper already checks the deps
    # property.
    if ($r->{meta}{deps} && !$r->{in_dump_object} && !$r->{in_completion}) {
        require Perinci::Sub::DepChecker;
        my $res = Perinci::Sub::DepChecker::check_deps($r->{meta}{deps});
        if ($res) {
            die [412, "Dependency failed: $res"];
        }
    }
}


sub action_version {
    no strict 'refs'; ## no critic: TestingAndDebugging::ProhibitNoStrict

    my ($self, $r) = @_;

    my @text;

    {
        my $meta = $r->{meta} = $self->get_meta($r, $self->url);
        push @text, $self->get_program_and_subcommand_name($r),
            " version ", ($meta->{entity_v} // "?"),
            ($meta->{entity_date} ? " ($meta->{entity_date})" : ''),
            "\n";
        for my $mod (@{ $meta->{'x.dynamic_generator_modules'} // [] }) {
            push @text, "  $mod version ", ${"$mod\::VERSION"},
                (${"$mod\::DATE"} ? " (".${"$mod\::DATE"}.")" : ""),
                    "\n";
        }
    }

    for my $url (@{ $self->extra_urls_for_version // [] }) {
        my $meta = $self->get_meta($r, $url);
        push @text, "  $url version ", ($meta->{entity_v} // "?"),
            ($meta->{entity_date} ? " ($meta->{entity_date})" : ''),
            "\n";
    }

    push @text, "  ", __PACKAGE__,
        " version ", ($Perinci::CmdLine::Lite::VERSION // "?"),
        ($Perinci::CmdLine::Lite::DATE ?
         " ($Perinci::CmdLine::Lite::DATE)":''),
        "\n";

    [200, "OK", join("", @text)];
}

<sub action_call {
    my ($self, $r) = @_;

    my %extra;
    if ($r->{send_argv}) {
        log_trace("[pericmd] Sending argv to server: %s", $extra{argv});
        $extra{argv} = $r->{orig_argv};
    } else {
        my %extra_args;
        $extra_args{-dry_run} = 1 if $r->{dry_run};
        $extra{args} = {%extra_args, %{$r->{args}}};
    }

    $extra{stream_arg} = 1 if $r->{stream_arg};

    my $url = $r->{subcommand_data}{url};

    # currently we don't log args because it's potentially large
    log_trace("[pericmd] Riap request: action=call, url=%s", $url);

    #log_trace("TMP: extra=%s", \%extra);

    # setup output progress indicator
    if ($r->{meta}{features}{progress}) {
        $self->_setup_progress_output;
    }

    $self->riap_client->request(
        call => $url, \%extra);
}

1;
# ABSTRACT: A Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(BUILD|get_meta|hook_.+|action_.+)$

=head1 SYNOPSIS

#RENDER_TEMPLATE: file=>"share/templates/synopsis.txt", context=>{module=>"Perinci::CmdLine::Lite"}


=head1 DESCRIPTION

#RENDER_TEMPLATE: file=>"share/templates/description.txt"

Perinci::CmdLine::Lite is the default backend implementation. Another
implementation is the heavier L<Perinci::CmdLine::Classic> which has a couple of
more features not yet incorporated into ::Lite, e.g. transactions.

You normally should use L<Perinci::CmdLine::Any> instead to be able to switch
backend on the fly.


=head1 REQUEST KEYS

All those supported by L<Perinci::CmdLine::Base>.

=head1 ATTRIBUTES

All the attributes of L<Perinci::CmdLine::Base>, plus:

=over

=item * validate_args => bool (default: 1)

Validate arguments using schema from metadata.

=back

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

=head2 PROGRESS => bool

Explicitly turn the progress bar on/off.

=head2 FORMAT_PRETTY_TABLE_COLUMN_ORDERS => array (json)

Set the default of C<table_column_orders> in C<format_options> in result
metadata, similar to what's implemented in L<Perinci::Result::Format> and
L<Data::Format::Pretty::Console>.


=head1 RESULT METADATA

All those supported by L<Perinci::CmdLine::Base>, plus:

=head2 x.hint.result_binary => bool

If set to true, then when formatting to C<text> formats, this class won't print
any newline to keep the data being printed unmodified.


=head1 SEE ALSO

L<Perinci::CmdLine::Any>

L<Perinci::CmdLine::Classic>

L<Perinci::CmdLine::Inline>

=cut
