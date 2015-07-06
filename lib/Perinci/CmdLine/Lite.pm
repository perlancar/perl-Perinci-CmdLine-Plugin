package Perinci::CmdLine::Lite;

# DATE
# VERSION

use 5.010001;
# use strict; # already enabled by Mo
# use warnings; # already enabled by Mo
use Log::Any::IfLOG '$log';

use List::Util qw(first);
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
has validate_args => (
    is=>'rw',
    default => 1,
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
        if ($self->read_env) {
            $copts->{no_env} = { $_t->('no_env') };
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

my $setup_progress;
sub _setup_progress_output {
    my $self = shift;

    return unless $ENV{PROGRESS} // (-t STDOUT);

    require Progress::Any::Output;
    Progress::Any::Output->set("TermProgressBarColor");
    $setup_progress = 1;
}

sub _unsetup_progress_output {
    my $self = shift;

    return unless $setup_progress;
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

    # set up log adapter
    if ($self->log) {
        require Log::Any::Adapter;
        Log::Any::Adapter->set(
            'Screen',
            min_level => $r->{log_level} // $self->log_level,
            formatter => sub { $self->program_name . ": $_[1]" },
        );
    }
}

sub hook_before_action {
    my ($self, $r) = @_;

    # validate arguments using schema from metadata
  VALIDATE_ARGS:
    {
        last unless $self->validate_args;

        # unless we're feeding the arguments to function, don't bother
        # validating arguments
        last unless $r->{action} eq 'call';

        my $meta = $r->{meta};

        # function is probably already wrapped
        last if $meta->{'x.perinci.sub.wrapper.logs'} &&
            (grep { $_->{validate_args} }
             @{ $meta->{'x.perinci.sub.wrapper.logs'} });

        require Data::Sah;

        # to be cheap, we simply use "$ref" as key as cache key. to be proper,
        # it should be hash of serialized content.
        my %validators; # key = "$schema"

        for my $arg (sort keys %{ $meta->{args} // {} }) {
            next unless exists($r->{args}{$arg});

            # we don't support validation of input stream because this must be
            # done after each 'get item' (but periswrap does)
            next if $meta->{args}{$arg}{stream};

            my $schema = $meta->{args}{$arg}{schema};
            next unless $schema;
            unless ($validators{"$schema"}) {
                my $v = Data::Sah::gen_validator($schema, {
                    return_type => 'str',
                    schema_is_normalized => 1,
                });
                $validators{"$schema"} = $v;
            }
            my $res = $validators{"$schema"}->($r->{args}{$arg});
            if ($res) {
                die [400, "Argument '$arg' fails validation: $res"];
            }
        }

        if ($meta->{args_rels}) {
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
        }

    }
}

sub __json {
    state $json = do {
        require JSON;
        JSON->new->canonical(1)->allow_nonref;
    };
    $json;
}

sub __gen_table {
    my ($data, $header_row, $resmeta, $is_pretty) = @_;

    $resmeta //= {};

    my @columns;
    if ($header_row) {
        @columns = @{$data->[0]};
    } else {
        @columns = map {"col$_"} 0..@{$data->[0]}-1;
    }

    my $column_orders; # e.g. [col2, col1, col3, ...]
  SET_COLUMN_ORDERS: {

        # find column orders from 'table_column_orders' in result metadata (or
        # from env)
        my $tcos;
        if ($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS}) {
            $tcos = __json->decode($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS});
        } elsif (my $rfos = ($resmeta->{'cmdline.format_options'} //
                                 $resmeta->{format_options})) {
            my $rfo = $rfos->{'text-pretty'} // $rfos->{text} // $rfos->{any};
            if ($rfo) {
                $tcos = $rfo->{table_column_orders};
            }
        }
        if ($tcos) {
            # find an entry in tcos that @columns contains all the columns of
          COLS:
            for my $cols (@$tcos) {
                for my $col (@$cols) {
                    next COLS unless first {$_ eq $col} @columns;
                }
                $column_orders = $cols;
                last SET_COLUMN_ORDERS;
            }
        }

        # find column orders from table spec
        $column_orders = $resmeta->{'table.fields'};
    }

    # reorder each row according to requested column order
    if ($column_orders) {
        require List::MoreUtils;

        # 0->2, 1->0, ... (map column position from unordered to ordered)
        my @map0 = sort {
            my $idx_a = List::MoreUtils::firstidx(sub {$_ eq $a->[1]},
                                                  @$column_orders) // 9999;
            my $idx_b = List::MoreUtils::firstidx(sub {$_ eq $b->[1]},
                                                  @$column_orders) // 9999;
            $idx_a <=> $idx_b || $a->[1] cmp $b->[1];
        } map {[$_, $columns[$_]]} 0..@columns-1;
        #use DD; dd \@map0;
        my @map;
        for (0..@map0-1) {
            $map[$_] = $map0[$_][0];
        }
        #use DD; dd \@map;
        my $newdata = [];
        for my $row (@$data) {
            my @newrow;
            for (0..@map-1) { $newrow[$_] = $row->[$map[$_]] }
            push @$newdata, \@newrow;
        }
        $data = $newdata;
    }

    if ($is_pretty) {
        require Text::Table::Tiny;
        Text::Table::Tiny::table(rows=>$data, header_row=>$header_row) . "\n";
    } else {
        no warnings 'uninitialized';
        shift @$data if $header_row;
        join("", map {join("\t", @$_)."\n"} @$data);
    }
}

sub hook_format_result {
    my ($self, $r) = @_;

    my $res    = $r->{res};
    my $format = $r->{format} // 'text';

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
                return __gen_table($data, 0, $res->[3], $is_pretty);
            } elsif (Data::Check::Structure::is_hos($data, {max=>$max})) {
                $data = [map {[$_, $data->{$_}]} sort keys %$data];
                unshift @$data, ["key", "value"];
                return __gen_table($data, 1, $res->[3], $is_pretty);
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
                unshift @$newdata, \@fieldnames;
                return __gen_table($newdata, 1, $res->[3], $is_pretty);
            } else {
                $format = 'json-pretty';
            }
        }
    }

    $res = $res->[2] if $r->{naked_res};

    warn "Unknown format '$format', fallback to json-pretty"
        unless $format =~ /\Ajson(-pretty)?\z/;
    $self->cleanser->clean_in_place($res);
    if ($format eq 'json') {
        return __json->encode($res) . "\n";
    } else {
        return __json->canonical(1)->pretty->encode($res);
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
                $log->debugf("[pericmd] Dry-run mode is activated");
                $r->{dry_run} = 1;
                #$ENV{VERBOSE} = 1;
            },
        };
    }

    # check deps property. XXX this should be done only when we don't wrap
    # subroutine, because Perinci::Sub::Wrapper already checks the deps
    # property.
    if ($r->{meta}{deps}) {
        require Perinci::Sub::DepChecker;
        my $res = Perinci::Sub::DepChecker::check_deps($r->{meta}{deps});
        if ($res) {
            die [412, "Dependency failed: $res"];
        }
    }
}

sub action_subcommands {
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

sub action_version {
    my ($self, $r) = @_;

    my @text;

    {
        my $meta = $r->{meta} = $self->get_meta($r, $self->url);
        push @text, $self->get_program_and_subcommand_name($r),
            " version ", ($meta->{entity_v} // "?"),
            ($meta->{entity_date} ? " ($meta->{entity_date})" : ''),
            "\n";
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

sub action_help {
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
        subcommands => $has_sc_no_sc ? $self->list_subcommands : undef,
        common_opts => $common_opts,
        per_arg_json => $self->per_arg_json,
        per_arg_yaml => $self->per_arg_yaml,
    );

    $res->[3]{"cmdline.skip_format"} = 1;
    $res;
}

sub action_call {
    my ($self, $r) = @_;

    my %extra;
    if ($r->{send_argv}) {
        $log->tracef("[pericmd] Sending argv to server: %s", $extra{argv});
        $extra{argv} = $r->{orig_argv};
    } else {
        my %extra_args;
        $extra_args{-dry_run} = 1 if $r->{dry_run};
        $extra{args} = {%extra_args, %{$r->{args}}};
    }

    $extra{stream_arg} = 1 if $r->{stream_arg};

    my $url = $r->{subcommand_data}{url};

    # currently we don't log args because it's potentially large
    $log->tracef("[pericmd] Riap request: action=call, url=%s", $url);

    #$log->tracef("TMP: extra=%s", \%extra);

    # setup output progress indicator
    if ($r->{meta}{features}{progress}) {
        $self->_setup_progress_output;
    }

    $self->riap_client->request(
        call => $url, \%extra);
}

1;
# ABSTRACT: A lightweight Rinci/Riap-based command-line application framework

=for Pod::Coverage ^(BUILD|get_meta|hook_.+|action_.+)$

=head1 SYNOPSIS

#RENDER_TEMPLATE: file=>"share/templates/synopsis.txt", context=>{module=>"Perinci::CmdLine::Lite"}


=head1 DESCRIPTION

#RENDER_TEMPLATE: file=>"share/templates/description.txt"

Perinci::CmdLine::Lite is the default backend implementation. You normally
should use L<Perinci::CmdLine::Any> instead to be able to switch backend on the
fly.


=head1 REQUEST KEYS

All those supported by L<Perinci::CmdLine::Base>, plus:

=over

=back


=head1 ATTRIBUTES

All the attributes of L<Perinci::CmdLine::Base>, plus:

=head2 log => bool (default: from env or 0)

Whether to enable logging. This currently means setting up L<Log::Any::Adapter>
to display logging (set in C<hook_after_parse_argv>, so tab completion skips
this step). To produce log, you use L<Log::Any> in your code.

The default is off. If you set LOG=1 or LOG_LEVEL or TRACE/DEBUG/VERBOSE/QUIET,
then the default will be on. It defaults to off if you set LOG=0 or
LOG_LEVEL=off.

=head2 log_level => str (default: from env, or 'warning')

Set default log level. The default can also be set via
LOG_LEVEL/TRACE/DEBUG/VERBOSE/QUIET.

=head2 validate_args => bool (default: 1)


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

L<Perinci::CmdLine::Any>, L<Perinci::CmdLine::Classic>

=cut
