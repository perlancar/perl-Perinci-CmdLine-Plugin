package Perinci::CmdLine::Base;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

# this class can actually be a role instead of base class for pericmd &
# pericmd-lite, but Mo is more lightweight than Role::Tiny (also R::T doesn't
# have attributes), Role::Basic, or Moo::Role.

BEGIN {
    if ($INC{'Perinci/CmdLine/Classic.pm'}) {
        require Moo; Moo->import;
    } else {
        require Mo; Mo->import(qw(build default));
    }
}

has actions => (is=>'rw');
has common_opts => (is=>'rw');
has completion => (is=>'rw');
has default_subcommand => (is=>'rw');
has get_subcommand_from_arg => (is=>'rw', default=>1);
has description => (is=>'rw');
has exit => (is=>'rw', default=>1);
has formats => (is=>'rw');
has pass_cmdline_object => (is=>'rw', default=>0);
has per_arg_json => (is=>'rw');
has per_arg_yaml => (is=>'rw');
has program_name => (
    is=>'rw',
    default => sub {
        my $pn = $ENV{PERINCI_CMDLINE_PROGRAM_NAME};
        if (!defined($pn)) {
            $pn = $0; $pn =~ s!.+/!!;
        }
        $pn;
    });
has riap_version => (is=>'rw', default=>1.1);
has riap_client => (is=>'rw');
has riap_client_args => (is=>'rw');
has subcommands => (is=>'rw');
has summary => (is=>'rw');
has tags => (is=>'rw');
has url => (is=>'rw');

has read_env => (is=>'rw', default=>1);
has env_name => (
    is => 'rw',
    default => sub {
        my $self = shift;
        __default_env_name($self->program_name);
    },
);

has read_config => (is=>'rw', default=>1);
has config_filename => (is=>'rw');
has config_dirs => (
    is=>'rw',
    default => sub {
        require Perinci::CmdLine::Util::Config;
        Perinci::CmdLine::Util::Config::get_default_config_dirs();
    },
);

has cleanser => (
    is => 'rw',
    lazy => 1,
    default => sub {
        require Data::Clean::JSON;
        Data::Clean::JSON->get_cleanser;
    },
);

# role: requires 'hook_after_get_meta'
# role: requires 'hook_format_row'
# role: requires 'default_prompt_template'

# role: requires 'hook_before_run'
# role: requires 'hook_before_read_config_file'
# role: requires 'hook_after_read_config_file'
# role: requires 'hook_after_parse_argv'
# role: requires 'hook_before_action'
# role: requires 'hook_after_action'
# role: requires 'hook_format_result'
# role: requires 'hook_display_result'
# role: requires 'hook_after_run'

# we put common stuffs here, but PC::Classic's final version will differ from
# PC::Lite's in several aspects: translation, supported output formats,
# PC::Classic currently adds some extra keys, some options are not added by
# PC::Lite (e.g. history/undo stuffs).
our %copts = (

    version => {
        getopt  => "version|v",
        summary => "Display program's version and exit",
        usage   => "--version (or -v)",
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{action} = 'version';
            $r->{skip_parse_subcommand_argv} = 1;
        },
    },

    help => {
        getopt  => 'help|h|?',
        summary => 'Display help message and exit',
        usage   => "--help (or -h, -?)",
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{action} = 'help';
            $r->{skip_parse_subcommand_argv} = 1;
        },
        order => 0, # high
    },

    format => {
        getopt  => 'format=s',
        summary => 'Choose output format, e.g. json, text',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{format} = $val;
        },
        default => undef,
        tags => ['category:output'],
        is_settable_via_config => 1,
    },
    json => {
        getopt  => 'json',
        summary => 'Set output format to json',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{format} = (-t STDOUT) ? 'json-pretty' : 'json';
        },
        tags => ['category:output'],
    },

    naked_res => {
        getopt  => 'naked-res!',
        summary => 'When outputing as JSON, strip result envelope',
        'summary.alt.bool.not' => 'When outputing as JSON, add result envelope',
        description => <<'_',

By default, when outputing as JSON, the full enveloped result is returned, e.g.:

    [200,"OK",[1,2,3],{"func.extra"=>4}]

The reason is so you can get the status (1st element), status message (2nd
element) as well as result metadata/extra result (4th element) instead of just
the result (3rd element). However, sometimes you want just the result, e.g. when
you want to pipe the result for more post-processing. In this case you can use
`--naked-res` so you just get:

    [1,2,3]

_
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{naked_res} = $val ? 1:0;
        },
        default => 0,
        tags => ['category:output'],
        is_settable_via_config => 1,
    },

    subcommands => {
        getopt  => 'subcommands',
        summary => 'List available subcommands',
        usage   => "--subcommands",
        show_in_usage => sub {
            my ($self, $r) = @_;
            !$r->{subcommand_name};
        },
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{action} = 'subcommands';
            $r->{skip_parse_subcommand_argv} = 1;
        },
    },

    # 'cmd=SUBCOMMAND_NAME' can be used to select other subcommands when
    # default_subcommand is in effect.
    cmd => {
        getopt  => "cmd=s",
        summary => 'Select subcommand',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{subcommand_name} = $val;
            $r->{subcommand_name_from} = '--cmd';
        },
        completion => sub {
            require Complete::Util;
            my %args = @_;
            my $cmdline = $args{cmdline};
            Complete::Util::complete_array_elem(
                array => [keys %{ $cmdline->list_subcommands }],
                word  => $args{word},
                ci    => 1,
            );
        },
    },

    config_path => {
        getopt  => 'config-path=s@',
        schema  => ['array*', of => 'str*'],
        'x.schema.element_entity' => 'filename',
        summary => 'Set path to configuration file',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{config_paths} //= [];
            push @{ $r->{config_paths} }, $val;
        },
        tags => ['category:configuration'],
    },
    no_config => {
        getopt  => 'no-config',
        summary => 'Do not use any configuration file',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{read_config} = 0;
        },
        tags => ['category:configuration'],
    },
    no_env => {
        getopt  => 'no-env',
        summary => 'Do not read environment for default options',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{read_env} = 0;
        },
        tags => ['category:environment'],
    },
    config_profile => {
        getopt  => 'config-profile=s',
        summary => 'Set configuration profile to use',
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{config_profile} = $val;
        },
        completion => sub {
            # return list of profiles in read config file

            my %args = @_;
            my $word    = $args{word} // '';
            my $cmdline = $args{cmdline};
            my $r       = $args{r};

            # we are not called from cmdline, bail (actually we might want to
            # return list of programs anyway, but we want to read the value of
            # bash_global_dir et al)
            return undef unless $cmdline;

            # since this is common option, at this point we haven't parsed
            # argument or even read config file. so we need to do that first.
            {
                # this is not activated yet
                $r->{read_config} = 1;

                my $res = $cmdline->parse_argv($r);
                #return undef unless $res->[0] == 200;
            }

            # we are not reading any config file, return empty list
            return [] unless $r->{config};

            my @profiles;
            for (keys %{$r->{config}}) {
                if (length $r->{subcommand_name}) {
                    push @profiles, $1
                        if /\A\Q$r->{subcommand_name}\E \s+ profile=(.+)/x;
                } else {
                    push @profiles, $1 if /\Aprofile=(.+)/;
                }
            }

            require Complete::Util;
            Complete::Util::complete_array_elem(
                array=>[sort @profiles], word=>$word, ci=>1);
        },
        tags => ['category:configuration'],
    },

    # since the cmdline opts is consumed, Log::Any::App doesn't see this. we
    # currently work around this via setting env.
    log_level => {
        getopt  => 'log-level=s',
        summary => 'Set log level',
        schema  => ['str*' => in => [
            qw/trace debug info warn warning error fatal/]],
        handler => sub {
            my ($go, $val, $r) = @_;
            $r->{log_level} = $val;
            $ENV{LOG_LEVEL} = $val;
        },
        is_settable_via_config => 1,
        tags => ['category:logging'],
    },
    trace => {
        getopt  => "trace",
        summary => "Set log level to trace",
        handler => sub {
            my ($go, $val, $r) = @_;
            $ENV{TRACE} = 1;
        },
        tags => ['category:logging'],
    },
    debug => {
        getopt  => "debug",
        summary => "Set log level to debug",
        handler => sub {
            my ($go, $val, $r) = @_;
            $ENV{DEBUG} = 1;
        },
        tags => ['category:logging'],
    },
    verbose => {
        getopt  => "verbose",
        summary => "Set log level to info",
        handler => sub {
            my ($go, $val, $r) = @_;
            $ENV{VERBOSE} = 1;
            $r->{_help_verbose} = 1;
        },
        tags => ['category:logging'],
    },
    quiet => {
        getopt  => "quiet",
        summary => "Set log level to quiet",
        handler => sub {
            my ($go, $val, $r) = @_;
            $ENV{QUIET} = 1;
        },
        tags => ['category:logging'],
    },

);

sub __default_env_name {
    my ($prog) = @_;

    for ($prog) {
        $_ //= "PROG"; # shouldn't happen
        $_ = uc($_);
        s/[^A-Z0-9]+/_/g;
    }
    "${prog}_OPT";
}

sub hook_before_run {}

sub hook_before_read_config_file {}

sub hook_after_read_config_file {}

sub hook_before_action {}

sub hook_after_action {}

sub get_meta {
    my ($self, $r, $url) = @_;

    my $res = $self->riap_client->request(meta => $url);
    die $res unless $res->[0] == 200;
    my $meta = $res->[2];
    $r->{meta} = $meta;
    $log->tracef("[pericmd] Running hook_after_get_meta ...");
    $self->hook_after_get_meta($r);
    $meta;
}

sub get_program_and_subcommand_name {
    my ($self, $r) = @_;
    my $res = ($self->program_name // "") . " " .
        ($r->{subcommand_name} // "");
    $res =~ s/\s+$//;
    $res;
}

sub get_subcommand_data {
    my ($self, $name) = @_;

    my $scs = $self->subcommands;
    return undef unless $scs;

    if (ref($scs) eq 'CODE') {
        return $scs->($self, name=>$name);
    } else {
        return $scs->{$name};
    }
}

sub list_subcommands {
    my ($self) = @_;
    return $self->{_cache_subcommands} if $self->{_cache_subcommands};

    my $scs = $self->subcommands;
    my $res;
    if ($scs) {
        if (ref($scs) eq 'CODE') {
            $scs = $scs->($self);
            die [500, "BUG: Subcommands code didn't return a hashref"]
                unless ref($scs) eq 'HASH';
        }
        $res = $scs;
    } else {
        $res = {};
    }
    $self->{_cache_subcommands} = $res;
    $res;
}

sub status2exitcode {
    my ($self, $status) = @_;
    return 0 if $status =~ /^2..|304/;
    $status - 300;
}

sub _detect_completion {
    my ($self, $r) = @_;

    if ($ENV{COMP_SHELL}) {
        $r->{shell} = $ENV{COMP_SHELL};
        return 1;
    } elsif ($ENV{COMP_LINE}) {
        $r->{shell} = 'bash';
        return 1;
    } elsif ($ENV{COMMAND_LINE}) {
        $r->{shell} = 'tcsh';
        return 1;
    }

    # assume default is bash
    $r->{shell} //= 'bash';

    0;
}

sub _read_env {
    my ($self, $r) = @_;

    return [] unless $self->read_env;
    my $env_name = $self->env_name;
    my $env = $ENV{$env_name};
    $log->tracef("[pericmd] Checking env %s: %s", $env_name, $env);
    return [] unless defined $env;

    # XXX is it "proper" to use Complete::* modules to parse cmdline, outside
    # the context of completion?

    my $words;
    if ($r->{shell} eq 'bash') {
        require Complete::Bash;
        ($words, undef) = @{ Complete::Bash::parse_cmdline($env, 0) };
    } elsif ($r->{shell} eq 'fish') {
        require Complete::Fish;
        ($words, undef) = @{ Complete::Fish::parse_cmdline($env) };
    } elsif ($r->{shell} eq 'tcsh') {
        require Complete::Tcsh;
        ($words, undef) = @{ Complete::Tcsh::parse_cmdline($env) };
    } elsif ($r->{shell} eq 'zsh') {
        require Complete::Zsh;
        ($words, undef) = @{ Complete::Zsh::parse_cmdline($env) };
    } else {
        die "Unsupported shell '$r->{shell}'";
    }
    $log->tracef("[pericmd] Words from env: %s", $words);
    $words;
}

sub do_completion {
    my ($self, $r) = @_;

    local $r->{in_completion} = 1;

    my ($words, $cword);
    if ($r->{shell} eq 'bash') {
        require Complete::Bash;
        ($words, $cword) = @{ Complete::Bash::parse_cmdline() };
    } elsif ($r->{shell} eq 'fish') {
        require Complete::Fish;
        ($words, $cword) = @{ Complete::Fish::parse_cmdline() };
    } elsif ($r->{shell} eq 'tcsh') {
        require Complete::Tcsh;
        ($words, $cword) = @{ Complete::Tcsh::parse_cmdline() };
    } elsif ($r->{shell} eq 'zsh') {
        require Complete::Zsh;
        ($words, $cword) = @{ Complete::Zsh::parse_cmdline() };
    } else {
        die "Unsupported shell '$r->{shell}'";
    }

    shift @$words; $cword--; # strip program name

    # @ARGV given by bash is messed up / different. during completion, we
    # get ARGV from parsing COMP_LINE/COMP_POINT.
    @ARGV = @$words;

    # check whether subcommand is defined. try to search from --cmd, first
    # command-line argument, or default_subcommand.
    $self->_parse_argv1($r);

    if ($r->{read_env}) {
        my $env_words = $self->_read_env($r);
        unshift @ARGV, @$env_words;
        $cword += @$env_words;
    }

    #$log->tracef("ARGV=%s", \@ARGV);
    #$log->tracef("words=%s", $words);

    # force format to text for completion, because user might type 'cmd --format
    # blah -^'.
    $r->{format} = 'text';

    my $scd = $r->{subcommand_data};
    my $meta = $self->get_meta($r, $scd->{url} // $self->{url});

    my $subcommand_name_from = $r->{subcommand_name_from} // '';

    require Perinci::Sub::Complete;
    my $compres = Perinci::Sub::Complete::complete_cli_arg(
        meta            => $meta, # must be normalized
        words           => $words,
        cword           => $cword,
        common_opts     => $self->common_opts,
        riap_server_url => $scd->{url},
        riap_uri        => undef,
        riap_client     => $self->riap_client,
        extras          => {r=>$r, cmdline=>$self},
        func_arg_starts_at => ($subcommand_name_from eq 'arg' ? 1:0),
        completion      => sub {
            my %args = @_;
            my $type = $args{type};

            # user specifies custom completion routine, so use that first
            if ($self->completion) {
                my $res = $self->completion(%args);
                return $res if $res;
            }
            # if subcommand name has not been supplied and we're at arg#0,
            # complete subcommand name
            if ($self->subcommands &&
                    $subcommand_name_from ne '--cmd' &&
                         $args{type} eq 'arg' && $args{argpos}==0) {
                require Complete::Util;
                return Complete::Util::complete_array_elem(
                    array => [keys %{ $self->list_subcommands }],
                    word  => $words->[$cword]);
            }

            # otherwise let periscomp do its thing
            return undef;
        },
    );

    my $formatted;
    if ($r->{shell} eq 'bash') {
        $formatted = Complete::Bash::format_completion(
            $compres, {word=>$words->[$cword]});
    } elsif ($r->{shell} eq 'fish') {
        $formatted = Complete::Fish::format_completion($compres);
    } elsif ($r->{shell} eq 'tcsh') {
        $formatted = Complete::Tcsh::format_completion($compres);
    } elsif ($r->{shell} eq 'zsh') {
        $formatted = Complete::Zsh::format_completion($compres);
    }

    [200, "OK", $formatted,
     # these extra result are for debugging
     {
         "func.words" => $words,
         "func.cword" => $cword,
         "cmdline.skip_format" => 1,
     }];
}

sub _read_config {
    require Perinci::CmdLine::Util::Config;

    my ($self, $r) = @_;

    $log->tracef("[pericmd] Finding config files ...");
    my $res = Perinci::CmdLine::Util::Config::read_config(
        config_paths    => $r->{config_paths},
        config_filename => $self->config_filename,
        config_dirs     => $self->config_dirs,
        program_name    => $self->program_name,
    );
    die $res unless $res->[0] == 200;
    $r->{config} = $res->[2];
    $r->{read_config_files} = $res->[3]{'func.read_files'};
    $log->tracef("[pericmd] Read config files: %s",
                 $r->{'read_config_files'});
}

sub _parse_argv1 {
    my ($self, $r) = @_;

    # parse common_opts which potentially sets subcommand
    {
        # one small downside for this is that we cannot do autoabbrev here,
        # because we're not yet specifying all options here.

        require Getopt::Long;
        my $old_go_conf = Getopt::Long::Configure(
            'pass_through', 'no_ignore_case', 'bundling', 'no_auto_abbrev');
        my @go_spec;
        my $co = $self->common_opts // {};
        for my $k (keys %$co) {
            push @go_spec, $co->{$k}{getopt} => sub {
                my ($go, $val) = @_;
                $co->{$k}{handler}->($go, $val, $r);
            };
        }
        #$log->tracef("\@ARGV before parsing common opts: %s", \@ARGV);
        Getopt::Long::GetOptions(@go_spec);
        Getopt::Long::Configure($old_go_conf);
        #$log->tracef("\@ARGV after  parsing common opts: %s", \@ARGV);
    }

    # select subcommand and fill subcommand data
    {
        my $scn = $r->{subcommand_name};
        my $scn_from = $r->{subcommand_name_from};
        if (!defined($scn) && defined($self->{default_subcommand})) {
            # get from default_subcommand
            if ($self->get_subcommand_from_arg == 1) {
                $scn = $self->{default_subcommand};
                $scn_from = 'default_subcommand';
            } elsif ($self->get_subcommand_from_arg == 2 && !@ARGV) {
                $scn = $self->{default_subcommand};
                $scn_from = 'default_subcommand';
            }
        }
        if (!defined($scn) && $self->{subcommands} && @ARGV) {
            # get from first command-line arg
            if ($ARGV[0] =~ /\A-/) {
                if ($r->{in_completion}) {
                    $scn = shift @ARGV;
                    $scn_from = 'arg';
                } else {
                    die [400, "Unknown option: $ARGV[0]"];
                }
            } else {
                $scn = shift @ARGV;
                $scn_from = 'arg';
            }
        }

        my $scd;
        if (defined $scn) {
            $scd = $self->get_subcommand_data($scn);
            unless ($r->{in_completion}) {
                die [500, "Unknown subcommand: $scn"] unless $scd;
            }
        } elsif (!$r->{action} && $self->{subcommands}) {
            # program has subcommands but user doesn't specify any subcommand,
            # or specific action. display help instead.
            $r->{action} = 'help';
            $r->{skip_parse_subcommand_argv} = 1;
        } else {
            $scn = '';
            $scd = {
                url => $self->url,
                summary => $self->summary,
                description => $self->description,
                pass_cmdline_object => $self->pass_cmdline_object,
                tags => $self->tags,
            };
        }
        $r->{subcommand_name} = $scn;
        $r->{subcommand_name_from} = $scn_from;
        $r->{subcommand_data} = $scd;
    }

    # also set dry-run on environment
    $r->{dry_run} = 1 if $ENV{DRY_RUN};

    $r->{_parse_argv1_done} = 1;
}

sub _parse_argv2 {
    require Perinci::CmdLine::Util::Config;

    my ($self, $r) = @_;

    my %args;

    if ($r->{read_env}) {
        my $env_words = $self->_read_env($r);
        unshift @ARGV, @$env_words;
    }

    # parse argv for per-subcommand command-line opts
    if ($r->{skip_parse_subcommand_argv}) {
        return [200, "OK (subcommand options parsing skipped)"];
    } else {
        my $scd = $r->{subcommand_data};
        my $meta = $self->get_meta($r, $scd->{url});

        # first fill in from subcommand specification
        if ($scd->{args}) {
            $args{$_} = $scd->{args}{$_} for keys %{ $scd->{args} };
        }

        # then read from configuration
        if ($r->{read_config}) {

            $log->tracef("[pericmd] Running hook_before_read_config_file ...");
            $self->hook_before_read_config_file($r);

            $self->_read_config($r);

            $log->tracef("[pericmd] Running hook_after_read_config_file ...");
            $self->hook_after_read_config_file($r);

            my $res = Perinci::CmdLine::Util::Config::get_args_from_config(
                r                  => $r,
                config             => $r->{config},
                args               => \%args,
                subcommand_name    => $r->{subcommand_name},
                config_profile     => $r->{config_profile},
                common_opts        => $self->common_opts,
                meta               => $meta,
                meta_is_normalized => 1,
            );
            die $res unless $res->[0] == 200;
            $log->tracef("[pericmd] args after reading config files: %s",
                         \%args);
            my $found = $res->[3]{'func.found'};
            if (defined($r->{config_profile}) && !$found &&
                    defined($r->{read_config_files}) &&
                        @{$r->{read_config_files}} &&
                            !$r->{ignore_missing_config_profile_section}) {
                return [412, "Profile '$r->{config_profile}' not found ".
                            "in configuration file"];
            }

        }

        # finally get from argv

        # since get_args_from_argv() doesn't pass $r, we need to wrap it
        my $copts = $self->common_opts;
        my %old_handlers;
        for (keys %$copts) {
            my $h = $copts->{$_}{handler};
            $copts->{$_}{handler} = sub {
                my ($go, $val) = @_;
                $h->($go, $val, $r);
            };
            $old_handlers{$_} = $h;
        }

        my $has_cmdline_src;
        for my $ak (keys %{$meta->{args} // {}}) {
            my $av = $meta->{args}{$ak};
            if ($av->{cmdline_src}) {
                $has_cmdline_src = 1;
                last;
            }
            # this will probably be eventually checked by the rinci function's
            # schema: stream arguments need to have cmdline_src set to
            # stdin_or_file, stdin_or_files, stdin, or file.
            if ($av->{stream}) {
                unless ($av->{cmdline_src} &&
                            $av->{cmdline_src} =~
                                /\A(stdin|file|stdin_or_files?)\z/) {
                    die "BUG: stream argument '$ak' needs to have cmdline_src ".
                        "set to stdin, file, stdin_or_file, or stdin_or_files";
                }
            }
        }

        require Perinci::Sub::GetArgs::Argv;
        my $ga_res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(
            argv                => \@ARGV,
            args                => \%args,
            meta                => $meta,
            meta_is_normalized  => 1,
            allow_extra_elems   => $has_cmdline_src ? 1:0,
            per_arg_json        => $self->{per_arg_json},
            per_arg_yaml        => $self->{per_arg_yaml},
            common_opts         => $copts,
            strict              => $r->{in_completion} ? 0:1,
            on_missing_required_args => sub {
                my %a = @_;

                my ($an, $aa, $as) = ($a{arg}, $a{args}, $a{spec});
                my $src = $as->{cmdline_src};
                if ($src && $as->{req}) {
                    # don't complain, we will fill argument from other source
                    return 1;
                } else {
                    # we have no other sources, so we complain about missing arg
                    return 0;
                }
            },
        );

        return $ga_res unless $ga_res->[0] == 200;

        require Perinci::Sub::CoerceArgs;
        my $coerce_res = Perinci::Sub::CoerceArgs::coerce_args(
            meta                => $meta,
            meta_is_normalized  => 1,
            args                => $ga_res->[2],
        );

        return $coerce_res unless $coerce_res->[0] == 200;

        # restore
        for (keys %$copts) {
            $copts->{$_}{handler} = $old_handlers{$_};
        }

        return $ga_res;
    }
}

sub parse_argv {
    my ($self, $r) = @_;

    $log->tracef("[pericmd] Parsing \@ARGV: %s", \@ARGV);

    # we parse argv twice. the first parse is with common_opts only so we're
    # able to catch --help, --version, etc early without having to know about
    # subcommands. two reasons for this: sometimes we need to get subcommand
    # name *from* cmdline opts (e.g. --cmd) and thus it's a chicken-and-egg
    # problem. second, it's faster because we don't have to load Riap client and
    # request the meta through it (especially in the case of remote URL).
    #
    # the second parse is after ge get subcommand name and the function
    # metadata. we can parse the remaining argv to get function arguments.
    #
    # note that when doing completion we're not using this algorithem and only
    # parse argv once. this is to make completion work across common- and
    # per-subcommand opts, e.g. --he<tab> resulting in --help (common opt) as
    # well as --height (function argument).

    $self->_parse_argv1($r) unless $r->{_parse_argv1_done};
    $self->_parse_argv2($r);
}

sub __gen_iter {
    require Data::Sah::Util::Type;

    my ($fh, $sch, $argname) = @_;
    my $type = Data::Sah::Util::Type::get_type($sch);

    if (Data::Sah::Util::Type::is_simple($sch)) {
        return sub {
            # XXX this will be configurable later. currently by default reading
            # binary is per-64k while reading string is line-by-line.
            local $/ = \(64*1024) if $type eq 'buf';

            state $eof;
            return undef if $eof;
            my $l = <$fh>;
            unless (defined $l) {
                $eof++; return undef;
            }
            $l;
        };
    } else {
        # expect JSON stream for non-simple types
        require JSON;
        state $json = JSON->new->allow_nonref;
        my $i = -1;
        return sub {
            state $eof;
            return undef if $eof;
            $i++;
            my $l = <$fh>;
            unless (defined $l) {
                $eof++; return undef;
            }
            eval { $l = $json->decode($l) };
            if ($@) {
                die "Invalid JSON in stream argument '$argname' record #$i: $@";
            }
            $l;
        };
    }
}

# parse cmdline_src argument spec properties for filling argument value from
# file and/or stdin. currently does not support argument submetadata.
sub parse_cmdline_src {
    my ($self, $r) = @_;

    my $action = $r->{action};
    my $meta   = $r->{meta};

    my $url = $r->{subcommand_data}{url} // $self->{url} // '';
    my $is_network = $url =~ m!^(https?|riap[^:]+):!;

    # handle cmdline_src
    if ($action eq 'call') {
        my $args_p = $meta->{args} // {};
        my $stdin_seen;
        for my $an (sort {
            my $csa  = $args_p->{$a}{cmdline_src};
            my $csb  = $args_p->{$b}{cmdline_src};
            my $posa = $args_p->{$a}{pos} // 9999;
            my $posb = $args_p->{$b}{pos} // 9999;

            # first, always put stdin_line before stdin / stdin_or_files
            (
                !$csa || !$csb ? 0 :
                    $csa eq 'stdin_line' && $csb eq 'stdin_line' ? 0 :
                    $csa eq 'stdin_line' && $csb =~ /^(stdin|stdin_or_files?)/ ? -1 :
                    $csb eq 'stdin_line' && $csa =~ /^(stdin|stdin_or_files?)/ ? 1 : 0
            )
            ||

            # then order by pos
            ($posa <=> $posb)

            ||
            # then by name
            ($a cmp $b)
        } keys %$args_p) {
            #$log->tracef("TMP: handle cmdline_src for arg=%s", $an);
            my $as = $args_p->{$an};
            my $src = $as->{cmdline_src};
            my $type = $as->{schema}[0]
                or die "BUG: No schema is defined for arg '$an'";
            # Riap::HTTP currently does not support streaming input
            my $do_stream = $as->{stream} && $url !~ /^https?:/;
            if ($src) {
                die [531,
                     "Invalid 'cmdline_src' value for argument '$an': $src"]
                    unless $src =~ /\A(stdin|file|stdin_or_files?|stdin_line)\z/;
                die [531,
                     "Sorry, argument '$an' is set cmdline_src=$src, but type ".
                         "is not str/buf/array, only those are supported now"]
                    unless $do_stream || $type =~ /\A(str|buf|array)\z/;
                if ($src =~ /\A(stdin|stdin_or_files?)\z/) {
                    die [531, "Only one argument can be specified ".
                             "cmdline_src stdin/stdin_or_file/stdin_or_files"]
                        if $stdin_seen++;
                }
                my $is_ary = $type eq 'array';
                if ($src eq 'stdin_line' && !exists($r->{args}{$an})) {
                    require Perinci::Object;
                    require Term::ReadKey;
                    my $prompt = Perinci::Object::rimeta($as)->langprop('cmdline_prompt') //
                        sprintf($self->default_prompt_template, $an);
                    print $prompt;
                    my $iactive = (-t STDOUT);
                    Term::ReadKey::ReadMode('noecho')
                          if $iactive && $as->{is_password};
                    chomp($r->{args}{$an} = <STDIN>);
                    do { print "\n"; Term::ReadKey::ReadMode(0) }
                        if $iactive && $as->{is_password};
                    $r->{args}{"-cmdline_src_$an"} = 'stdin_line';
                } elsif ($src eq 'stdin' || $src eq 'file' &&
                        ($r->{args}{$an}//"") eq '-') {
                    die [400, "Argument $an must be set to '-' which means ".
                             "from stdin"]
                        if defined($r->{args}{$an}) &&
                            $r->{args}{$an} ne '-';
                    #$log->trace("Getting argument '$an' value from stdin ...");
                    $r->{args}{$an} = $do_stream ?
                        __gen_iter(\*STDIN, $as->{schema}, $an) :
                            $is_ary ? [<STDIN>] :
                                do {local $/; ~~<STDIN>};
                    $r->{args}{"-cmdline_src_$an"} = 'stdin';
                } elsif ($src eq 'stdin_or_file' || $src eq 'stdin_or_files') {
                    # push back argument value to @ARGV so <> can work to slurp
                    # all the specified files
                    local @ARGV = @ARGV;
                    unshift @ARGV, $r->{args}{$an}
                        if defined $r->{args}{$an};

                    # with stdin_or_file, we only accept one file
                    splice @ARGV, 1
                        if @ARGV > 1 && $src eq 'stdin_or_file';

                    #$log->tracef("Getting argument '$an' value from ".
                    #                 "$src, \@ARGV=%s ...", \@ARGV);

                    # perl doesn't seem to check files, so we check it here
                    for (@ARGV) {
                        next if $_ eq '-';
                        die [500, "Can't read file '$_': $!"] if !(-r $_);
                    }

                    $r->{args}{"-cmdline_srcfilenames_$an"} = [@ARGV];
                    $r->{args}{$an} = $do_stream ?
                        __gen_iter(\*ARGV, $as->{schema}, $an) :
                            $is_ary ? [<>] :
                                do {local $/; ~~<>};
                    $r->{args}{"-cmdline_src_$an"} = $src;
                } elsif ($src eq 'file') {
                    unless (exists $r->{args}{$an}) {
                        if ($as->{req}) {
                            die [400,
                                 "Please specify filename for argument '$an'"];
                        } else {
                            next;
                        }
                    }
                    die [400, "Please specify filename for argument '$an'"]
                        unless defined $r->{args}{$an};
                    #$log->trace("Getting argument '$an' value from ".
                    #                "file ...");
                    my $fh;
                    my $fname = $r->{args}{$an};
                    unless (open $fh, "<", $fname) {
                        die [500, "Can't open file '$fname' for argument '$an'".
                                 ": $!"];
                    }
                    $r->{args}{$an} = $do_stream ?
                        __gen_iter($fh, $as->{schema}, $an) :
                            $is_ary ? [<$fh>] :
                                do { local $/; ~~<$fh> };
                    $r->{args}{"-cmdline_src_$an"} = 'file';
                    $r->{args}{"-cmdline_srcfilenames_$an"} = [$fname];
                }
            }

            # encode to base64 if binary and we want to cross network (because
            # it's usually JSON)
            if ($self->riap_version == 1.2 && $is_network &&
                    defined($r->{args}{$an}) && $args_p->{$an}{schema} &&
                        $args_p->{$an}{schema}[0] eq 'buf' &&
                            !$r->{args}{"$an:base64"}) {
                require MIME::Base64;
                $r->{args}{"$an:base64"} =
                    MIME::Base64::encode_base64($r->{args}{$an}, "");
                delete $r->{args}{$an};
            }
        } # for arg
    }
    #$log->tracef("args after cmdline_src is processed: %s", $r->{args});
}

# determine filehandle to output to (normally STDOUT, but we can also send to a
# pager
sub select_output_handle {
    my ($self, $r) = @_;

    my $resmeta = $r->{res}[3] // {};

    my $handle;
    if ($resmeta->{"cmdline.page_result"}) {
        require File::Which;
        my $pager = $resmeta->{"cmdline.pager"} //
            $ENV{PAGER};
        unless (defined $pager) {
            $pager = "less -FRSX" if File::Which::which("less");
        }
        unless (defined $pager) {
            $pager = "more" if File::Which::which("more");
        }
        unless (defined $pager) {
            die [500, "Can't determine PAGER"];
        }
        last unless $pager; # ENV{PAGER} can be set 0/'' to disable paging
        #$log->tracef("Paging output using %s", $pager);
        open $handle, "| $pager";
    }
    $handle //= \*STDOUT;
    $r->{output_handle} = $handle;
}

sub display_result {
    require Data::Sah::Util::Type;

    my ($self, $r) = @_;

    my $meta = $r->{meta};
    my $res = $r->{res};
    my $fres = $r->{fres};
    my $resmeta = $res->[3] // {};

    my $handle = $r->{output_handle};

    my $sch = $meta->{result}{schema};
    my $type = Data::Sah::Util::Type::get_type($sch) // '';

    if ($resmeta->{stream} // $meta->{result}{stream}) {
        my $x = $res->[2];
        if (ref($x) eq 'CODE') {
            if (Data::Sah::Util::Type::is_simple($sch)) {
                while (defined(my $l = $x->())) {
                    print $l;
                    print "\n" unless $type eq 'buf';
                }
            } else {
                require JSON;
                state $json = JSON->new->allow_nonref;
                while (defined(my $rec = $x->())) {
                    print $json->encode($rec), "\n";
                }
            }
        } else {
            die [500, "Invalid stream in result (not a coderef)"];
        }
    } else {
        print $handle $fres;
    }
}

sub run {
    my ($self) = @_;
    $log->tracef("[pericmd] -> run(), \@ARGV=%s", \@ARGV);

    my $co = $self->common_opts;

    my $r = {
        orig_argv   => [@ARGV],
        common_opts => $co,
    };

    # completion is special case, we delegate to do_completion()
    if ($self->_detect_completion($r)) {
        $r->{res} = $self->do_completion($r);
        goto FORMAT;
    }

    # set default from common options
    $r->{naked_res} = $co->{naked_res}{default} if $co->{naked_res};
    $r->{format}    = $co->{format}{default} if $co->{format};

    if ($self->read_config) {
        # note that we will be reading config file
        $r->{read_config} = 1;
    }

    if ($self->read_env) {
        # note that we will be reading env for default options
        $r->{read_env} = 1;
    }

    eval {
        $log->tracef("[pericmd] Running hook_before_run ...");
        $self->hook_before_run($r);

        my $parse_res = $self->parse_argv($r);
        if ($parse_res->[0] == 501) {
            # we'll need to send ARGV to the server, because it's impossible to
            # get args from ARGV (e.g. there's a cmdline_alias with CODE, which
            # has been transformed into string when crossing network boundary)
            $r->{send_argv} = 1;
        } elsif ($parse_res->[0] != 200) {
            die $parse_res;
        }
        $r->{parse_argv_res} = $parse_res;
        $r->{args} = $parse_res->[2] // {};

        # set defaults
        $r->{action} //= 'call';

        $log->tracef("[pericmd] Running hook_after_parse_argv ...");
        $self->hook_after_parse_argv($r);

        $self->parse_cmdline_src($r);

        #$log->tracef("TMP: parse_res: %s", $parse_res);

        my $missing = $parse_res->[3]{"func.missing_args"};
        die [400, "Missing required argument(s): ".join(", ", @$missing)]
            if $missing && @$missing;

        my $scd = $r->{subcommand_data};
        if ($scd->{pass_cmdline_object} // $self->pass_cmdline_object) {
            $r->{args}{-cmdline} = $self;
            $r->{args}{-cmdline_r} = $r;
        }

        $log->tracef("[pericmd] Running hook_before_action ...");
        $self->hook_before_action($r);

        my $meth = "action_$r->{action}";
        die [500, "Unknown action $r->{action}"] unless $self->can($meth);
        $log->tracef("[pericmd] Running %s() ...", $meth);
        $r->{res} = $self->$meth($r);
        #$log->tracef("[pericmd] res=%s", $r->{res}); #1

        $log->tracef("[pericmd] Running hook_after_action ...");
        $self->hook_after_action($r);
    };
    my $err = $@;
    if ($err || !$r->{res}) {
        if ($err) {
            $err = [500, "Died: $err"] unless ref($err) eq 'ARRAY';
            if (%Devel::Confess::) {
                require Scalar::Util;
                my $id = Scalar::Util::refaddr($err);
                my $stack_trace = $Devel::Confess::MESSAGES{$id};
                $err->[1] .= "\n$stack_trace" if $stack_trace;
            }
            $err->[1] =~ s/\n+$//;
            $r->{res} = $err;
        } else {
            $r->{res} = [500, "Bug: no response produced"];
        }
    } elsif (ref($r->{res}) ne 'ARRAY') {
        $log->tracef("[pericmd] res=%s", $r->{res}); #2
        $r->{res} = [500, "Bug in program: result not an array"];
    } elsif (!$r->{res}[0] || $r->{res}[0] < 200 || $r->{res}[0] > 555) {
        $log->tracef("[pericmd] res=%s", $r->{res}); #3
        $r->{res} = [500, "Bug in program: invalid result status, ".
                         "must be 200 <= x <= 555"];
    }
    $r->{format} //= $r->{res}[3]{'cmdline.default_format'};
    $r->{format} //= $r->{meta}{'cmdline.default_format'};
    my $restore_orig_result;
    my $orig_result;
    if (exists $r->{res}[3]{'cmdline.result'}) {
        # temporarily change the result for formatting
        $restore_orig_result = 1;
        $orig_result = $r->{res}[2];
        $r->{res}[2] = $r->{res}[3]{'cmdline.result'};
    }
  FORMAT:
    if ($r->{res}[3]{'cmdline.skip_format'}) {
        $r->{fres} = $r->{res}[2];
    } elsif ($r->{res}[3]{stream} // $r->{meta}{result}{stream}) {
        # stream will be formatted as displayed by display_result()
    } else {
        $log->tracef("[pericmd] Running hook_format_result ...");
        $r->{fres} = $self->hook_format_result($r) // '';
    }
    $self->select_output_handle($r);
    $log->tracef("[pericmd] Running hook_display_result ...");
    $self->hook_display_result($r);
    $log->tracef("[pericmd] Running hook_after_run ...");
    $self->hook_after_run($r);

    if ($restore_orig_result) {
        $r->{res}[2] = $orig_result;
    }

    my $exitcode;
    if ($r->{res}[3] && defined($r->{res}[3]{'cmdline.exit_code'})) {
        $exitcode = $r->{res}[3]{'cmdline.exit_code'};
    } else {
        $exitcode = $self->status2exitcode($r->{res}[0]);
    }
    if ($self->exit) {
        $log->tracef("[pericmd] exit(%s)", $exitcode);
        exit $exitcode;
    } else {
        # so this can be tested
        $log->tracef("[pericmd] <- run(), exitcode=%s", $exitcode);
        $r->{res}[3]{'x.perinci.cmdline.base.exit_code'} = $exitcode;
        return $r->{res};
    }
}

1;
# ABSTRACT: Base class for Perinci::CmdLine{::Classic,::Lite}

=for Pod::Coverage ^(.+)$

=head1 DESCRIPTION


=head1 PROGRAM FLOW (NORMAL)

If you execute C<run()>, this is what will happen, in order:

=over

=item * Detect if we are running under tab completion mode

This is done by checking the existence of special environment varibles like
C<COMP_LINE> (bash) or C<COMMAND_LINE> (tcsh). If yes, then jump to L</"PROGRAM
FLOW (TAB COMPLETION)">. Otherwise, continue.

=item * Run hook_before_run, if defined

This hook (and every other hook) will be passed a single argument C<$r>, a hash
that contains request data (see L</"REQUEST KEYS">).

Some ideas that you can do in this hook: XXX.

=item * Parse command-line arguments (@ARGV) and set C<action>

If C<read_env> attribute is set to true, and there is environment variable
defined to set default options (see documentation on C<read_env> and C<env_name>
attributes) then the environment variable is parsed and prepended first to the
command-line, so it can be parsed together. For example, if your program is
called C<foo> and environment variable C<FOO_OPT> is set to C<--opt1 --opt2
val>. When you execute:

 % foo --no-opt1 --trace 1 2

then C<@ARGV> will be set to C<< ('--opt1', '--opt2', 'val', '--no-opt1',
'--trace', 1, 2) >>. This way, command-line arguments can have a higher
precedence and override settings from the environment variable (in this example,
C<--opt1> is negated by C<--no-opt1>).

Currently, parsing is done in two steps. The first step is to extract subcommand
name. Because we want to allow e.g. C<cmd --verbose subcmd> in addition to C<cmd
subcmd> (that is, user is allowed to specify options before subcommand name) we
cannot simply get subcommand name from the first element of C<@ARGV> but must
parse command-line options. Also, we want to allow user specifying subcommand
name from option C<cmd --cmd subcmd> because we want to support the notion of
"default subcommand" (subcommand that takes effect if there is no subcommand
specified).

In the first step, since we do not know the subcommand yet, we only parse common
options and strip them. Unknown options at this time will be passed through.

If user specifies common option like C<--help> or C<--version>, then action will
be set to (respectively) C<help> and C<version> and the second step will be
skipped. Otherwise we continue the the second step and action by default is set
to C<call>.

At the end of the first step, we already know the subcommand name (of course, if
subcommand name is unknown, we exit with error) along with subcommand spec: its
URL, per-subcommand settings, and so on (see the C<subcommands> attribute). If
there are no subcommands, subcommand name is set to C<''> (empty string) and the
subcommand spec is filled from the attributes, e.g. C<url>, C<summary>, <tags>,
and so on.

We then perform a C<meta> Riap request to the URL to get the Rinci metadata.
After that, C<hook_after_get_meta> is run if provided. From the Rinci metadata
we get list of arguments (the C<args> property). From this, we generate a spec
of command-line options to feed to L<Getopt::Long>. There are some conversions
being done, e.g. an argument called C<foo_bar> will become command-line option
C<--foo-bar>. Command-line aliases from metadata are also added to the
C<Getopt::Long> spec.

It is also at this step that we read config file (if C<read_config> attribute is
true). We run C<hook_before_read_config_file> first. Some ideas to do in this
hook: setting default config profile.

We then pass the spec to C<Getopt::Long::GetOptions>, we get function arguments.

We then run C<hook_after_parse_argv>. Some ideas to do in this hook: XXX.

Function arguments that are still missing can be filled from STDIN or files, if
the metadata specifies C<cmdline_src> property (see L<Rinci::function> for more
details).

=item * Delegate to C<action_$action> method

Before running the C<action_$action> method, C<hook_before_action> is called
e.g. to allow changing/fixing action, last chance to check arguments, etc.

After we get the action from the previous step, we delegate to separate
C<action_$action> method (so there is C<action_version>, C<action_help>, and so
on; and also C<action_call>). These methods also receive C<$r> as their argument
and must return an enveloped result (see L<Rinci::function> for more details).

Result is put in C<< $r->{res} >>.

C<hook_after_action> is then called e.g. to preformat result.

=item * Run hook_format_result

Hook must set C<< $r->{fres} >> (formatted result).

If result has C<cmdline.skip_format> result metadata property, then this step is
skipped and C<< $r->{fres} >> is simply taken from C<< $r->{res}[2] >>.

=item * Run hook_display_result

This hook is used by XXX.

=item * Run hook_after_run, if defined

Some ideas to do in this hook: XXX.

=item * Exit (or return result)

If C<exit> attribute is true, will C<exit()> with the action's envelope result
status. If status is 200, exit code is 0. Otherwise exit code is status minus
300. So, a response C<< [501, "Not implemented"] >> will result in exit code of
201.

If C<exit> attribute is false, will simply return the action result (C<<
$r->{res} >>). And will also set exit code in C<<
$r->{res}[3]{'x.perinci.cmdline.base.exit_code'} >>.

=back


=head1 PROGRAM FLOW (TAB COMPLETION)

If program is detected running in tab completion mode, there is some differences
in the flow. First, C<@ARGV> is set from C<COMP_LINE> (or C<COMMAND_LINE>)
environment variable. Afterwards, completion is done by calling
L<Perinci::Sub::Complete>'s C<complete_cli_arg>.

The result is then output to STDOUT (resume from Run hook_format_result step in
the normal program flow).


=head1 REQUEST KEYS

The various values in the C<$r> hash/stash.

=over

=item * orig_argv => array

Original C<@ARGV> at the beginning of C<run()>.

=item * common_opts => hash

Value of C<common_opts> attribute, for convenience.

=item * action => str

Selected action to use. Usually set from the common options.

=item * format => str

Selected format to use. Usually set from the common option C<--format>.

=item * read_config => bool

This is set in run() to signify that we have tried to read config file (this is
set to true even though config file does not exist). This is never set to true
when C<read_config> attribute is set, which means that we never try to read any
config file.

=item * read_env => bool

This is set in run() to signify that we will try to read env for default
options. This settng can be turned off e.g. in common option C<no_env>. This is
never set to true when C<read_env> attribute is set to false, which means that
we never try to read environment.

=item * config => hash

This is set in the routine that reads config file, containing the config hash.
It might be an empty hash (if there is on config file to read), or a hash with
sections as keys and hashrefs as values (configuration for each section). The
data can be merged from several existing config files.

=item * read_config_files => array

This is set in the routine that reads config file, containing the list of config
files actually read, in order.

=item * config_paths => array of str

=item * config_profile => str

=item * parse_argv_res => array

Enveloped result of C<parse_argv()>.

=item * ignore_missing_config_profile_section => bool (default 1)

This is checked in the parse_argv(). To aid error checking, when a user
specifies a profile (e.g. via C<--config-profile FOO>) and config file exists
but the said profile section is not found in the config file, an error is
returned. This is to notify user that perhaps she mistypes the profile name.

But this checking can be turned off with this setting. This is sometimes used
when e.g. a subclass wants to pick a config profile automatically by setting C<<
$r->{config_profile} >> somewhere before reading config file, but do not want to
fail execution when config profile is not found. An example of code that does
this is L<Perinci::CmdLine::fatten>.

=item * subcommand_name => str

Also set by C<parse_argv()>. The subcommand name in effect, either set
explicitly by user using C<--cmd> or the first command-line argument, or set
implicitly with the C<default_subcommand> attribute. Undef if there is no
subcommand name in effect.

=item * subcommand_name_from => str

Also set by C<parse_argv()>. Tells how the C<subcommand_name> request key is
set. Value is either C<--cmd> (if set through C<--cmd> common option), C<arg>
(if set through first command-line argument), C<default_subcommand> (if set to
C<default_subcommand> attribute), or undef if there is no subcommand_name set.

=item * subcommand_data => hash

Also set by C<parse_argv()>. Subcommand data, including its URL, summary (if
exists), and so on. Note that if there is no subcommand, this will contain data
for the main command, i.e. URL will be set from C<url> attribute, summary from
C<summary> attribute, and so on. This is a convenient way to get what URL and
summary to use, and so on.

=item * skip_parse_subcommand_argv => bool

Checked by C<parse_argv()>. Can be set to 1, e.g. in common option handler for
C<--help> or C<--version> to skip parsing @ARGV for per-subcommand options.

=item * args => hash

Also taken from C<parse_argv()> result.

=item * meta => hash

Result of C<get_meta()>.

=item * dry_run => bool

Whether to pass C<-dry_run> special argument to function.

=item * res => array

Enveloped result of C<action_ACTION()>.

=item * fres => str

Result from C<hook_format_result()>.

=item * output_handle => handle

Set by select_output_handle() to choose output handle. Normally it's STDOUT but
can also be pipe to pager (if paging is turned on).

=item * naked_res => bool

Set to true if user specifies C<--naked-res>.

=back


=head1 CONFIGURATION

Configuration can be used to set function arguments as well as some common
options.

Configuration is currently in the L<IOD> (basically INI) format.

By default these paths are searched: C<$HOME/.config/$prog_name.conf>,
C<$HOME/$prog_name.conf>, C</etc/$prog_name.conf>. The location can be
customized from command-line option C<--config-path>.

All existing configuration files will be read in order, and the result merged if
more than one files exist.

Section names map to subcommand names. For application that does not have
subcommand, you can put parameters outside any section, e.g.:

 param=val
 otherparam=val
 ...

For application that has subcommands, put parameters inside section with the
same name as the subcommand name:

 [subcommand1]
 param=val
 ...

 [subcommand2]
 param2=val
 ...

Or you can also put some parameters outside the section which will be used for
all subcommands:

 commonarg=val

 [subcommand1]
 param1=val
 ...

 [subcommand2]
 param2=val
 ...

A configuration file can also have (multiple) profiles, to allow multiple
configuration to be stored in a single file. Section names can have
"profile=PROFILENAME" suffix to mark it as belonging to a certain profile.
Parameters in sections with matching "profile=PROFILENAME" will be read.
Parameters in sections without any profile names will still be read. Example:

 a=0
 b=0
 d=9

 [profile=p1]
 a=1
 b=2

 [profile=p2]
 a=10
 b=20

 [subcommand1 profile=p1]
 c=3

 [subcommand1 profile=p2]
 c=1

If you run:

 % cmd subcommand1

then your subcommand1 function will get: a=0, b=0, d=9.

 % cmd subcommand1 --config-profile p1

then your subcommand1 function will get: a=1, b=2, c=3, d=9. If you run:

 % cmd subcommand1 --config-profile p2

then your subcommand1 function will get: a=10, b=20, c=30, d=9.

Parameter names map to function argument names or common option. If a common
option name clashes with a function argument name, the function argument is
accessible using the C<NAME.arg> syntax. For example, C<log_level> is a common
option name. If your function also has a C<log_level> argument, to set this
function argument, you write:

 log_level.arg=blah


=head1 ATTRIBUTES

=head2 actions => array

Contains a list of known actions and their metadata. Keys should be action
names, values should be metadata. Metadata is a hash containing these keys:

=head2 common_opts => hash

A hash of common options, which are command-line options that are not associated
with any subcommand. Each option is itself a specification hash containing these
keys:

=over

=item * category (str)

Optional, for grouping options in help/usage message, defaults to C<Common
options>.

=item * getopt (str)

Required, for Getopt::Long specification.

=item * handler (code)

Required, for Getopt::Long specification. Note that the handler will receive
C<<($geopt, $val, $r)>> (an extra C<$r>).

=item * usage (str)

Optional, displayed in usage line in help/usage text.

=item * summary (str)

Optional, displayed in description of the option in help/usage text.

=item * show_in_usage (bool or code, default: 1)

A flag, can be set to 0 if we want to skip showing this option in usage in
--help, to save some space. The default is to show all, except --subcommand when
we are executing a subcommand (obviously).

=item * show_in_options (bool or code, default: 1)

A flag, can be set to 0 if we want to skip showing this option in options in
--help. The default is to 0 for --help and --version in compact help. Or
--subcommands, if we are executing a subcommand (obviously).

=item * order (int)

Optional, for ordering. Lower number means higher precedence, defaults to 1.

=back

A partial example from the default set by the framework:

 {
     help => {
         category        => 'Common options',
         getopt          => 'help|h|?',
         usage           => '--help (or -h, -?)',
         handler         => sub { ... },
         order           => 0,
         show_in_options => sub { $ENV{VERBOSE} },
     },
     format => {
         category    => 'Common options',
         getopt      => 'format=s',
         summary     => 'Choose output format, e.g. json, text',
         handler     => sub { ... },
     },
     undo => {
         category => 'Undo options',
         getopt   => 'undo',
         ...
     },
     ...
 }

The default contains: help (getopt C<help|h|?>), version (getopt C<version|v>),
action (getopt C<action>), format (getopt C<format=s>), format_options (getopt
C<format-options=s>), json). If there are more than one subcommands, this will
also be added: list (getopt C<list|l>). If dry-run is supported by function,
there will also be: dry_run (getopt C<dry-run>). If undo is turned on, there
will also be: undo (getopt C<undo>), redo (getopt C<redo>), history (getopt
C<history>), clear_history (getopt C<clear-history>).

Sometimes you do not want some options, e.g. to remove C<format> and
C<format_options>:

 delete $cmd->common_opts->{format};
 delete $cmd->common_opts->{format_options};
 $cmd->run;

Sometimes you want to rename some command-line options, e.g. to change version
to use capital C<-V> instead of C<-v>:

 $cmd->common_opts->{version}{getopt} = 'version|V';

Sometimes you want to add subcommands as common options instead. For example:

 $cmd->common_opts->{halt} = {
     category    => 'Server options',
     getopt      => 'halt',
     summary     => 'Halt the server',
     handler     => sub {
         my ($go, $val, $r) = @_;
         $r->{subcommand_name} = 'shutdown';
     },
 };

This will make:

 % cmd --halt

equivalent to executing the 'shutdown' subcommand:

 % cmd shutdown

=head2 completion => code

Will be passed to L<Perinci::Sub::Complete>'s C<complete_cli_arg()>. See its
documentation for more details.

=head2 default_subcommand => str

Set subcommand to this if user does not specify which to use (either via first
command-line argument or C<--cmd> option). See also: C<get_subcommand_from_arg>.

=head2 get_subcommand_from_arg => int (default: 1)

The default is 1, which is to get subcommand from the first command-line
argument except when there is C<default_subcommand> defined. Other valid values
are: 0 (not getting from first command-line argument), 2 (get from first
command-line argument even though there is C<default_subcommand> defined).

=head2 description => str

=head2 exit => bool (default: 1)

=head2 formats => array

Available output formats.

=head2 pass_cmdline_object => bool (default: 0)

Whether to pass special argument C<-cmdline> containing the cmdline object to
function. This can be overriden using the C<pass_cmdline_object> on a
per-subcommand basis.

In addition to C<-cmdline>, C<-cmdline_r> will also be passed, containing the
C<$r> per-request stash/hash (see L</"REQUEST KEYS">).

Passing the cmdline object can be useful, e.g. to call action_help(), to get the
settings of the Perinci::CmdLine, etc.

=head2 program_name => str

Default is from PERINCI_CMDLINE_PROGRAM_NAME environment or from $0.

=head2 riap_client => obj

Set to Riap client instance, should you want to create one yourself. Otherwise
will be set L<Perinci::Access> (in PC:Classic), or L<Perinci::Access::Lite> (in
PC:Lite).

=head2 riap_version => float (default: 1.1)

Specify L<Riap> protocol version to use. Will be passed to Riap client
constructor (unless you already provide a Riap client object, see
C<riap_client>).

=head2 riap_client_args => hash

Arguments to pass to Riap client constructor. Will be used unless you create
your own Riap client object (see C<riap_client>). One of the things this
attribute is used is to pass HTTP basic authentication to Riap client
(L<Perinci::Access::HTTP::Client>):

 riap_client_args => {handler_args => {user=>$USER, password=>$PASS}}

=head2 subcommands => hash | code

Should be a hash of subcommand specifications or a coderef.

Each subcommand specification is also a hash(ref) and should contain these keys:

=over

=item * C<url> (str, required)

Location of function (accessed via Riap).

=item * C<summary> (str, optional)

Will be retrieved from function metadata at C<url> if unset

=item * C<description> (str, optional)

Shown in verbose help message, if description from function metadata is unset.

=item * C<tags> (array of str, optional)

For grouping or categorizing subcommands, e.g. when displaying list of
subcommands.

=item * C<log_any_app> (bool, optional)

Whether to load Log::Any::App, default is true. For subcommands that need fast
startup you can try turning this off for said subcommands. See L</"LOGGING"> for
more details.

=item * C<use_utf8> (bool, optional)

Whether to issue L<< binmode(STDOUT, ":utf8") >>. See L</"LOGGING"> for more
details.

=item * C<undo> (bool, optional)

Can be set to 0 to disable transaction for this subcommand; this is only
relevant when C<undo> attribute is set to true.

=item * C<show_in_help> (bool, optional, default 1)

If you have lots of subcommands, and want to show only some of them in --help
message, set this to 0 for subcommands that you do not want to show.

=item * C<pass_cmdline_object> (bool, optional, default 0)

To override C<pass_cmdline_object> attribute on a per-subcommand basis.

=item * C<args> (hash, optional)

If specified, will send the arguments (as well as arguments specified via the
command-line). This can be useful for a function that serves more than one
subcommand, e.g.:

 subcommands => {
     sub1 => {
         summary => 'Subcommand one',
         url     => '/some/func',
         args    => {flag=>'one'},
     },
     sub2 => {
         summary => 'Subcommand two',
         url     => '/some/func',
         args    => {flag=>'two'},
     },
 }

In the example above, both subcommand C<sub1> and C<sub2> point to function at
C</some/func>. But the function can differentiate between the two via the
C<flag> argument being sent.

 % cmdprog sub1 --foo 1 --bar 2
 % cmdprog sub2 --foo 2

In the first invocation, function will receive arguments C<< {foo=>1, bar=>2,
flag=>'one'} >> and for the second: C<< {foo=>2, flag=>'two'} >>.

=back

Subcommands can also be a coderef, for dynamic list of subcommands. The coderef
will be called as a method with hash arguments. It can be called in two cases.
First, if called without argument C<name> (usually when doing --subcommands) it
must return a hashref of subcommand specifications. If called with argument
C<name> it must return subcommand specification for subcommand with the
requested name only.

=head2 summary => str

=head2 tags => array of str

=head2 url => str

Required if you only want to run one function. URL should point to a function
entity.

Alternatively you can provide multiple functions from which the user can select
using the first argument (see B<subcommands>).

=head2 read_env => bool (default: 1)

Whether to read environment variable for default options.

=head2 env_name => str

Environment name to read default options from. Default is from program name,
upper-cased, sequences of dashes/nonalphanums replaced with a single underscore,
plus a C<_OPT> suffix. So if your program name is called C<cpandb-cpanmeta> the
default environment name is C<CPANDB_CPANMETA_OPT>.

=head2 read_config => bool (default: 1)

Whether to read configuration files.

=head2 config_dirs => array of str

Which directories to look for configuration file. The default is to look at the
user's home and then system location. On Unix, it's C<< [ "$ENV{HOME}/.config",
$ENV{HOME}, "/etc"] >>. If $ENV{HOME} is empty, getpwuid() is used to get home
directory entry.

=head2 config_filename => str

Configuration filename. The default is C<< program_name . ".conf" >>. For
example, if your program is named C<foo-bar>, config_filename will be
C<foo-bar.conf>.


=head1 METHODS

=head2 $cmd->run() => ENVRES

The main method to run your application. See L</"PROGRAM FLOW"> for more details
on what this method does.

=head2 $cmd->do_completion() => ENVRES

Called by run().

=head2 $cmd->parse_argv() => ENVRES

Called by run().

=head2 $cmd->get_meta($r, $url) => ENVRES

Called by parse_argv() or do_completion(). Subclass has to implement this.


=head1 HOOKS

All hooks will receive the argument C<$r>, a per-request hash/stash. The list
below is by order of calling.

=head2 $cmd->hook_before_run($r)

Called at the start of C<run()>. Can be used to set some initial values of other
C<$r> keys. Or setup the logger.

=head2 $cmd->hook_before_read_config_file($r)

Only called when C<read_config> attribute is true.

=head2 $cmd->hook_after_read_config_file($r)

Only called when C<read_config> attribute is true.

=head2 $cmd->hook_after_get_meta($r)

Called after the C<get_meta> method gets function metadata, which normally
happens during parsing argument, because parsing function arguments require the
metadata (list of arguments, etc).

PC:Lite as well as PC:Classic use this hook to insert a common option
C<--dry-run> if function metadata expresses that function supports dry-run mode.

PC:Lite also checks the C<deps> property here. PC:Classic doesn't do this
because it uses function wrapper (L<Perinci::Sub::Wrapper>) which does this.

=head2 $cmd->hook_after_parse_argv($r)

Called after C<run()> calls C<parse_argv()> and before it checks the result.
C<$r->{parse_argv_res}> will contain the result of C<parse_argv()>. The hook
gets a chance to, e.g. fill missing arguments from other source.

Note that for sources specified in the C<cmdline_src> property, this base class
will do the filling in after running this hook, so no need to do that here.

PC:Lite uses this hook to give default values to function arguments C<<
$r->{args} >> from the Rinci metadata. PC:Classic doesn't do this because it
uses function wrapper (L<Perinci::Sub::Wrapper>) which will do this as well as
some other stuffs (validate function arguments, etc).

=head2 $cmd->hook_before_action($r)

Called before calling the C<action_ACTION> method. Some ideas to do in this
hook: modifying action to run (C<< $r->{action} >>), last check of arguments
(C<< $r->{args} >>) before passing them to function.

PC:Lite uses this hook to validate function arguments. PC:Classic does not do
this because it uses function wrapper which already does this.

=head2 $cmd->hook_after_action($r)

Called after calling C<action_ACTION> method. Some ideas to do in this hook:
preformatting result (C<< $r->{res} >>).

=head2 $cmd->hook_format_result($r)

The hook is supposed to format result in C<$res->{res}> (an array).

All direct subclasses of PC:Base do the formatting here.

=head2 $cmd->hook_display_result($r)

The hook is supposed to display the formatted result (stored in C<$r->{fres}>)
to STDOUT. But in the case of streaming output, this hook can also set it up.

All direct subclasses of PC:Base do the formatting here.

=head2 $cmd->hook_after_run($r)

Called at the end of C<run()>, right before it exits (if C<exit> attribute is
true) or returns C<$r->{res}>. The hook has a chance to modify exit code or
result.


=head1 SPECIAL ARGUMENTS

Below is list of special arguments that may be passed to your function by the
framework. Per L<Rinci> specification, these are prefixed by C<-> (dash).

=head2 -dry_run => bool

Only when in dry run mode, to notify function that we are in dry run mode.

=head2 -cmdline => obj

Only when C<pass_cmdline_object> attribute is set to true. This can be useful
for the function to know about various stuffs, by probing the framework object.

=head2 -cmdline_r => hash

Only when C<pass_cmdline_object> attribute is set to true. Contains the C<$r>
per-request hash/stash. This can be useful for the function to know about
various stuffs, e.g. parsed configuration data, etc.

=head2 -cmdline_src_ARGNAME => str

This will be set if argument is retrieved from C<file>, C<stdin>,
C<stdin_or_file>, C<stdin_or_files>, or C<stdin_line>.

=head2 -cmdline_srcfilenames_ARGNAME => array

An extra information if argument value is retrieved from file(s), so the
function can know the filename(s).


=head1 METADATA PROPERTY ATTRIBUTE

This module observes the following Rinci metadata property attributes:

=head2 cmdline.default_format => STR

Set default output format (if user does not specify via --format command-line
option).


=head1 RESULT METADATA

This module interprets the following result metadata property/attribute:

=head2 attribute: cmdline.exit_code => int

Instruct to use this exit code, instead of using (function status - 300).

=head2 attribute: cmdline.result => any

Replace result. Can be useful for example in this case:

 sub is_palindrome {
     my %args = @_;
     my $str = $args{str};
     my $is_palindrome = $str eq reverse($str);
     [200, "OK", $is_palindrome,
      {"cmdline.result" => ($is_palindrome ? "Palindrome" : "Not palindrome")}];
 }

When called as a normal function we return boolean value. But as a CLI, we
display a more user-friendly message.

=head2 attribute: cmdline.default_format => str

Default format to use. Can be useful when you want to display the result using a
certain format by default, but still allows user to override the default.

=head2 attribute: cmdline.page_result => bool

If you want to filter the result through pager (currently defaults to
C<$ENV{PAGER}> or C<less -FRSX>), you can set C<cmdline.page_result> in result
metadata to true.

For example:

 $SPEC{doc} = { ... };
 sub doc {
     ...
     [200, "OK", $doc, {"cmdline.page_result"=>1}];
 }

=head2 attribute: cmdline.pager => STR

Instruct to use specified pager instead of C<$ENV{PAGER}> or the default C<less>
or C<more>.

=head2 attribute: cmdline.skip_format => bool (default: 0)

When we want the command-line framework to just print the result without any
formatting.

=head2 attribute: x.perinci.cmdline.base.exit_code => int

This is added by this module, so exit code can be tested.


=head1 ENVIRONMENT

=over

=item * PAGER => str

Like in other programs, can be set to select the pager program (when
C<cmdline.page_result> result metadata is active). Can also be set to C<''> or
C<0> to explicitly disable paging even though C<cmd.page_result> result metadata
is active.

=item * PERINCI_CMDLINE_PROGRAM_NAME => STR

Can be used to set CLI program name.

=back
