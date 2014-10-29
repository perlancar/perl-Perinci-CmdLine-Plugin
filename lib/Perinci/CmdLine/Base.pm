package Perinci::CmdLine::Base;

# DATE
# VERSION

use 5.010001;

# this class can actually be a role instead of base class for pericmd &
# pericmd-lite, but Mo is more lightweight than Role::Tiny (also R::T doesn't
# have attributes), Role::Basic, or Moo::Role.

use if  $INC{'Perinci/CmdLine.pm'}, qw(Moo);
use if !$INC{'Perinci/CmdLine.pm'}, qw(Mo  build default);

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

has read_config => (is=>'rw', default=>1);
has config_filename => (is=>'rw');
has config_dirs => (
    is=>'rw',
    default => sub {
        ["/etc", $ENV{HOME}];
    },
);

has arg_part_size => (is => 'rw');
has res_part_size => (is => 'rw');

# role: requires 'hook_after_get_meta'
# role: requires 'hook_before_run'
# role: optional 'hook_before_read_config_file'
# role: requires 'hook_after_parse_argv'
# role: requires 'hook_format_result'
# role: requires 'hook_format_row'
# role: requires 'hook_display_result'
# role: requires 'hook_after_run'
# role: requires 'default_prompt_template'

sub hook_before_read_config_file {}

sub get_meta {
    my ($self, $r, $url) = @_;

    my $res = $self->riap_client->request(meta => $url);
    die $res unless $res->[0] == 200;
    my $meta = $res->[2];
    $r->{meta} = $meta;
    $self->hook_after_get_meta($r);
    $meta;
}

sub get_program_and_subcommand_name {
    my ($self, $r) = @_;
    my $res = $self->program_name . " " .
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
    state $cached;
    return $cached if $cached;

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
    $cached = $res;
}

sub status2exitcode {
    my ($self, $status) = @_;
    return 0 if $status == 200;
    $status - 300;
}

sub do_completion {
    require Complete::Bash;

    my ($self, $r) = @_;

    local $r->{in_completion} = 1;

    my ($words, $cword) = @{ Complete::Bash::parse_cmdline(undef, undef, '=') };
    shift @$words; $cword--; # strip program name

    # check whether subcommand is defined. try to search from --cmd, first
    # command-line argument, or default_subcommand.
    {
        # @ARGV given by bash is messed up / different. during completion, we
        # get ARGV from parsing COMP_LINE/COMP_POINT. this might not be the case
        # with other shells like zsh/fish. XXX detect and support other shell.
        local @ARGV = @$words;
        $self->_parse_argv1($r);
    }

    # force format to text for completion, because user might type 'cmd --format
    # blah -^'.
    $r->{format} = 'text';

    my $scd = $r->{subcommand_data};
    my $meta = $self->get_meta($r, $scd->{url} // $self->{url});

    require Perinci::Sub::Complete;
    my $compres = Perinci::Sub::Complete::complete_cli_arg(
        meta            => $meta,
        words           => $words,
        cword           => $cword,
        common_opts     => $self->common_opts,
        riap_server_url => $scd->{url},
        riap_uri        => undef,
        riap_client     => $self->riap_client,
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
            if ($self->subcommands && $r->{subcommand_name_from} ne '--cmd' &&
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
    [200, "OK", Complete::Bash::format_completion($compres)];
}

sub _read_config {
    require Config::IOD::Reader;

    my ($self, $r) = @_;

    if (!$r->{config_paths}) {
        $r->{config_paths} = [];
        my $name = $self->config_filename //
            $self->program_name . ".conf";
        for my $dir (@{ $self->config_dirs }) {
            my $path = "$dir/" . $name;
            push @{ $r->{config_paths} }, $path if -e $path;
        }
    }

    my $reader = Config::IOD::Reader->new;
    my %res;
    for my $path (@{ $r->{config_paths} }) {
        my $hoh = $reader->read_file($path);
        for my $section (keys %$hoh) {
            my $hash = $hoh->{$section};
            for (keys %$hash) {
                $res{$section}{$_} = $hash->{$_};
            }
        }
    }
    \%res;
}

sub _parse_argv1 {
    my ($self, $r) = @_;

    # parse common_opts which potentially sets subcommand
    {
        # one small downside for this is that we cannot do autoabbrev here,
        # because we're not yet specifying all options here.

        require Getopt::Long;
        my $old_go_conf = Getopt::Long::Configure(
            'pass_through', 'permute', 'no_ignore_case', 'no_auto_abbrev');
        my @go_spec;
        my $co = $self->common_opts // {};
        for my $k (keys %$co) {
            push @go_spec, $co->{$k}{getopt} => sub {
                my ($go, $val) = @_;
                $co->{$k}{handler}->($go, $val, $r);
            };
        }
        Getopt::Long::GetOptions(@go_spec);
        Getopt::Long::Configure($old_go_conf);
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
}

sub parse_argv {
    my ($self, $r) = @_;

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

    $self->_parse_argv1($r);

    my %args;

    # read from configuration
    if ($r->{read_config}) {
        $self->hook_before_read_config_file($r);

        my $conf = $self->_read_config($r);
        my $scn  = $r->{subcommand_name};
        my $profile = $r->{config_profile};
        my $found;
        for my $section (keys %$conf) {
            if (defined $profile) {
                if (length $scn) {
                    next unless
                        $section =~ /\A\Q$scn\E\s+\Q$profile\E\z/ # old, deprecated
                            || $section =~ /\A\Q$scn\E\s+\Qprofile=$profile\E\z/;
                } else {
                    next unless $section eq $profile # old, deprecated
                        || $section eq "profile=$profile";
                }
            } else {
                if (length $scn) {
                    next unless $section eq $scn;
                } else {
                    next unless $section eq 'GLOBAL';
                }
            }
            $args{$_} = $conf->{$section}{$_}
                for keys %{ $conf->{$section} };
            $found++;
            last;
        }
        if (defined($profile) && !$found) {
            return [412, "Profile '$profile' not found in configuration file"];
        }
    }

    # parse argv for per-subcommand command-line opts
    if ($r->{skip_parse_subcommand_argv}) {
        return [200, "OK (subcommand options parsing skipped)"];
    } else {
        my $scd = $r->{subcommand_data};
        my $meta = $self->get_meta($r, $scd->{url});

        if ($scd->{args}) {
            $args{$_} = $scd->{args}{$_} for keys %{ $scd->{args} };
        }

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
        for my $av (values %{ $meta->{args} // {} }) {
            if ($av->{cmdline_src}) {
                $has_cmdline_src = 1;
                last;
            }
        }

        require Perinci::Sub::GetArgs::Argv;
        my $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(
            argv                => \@ARGV,
            args                => \%args,
            meta                => $meta,
            allow_extra_elems   => $has_cmdline_src ? 1:0,
            per_arg_json        => $self->{per_arg_json},
            per_arg_yaml        => $self->{per_arg_yaml},
            common_opts         => $copts,
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

        # restore
        for (keys %$copts) {
            $copts->{$_}{handler} = $old_handlers{$_};
        }

        return $res;
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
                    $csa eq 'stdin_line' && $csb =~ /^(stdin|stdin_or_files)/ ? -1 :
                    $csb eq 'stdin_line' && $csa =~ /^(stdin|stdin_or_files)/ ? 1 : 0
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
            my $do_partial_arg = defined($self->arg_part_size) &&
                $meta->{features}{partial_arg};
            $r->{do_partial_arg} = $do_partial_arg;
            if ($src) {
                die [531,
                     "Invalid 'cmdline_src' value for argument '$an': $src"]
                    unless $src =~ /\A(stdin|file|stdin_or_files|stdin_line)\z/;
                die [531,
                     "Sorry, argument '$an' is set cmdline_src=$src, but type ".
                         "is not str/buf/array, only those are supported now"]
                    unless $type =~ /\A(str|buf|array)\z/;
                if ($src =~ /\A(stdin|stdin_or_files)\z/) {
                    die [531, "Only one argument can be specified ".
                             "cmdline_src stdin/stdin_or_files"]
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
                } elsif ($src eq 'stdin' || $src eq 'file' &&
                        ($r->{args}{$an}//"") eq '-') {
                    die [400, "Argument $an must be set to '-' which means ".
                             "from stdin"]
                        if defined($r->{args}{$an}) &&
                            $r->{args}{$an} ne '-';
                    #$log->trace("Getting argument '$an' value from stdin ...");
                    $r->{args}{$an} = $do_partial_arg ?
                        \*STDIN : $is_ary ? [<STDIN>] : do {local $/;<STDIN>};
                } elsif ($src eq 'stdin_or_files') {
                    # push back argument value to @ARGV so <> can work to slurp
                    # all the specified files
                    local @ARGV = @ARGV;
                    unshift @ARGV, $r->{args}{$an}
                        if defined $r->{args}{$an};
                    #$log->tracef("Getting argument '$an' value from ".
                    #                 "stdin_or_files, \@ARGV=%s ...", \@ARGV);

                    # perl doesn't seem to check files, so we check it here
                    for (@ARGV) {
                        next if $_ eq '-';
                        die [500, "Can't read file '$_': $!"] if !(-r $_);
                    }

                    $r->{args}{$an} = $do_partial_arg ?
                        \*ARGV : $is_ary ? [<>] : do { local $/; <> };
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
                    unless (open $fh, "<", $r->{args}{$an}) {
                        die [500, "Can't open file '$r->{args}{$an}' ".
                                 "for argument '$an': $!"];
                    }
                    $r->{args}{$an} = $do_partial_arg ?
                        $fh : $is_ary ? [<$fh>] : do { local $/; <$fh> };
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
    my ($self, $r) = @_;

    my $res = $r->{res};
    my $fres = $r->{fres};
    my $resmeta = $res->[3] // {};

    my $handle = $r->{output_handle};

    use experimental 'smartmatch';
    if ($resmeta->{is_stream}) {
        die [500, "Can't format stream as " . $self->format .
                 ", please use --format text"]
            unless $self->format =~ /^text/;
        my $x = $res->[2];
        if (ref($x) eq 'GLOB') {
            while (!eof($x)) {
                print $handle ~~<$x>;
            }
        } elsif (blessed($x) && $x->can('getline') && $x->can('eof')) {
            # IO::Handle-like object
            while (!$x->eof) {
                print $handle $x->getline;
            }
        } elsif (ref($x) eq 'ARRAY') {
            # tied array
            while (~~(@$x) > 0) {
                print $handle $self->hook_format_row($r, shift(@$x));
            }
        } else {
            die [500, "Invalid stream in result (not a glob/IO::Handle-like ".
                     "object/(tied) array)\n"];
        }
    } else {
        print $handle $fres;
    }
}

sub run {
    my ($self) = @_;

    my $r = {orig_argv=>[@ARGV]};

    # completion is special case, we delegate to do_completion()
    if ($ENV{COMP_LINE}) {
        $r->{res} = $self->do_completion($r);
        goto FORMAT;
    }

    if ($self->read_config) {
        # note that we have read the config
        $r->{read_config} = 1;
    }

    eval {
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

        $self->hook_after_parse_argv($r);
        $self->parse_cmdline_src($r);
        my $missing = $parse_res->[3]{"func.missing_args"};
        die [400, "Missing required argument(s): ".join(", ", @$missing)]
            if $missing && @$missing;

        my $args = $parse_res->[2];
        my $scd = $r->{subcommand_data};
        $args->{-cmdline} = $self if $scd->{pass_cmdline_object} //
            $self->pass_cmdline_object;

        my $meth = "run_$r->{action}";
        die [500, "Unknown action $r->{action}"] unless $self->can($meth);
        $r->{res} = $self->$meth($r);
    };
    my $err = $@;
    if ($err || !$r->{res}) {
        if ($err) {
            $err =~ s/\n+$//;
            $err = [500, "Died: $err"] unless ref($err) eq 'ARRAY';
            $r->{res} = $err;
        } else {
            $r->{res} = [500, "Bug: no response produced"];
        }
    }
    $r->{format} //= $r->{res}[3]{'cmdline.default_format'};
    $r->{format} //= $r->{meta}{'cmdline.default_format'};
    if (exists $r->{res}[3]{'cmdline.result'}) {
        $r->{res}[2] = $r->{res}[3]{'cmdline.result'};
    }
  FORMAT:
    if ($r->{res}[3]{'cmdline.skip_format'}) {
        $r->{fres} = $r->{res}[2];
    } else {
        $r->{fres} = $self->hook_format_result($r) // '';
    }
    $self->select_output_handle($r);
    $self->hook_display_result($r);
    $self->hook_after_run($r);

    my $exitcode;
    if ($r->{res}[3] && defined($r->{res}[3]{'cmdline.exit_code'})) {
        $exitcode = $r->{res}[3]{'cmdline.exit_code'};
    } else {
        $exitcode = $self->status2exitcode($r->{res}[0]);
    }
    if ($self->exit) {
        exit $exitcode;
    } else {
        # so this can be tested
        $r->{res}[3]{'x.perinci.cmdline.base.exit_code'} = $exitcode;
        return $r->{res};
    }
}

1;
# ABSTRACT: Base class for Perinci::CmdLine{,::Lite}

=for Pod::Coverage ^(.+)$

=head1 REQUEST KEYS

The various values in the C<$r> hash/stash.

=over

=item * action => str

Selected action to use. Usually set from the common options.

=item * format => str

Selected format to use. Usually set from the common option C<--format>.

=item * read_config => bool

=item * config_paths => array of str

=item * config_profile => str

=item * parse_argv_res => array

Enveloped result of C<parse_argv()>.

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

=item * do_partial_arg => str

Set to argument name which will be sent in chunks, if we are doing chunked
argument (sending large argument in chunks via several call requests). This will
be enabled when C<arg_part_size> is set, function supports the C<partial_arg>
feature, the argument has its C<partial> property set, and the partial argument
comes from stdin or files.

=item * do_partial_res => bool

Set to true if we are doing chunked response (retrieving potentially large
amount of result in chunks via several call requests). This will be enabled when
C<res_part_size> is set and function supports the C<partial_res> feature.

=back


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
C<format-options=s>), json*, yaml*, perl*. If there are more than one
subcommands, this will also be added: list (getopt C<list|l>). If dry-run is
supported by function, there will also be: dry_run (getopt C<dry-run>). If undo
is turned on, there will also be: undo (getopt C<undo>), redo (getopt C<redo>),
history (getopt C<history>), clear_history (getopt C<clear-history>).

*) Currently only added if you say C<use Perinci::CmdLine 1.04>.

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

Passing the cmdline object can be useful, e.g. to call run_help(), etc.

=head2 program_name => str

Default is from PERINCI_CMDLINE_PROGRAM_NAME environment or from $0.

=head2 riap_client => float (default: 1.1)

Specify L<Riap> protocol version to use. Will be passed to C<riap_client_args>.

=head2 riap_client => obj

Optional. Can be set to L<Perinci::Access> (or compatible) instance. Sometimes a
Riap request needs to be performed, e.g. when requesting completion to the
server. If this is empty, the request won't be done.

See Perinci::CmdLine where it is set by default. In Perinci::CmdLine::Lite, this
is left undefined by default.

=head2 riap_version => float (default: 1.1)

Will be passed to Riap client constructor as well.

=head2 riap_client_args => hash

Arguments to pass to L<Perinci::Access> constructor. This is useful for passing
e.g. HTTP basic authentication to Riap client
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

=head2 read_config => bool (default: 1)

Whether to read configuration files.

=head2 config_dirs => array of str

Which directories to look for configuration file. The default is to look at the
system location and then per-user home directory. On Unix, it's C<< ["/etc",
$ENV{HOME}] >>.

=head2 config_filename => str

Configuration filename. The default is C<< program_name . ".conf" >>. For
example, if your program is named C<foo-bar>, config_filename will be
C<foo-bar.conf>.

=head2 arg_part_size => int

If set, turn on sending of argument in chunks (see C<partial_arg> feature in
L<Rinci::function>, specified since Rinci 1.1.63). Can be useful if argument to
be sent is very large. For example, if you set this attribute to C<10*1024*1024>
(10MB) then if argument is 15MB, it will be sent in two steps (two Riap C<call>
requests): the first 10MB and then the rest (5MB).

The function must support C<partial_arg> feature, and the correspondin argument
must have its C<partial> property set to true. Additionally, chunking will only
be done if argument comes from files or stdin.

=head2 res_part_size => int

If set, turn on requesting result in chunks (see L<partial_res> feature in
L<Rinci::function>, specified since Rinci 1.1.63). Can be useful if result is
potentially very large. For example, if you set this attribute to
C<10*1024*1024> (10MB) then if result is 15MB, it will be retrieved in two steps
(two Riap C<call> requests): the first 10MB and then the rest (5MB).

The function must support C<partial_res> feature.


=head1 METHODS

=head2 $cmd->run() => ENVRES

Will parse command-line arguments with C<parse_argv()>, select/set subcommand,
call hooks, run the appropriate C<run_ACTION()> method, and finally format and
display the result.

The C<run_ACTION()> methods will be passed C<$r> and is supposed to return an
enveloped result. The result will then be put in C<< $r->{res} >>.

If C<exit> attribute is true, will exit with the action's envelope result
status. If status is 200, exit code is 0. Otherwise exit code is status minus
300. So, a response C<< [501, "Not implemented"] >> will result in exit code of
201.

If C<exit> attribute is false, will simply return the action result (C<<
$r->{res} >>). And will also return the exit code in C<<
$r->{res}[3]{'x.perinci.cmdline.base.exit_code'} >>.


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

=head2 $cmd->hook_after_parse_argv($r)

Called after C<run()> calls C<parse_argv()> and before it checks the result.
C<$r->{parse_argv_res}> will contain the result of C<parse_argv()>. The hook
gets a chance to, e.g. fill missing arguments from other source.

=head2 $cmd->hook_format_result($r)

The hook is supposed to format result in C<$res->{res}> (an array).

=head2 $cmd->hook_display_result($r)

The hook is supposed to display the formatted result (stored in C<$r->{fres}>)
to STDOUT. But in the case of streaming output, this hook can also set it up.

=head2 $cmd->hook_after_run($r)

Called at the end of C<run()>, right before it exits (if C<exit> attribute is
true) or returns C<$r->{res}>. The hook has a chance to modify exit code or
result.


=head1 METADATA PROPERTY ATTRIBUTE

This module observes the following Rinci metadata property attributes:

=head2 cmdline.default_format => STR

Set default output format (if user does not specify via --format command-line
option).


=head1 RESULT METADATA

This module interprets the following result metadata property/attribute:

=head2 property: is_stream => BOOL

XXX should perhaps be defined as standard in L<Rinci::function>.

If set to 1, signify that result is a stream. Result must be a glob, or an
object that responds to getline() and eof() (like a Perl L<IO::Handle> object),
or an array/tied array. Format must currently be C<text> (streaming YAML might
be supported in the future). Items of result will be displayed to output as soon
as it is retrieved, and unlike non-streams, it can be infinite.

An example function:

 $SPEC{cat_file} = { ... };
 sub cat_file {
     my %args = @_;
     open my($fh), "<", $args{path} or return [500, "Can't open file: $!"];
     [200, "OK", $fh, {is_stream=>1}];
 }

another example:

 use Tie::Simple;
 $SPEC{uc_file} = { ... };
 sub uc_file {
     my %args = @_;
     open my($fh), "<", $args{path} or return [500, "Can't open file: $!"];
     my @ary;
     tie @ary, "Tie::Simple", undef,
         SHIFT     => sub { eof($fh) ? undef : uc(~~<$fh> // "") },
         FETCHSIZE => sub { eof($fh) ? 0 : 1 };
     [200, "OK", \@ary, {is_stream=>1}];
 }

See also L<Data::Unixish> and L<App::dux> which deals with streams.

=head2 attribute: cmdline.exit_code => int

Instruct Perinci::CmdLine to use this exit code, instead of using (function
status - 300).

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

Instruct Perinci::CmdLine to use specified pager instead of C<$ENV{PAGER}> or
the default C<less> or C<more>.

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
