package Perinci::CmdLine::Base;

# DATE
# VERSION

use 5.010001;

# this class can actually be a role instead of base class for pericmd &
# pericmd-lite, but Mo is more lightweight than Role::Tiny (also R::T doesn't
# have attributes), Role::Basic, or Moo::Role.

use if  $INC{'Perinci/CmdLine/Lite.pm'}, qw(Mo  build default);
use if !$INC{'Perinci/CmdLine/Lite.pm'}, qw(Moo);

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
has riap_client => (is=>'rw');
has riap_client_args => (is=>'rw');
has subcommands => (is=>'rw');
has summary => (is=>'rw');
has tags => (is=>'rw');
has url => (is=>'rw');

has read_config => (is=>'rw', default=>1);
has config_filename => (is=>'rw');
has config_dirs => (is=>'rw');

# role: requires 'get_meta' # ($url)

# role: requires 'hook_before_run'
# role: requires 'hook_after_parse_argv'
# role: requires 'hook_format_result'
# role: requires 'hook_display_result'
# role: requires 'hook_after_run'

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
    my $meta = $self->get_meta($scd->{url} // $self->{url});

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
    # problem. second, it's faster (especially in P::C case).
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

    # parse argv for per-subcommand command-line opts
    if ($r->{skip_parse_subcommand_argv}) {
        return [200, "OK (subcommand options parsing skipped)", \%args];
    } else {
        my $scd = $r->{subcommand_data};
        my $meta = $self->get_meta($scd->{url});
        $r->{meta} = $meta;

        $r->{format} //= $meta->{'cmdline.default_format'};

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
            args                => $scd->{args} ? { %{$scd->{args}} } : undef,
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

sub run {
    my ($self) = @_;

    my $r = {orig_argv=>[@ARGV]};

    # completion is special case, we delegate to do_completion()
    if ($ENV{COMP_LINE}) {
        $r->{res} = $self->do_completion($r);
        goto FORMAT;
    }

    eval {
        $self->hook_before_run($r);

        my $parse_res = $self->parse_argv($r);
        if ($parse_res->[0] == 502) {
            # we need to send ARGV to the server, because it's impossible to get
            # args from ARGV (e.g. there's a cmdline_alias with CODE, which has
            # been transformed into string when crossing network boundary)
            $r->{send_argv} = 1;
        } elsif ($parse_res->[0] != 200) {
            die $parse_res;
        }
        $r->{parse_argv_res} = $parse_res;
        $r->{args} = $parse_res->[2] // {};

        # set defaults
        $r->{action} //= 'call';

        $self->hook_after_parse_argv($r);
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
            $err = [500, "Died: $err"] unless ref($err) eq 'ARRAY';
            $r->{res} = $err;
        } else {
            $r->{res} = [500, "Bug: no response produced"];
        }
    }
    $r->{format} //= $r->{res}[3]{'cmdline.default_format'};
    if (exists $r->{res}[3]{'cmdline.result'}) {
        $r->{res}[2] = $r->{res}[3]{'cmdline.result'};
    }
  FORMAT:
    if ($r->{res}[3]{'cmdline.skip_format'}) {
        $r->{fres} = $r->{res};
    } else {
        $r->{fres} = $self->hook_format_result($r) // '';
    }
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

=back


=head1 ATTRIBUTES

=head2 actions => array

Contains a list of known actions and their metadata. Keys should be action
names, values should be metadata. Metadata is a hash containing these keys:

=over

=item * default_log => BOOL (optional)

Whether to enable logging by default (Log::Any::App) when C<LOG> environment
variable is not set. To speed up program startup, logging is by default turned
off for simple actions like C<help>, C<list>, C<version>.

=item * use_utf8 => BOOL (optional)

Whether to issue C<< binmode(STDOUT, ":utf8") >>. See L</"UTF8 OUTPUT"> for more
details.

=back

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
         $cmd->{_subcommand_name} = 'shutdown';
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

=head2 riap_client => obj

Optional. Can be set to L<Perinci::Access> (or compatible) instance. Sometimes a
Riap request needs to be performed, e.g. when requesting completion to the
server. If this is empty, the request won't be done.

See Perinci::CmdLine where it is set by default. In Perinci::CmdLine::Lite, this
is left undefined by default.

=head2 riap_client_args => hash

Arguments to pass to L<Perinci::Access> constructor. This is useful for passing
e.g. HTTP basic authentication to Riap client
(L<Perinci::Access::HTTP::Client>):

 riap_client_args => {handler_args => {user=>$USER, password=>$PASS}}

=head2 subcommands => hash | code

=head2 subcommands => {NAME => {ARGUMENT=>...}, ...} | CODEREF

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

Set configuration filename. The default is C<< basename($0) . ".conf" >>. For
example, if your program is named C<foo-bar>, config_filename will be
C<foo-bar.conf>.


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

=head2 $cmd->get_meta($url) => ENVRES

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

=head2 x.perinci.cmdline.default_format => STR

Set default output format (if user does not specify via --format command-line
option).


=head1 RESULT_METADATA

This module interprets the following result metadata property/attribute:

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

=head2 attribute: cmdline.skip_format => bool (default: 0)

When we want the command-line framework to just print the result without any
formatting.

=head2 attribute: x.perinci.cmdline.base.exit_code => int

This is added by this module, so exit code can be tested.


=head1 ENVIRONMENT

=over

=item * PERINCI_CMDLINE_PROGRAM_NAME => STR

Can be used to set CLI program name.

=back
