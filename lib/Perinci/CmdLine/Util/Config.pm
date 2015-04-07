package Perinci::CmdLine::Util::Config;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use PERLANCAR::File::HomeDir qw(get_my_home_dir);

our %SPEC;

$SPEC{get_default_config_dirs} = {
    v => 1.1,
    args => {},
};
sub get_default_config_dirs {
    my @dirs;
    local $PERLANCAR::File::HomeDir::DIE_ON_FAILURE = 1;
    my $home = get_my_home_dir();
    if ($^O eq 'MSWin32') {
        push @dirs, $home;
    } else {
        push @dirs, "$home/.config", $home, "/etc";
    }
    \@dirs;
}

$SPEC{read_config} = {
    v => 1.1,
    args => {
        config_paths => {},
        config_filenames => {},
        config_dirs => {},
        program_name => {},
    },
};
sub read_config {
    require Config::IOD::Reader;

    my %args = @_;

    my $config_dirs = $args{config_dirs} // get_default_config_dirs();

    my $paths;
    if ($args{config_paths}) {
        $paths = $args{config_paths};
    } else {
        my $name = $args{config_filename} //
            $args{program_name} . ".conf";
        for my $dir (@$config_dirs) {
            my $path = "$dir/" . $name;
            push @$paths, $path if -e $path;
        }
    }

    my $reader = Config::IOD::Reader->new;
    my %res;
    my @read;
    for my $path (@$paths) {
        my $hoh = $reader->read_file($path);
        push @read, $path;
        for my $section (keys %$hoh) {
            my $hash = $hoh->{$section};
            for (keys %$hash) {
                $res{$section}{$_} = $hash->{$_};
            }
        }
    }
    [200, "OK", \%res, {'func.read_files' => \@read}];
}

$SPEC{get_args_from_config} = {
    v => 1.1,
    args => {
        config => {},
        args => {},
        subcommand_name => {},
        config_profile => {},
        meta => {},
        meta_is_normalized => {},
    },
};
sub get_args_from_config {
    my %fargs = @_;

    my $conf    = $fargs{config};
    my $scn     = $fargs{subcommand_name} // '';
    my $profile = $fargs{config_profile};
    my $args    = $fargs{args} // {};
    my $meta    = $fargs{meta};
    my $found;

    unless ($fargs{meta_is_normalized}) {
        require Perinci::Sub::Normalize;
        $meta = Perinci::Sub::Normalize::normalize_function_metadata($meta);
    }

    # put GLOBAL before all other sections
    my @sections = sort {
        ($a eq 'GLOBAL' ? 0:1) <=> ($b eq 'GLOBAL' ? 0:1) ||
            $a cmp $b
        } keys %$conf;

    my %seen_profiles; # for debugging message
    for my $section (@sections) {
        my ($sect_scn, $sect_profile);
        if ($section =~ /\A\w+\z/) {
            $sect_scn = $section;
        } elsif ($section =~ /\Aprofile=(.*)\z/) {
            $sect_scn = 'GLOBAL';
            $sect_profile = $1;
        } elsif ($section =~ /\A(\w+)\s+profile=(.*)\z/) {
            $sect_scn = $1;
            $sect_profile = $2;
        } else {
            die [412, "Error in config file: invalid section name ".
                     "'$section', please use subcommand name + optional ".
                         "' profile=PROFILE' only"];
        }
        $seen_profiles{$sect_profile}++ if defined $sect_profile;
        if (length $scn) {
            next if $sect_scn ne 'GLOBAL' && $sect_scn ne $scn;
        } else {
            next if $sect_scn ne 'GLOBAL';
        }
        if (defined $profile) {
            next if defined($sect_profile) && $sect_profile ne $profile;
            $found++ if defined($sect_profile) && $sect_profile eq $profile;
        } else {
            next if defined($sect_profile);
        }

        my $as = $meta->{args} // {};
        for my $k (keys %{ $conf->{$section} }) {
            my $v = $conf->{$section}{$k};
            # since IOD might return a scalar or an array (depending on whether
            # there is a single param=val or multiple param= lines), we need to
            # arrayify the value if the argument is expected to be an array.
            if (ref($v) ne 'ARRAY' && $as->{$k} && $as->{$k}{schema} &&
                    $as->{$k}{schema}[0] eq 'array') {
                $args->{$k} = [$v];
            } else {
                $args->{$k} = $v;
            }
        }
    }
    $log->tracef("[pericmd] Seen config profiles: %s",
                 [sort keys %seen_profiles]);

    [200, "OK", $args, {'func.found'=>$found}];
}

1;
# ABSTRACT: Utility routines related to config files
