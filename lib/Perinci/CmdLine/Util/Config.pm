package Perinci::CmdLine::Util::Config;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;
#use Log::Any '$log';

our %SPEC;

sub get_default_config_dirs {
    my @dirs;
    if ($^O eq 'MSWin32') {
        require File::HomeDir;
        push @dirs, File::HomeDir->my_home;
    } else {
        push @dirs, $ENV{HOME};
        push @dirs, "/etc";
    }
    [grep {defined} @dirs];
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

    # put GLOBAL before all other sections
    my @sections = sort {
        ($a eq 'GLOBAL' ? 0:1) <=> ($b eq 'GLOBAL' ? 0:1) ||
            $a cmp $b
        } keys %$conf;

    for my $section (@sections) {
        if (defined $profile) {
            if (length $scn) {
                next unless $section =~ /\A(\Q$scn\E|GLOBAL)\s+\Qprofile=$profile\E\z/;
            } else {
                next unless $section eq "profile=$profile";
            }
        } else {
            if (length $scn) {
                next unless $section eq $scn || $section eq 'GLOBAL';
            } else {
                next unless $section eq 'GLOBAL';
            }
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
        $found++;
    }

    [200, "OK", $args, {'func.found'=>$found}];
}

1;
# ABSTRACT: Utility routines related to config files
