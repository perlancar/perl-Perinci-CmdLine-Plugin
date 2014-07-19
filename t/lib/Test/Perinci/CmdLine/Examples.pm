package
    Test::Perinci::CmdLine::Examples;

our $VERSION = 0.20140719;
our $DATE    = '2014-07-19';

$SPEC{f_hello} = {
    v => 1.1,
    summary => 'Summary for f_hello',
    description => <<'_',

Description for f_hello

_
};
sub f_hello {
    my %args = @_;
    [200, "OK", "Hello, world!"];
}

1;
