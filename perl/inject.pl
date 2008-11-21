#!/usr/bin/perl

use re 'eval';

$a = 'bla';
$user_input = "(?{print \"we're executing this code ;-)\n\";})";
$a =~ /$user_input/;

