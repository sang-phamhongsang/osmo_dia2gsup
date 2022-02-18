= Osmocom DIAMETER -> GSUP translator =

This project implements a proxy translating Diameter into GSUP, hence allowing
Diameter clients connecting to GSUP server. This allows, for instance, using
OsmoHLR as an HSS used by 4G/5G nodes.

== Building ==

Install erlang and rebar3 packages (not "rebar", that's version 2! You may need
to compile it from source in some distros).

$ rebar3 compile
$ rebar3 escriptize

== Testing ==

Unit tests can be run this way:
$ rebar3 eunit

== Running ==

Once osmo_dia2gsup is built, you can start it this way:

$ rebar3 shell

In the erlang shell:
1> osmo_dia2gsup:start().
