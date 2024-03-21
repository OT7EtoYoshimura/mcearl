McEarl
=====
A Minecraft Classic server written in Erlang/OTP

Developed as an individual project for my 6th semester
"Advanced Software" at Fontys Hogeschool.

Dependencies
----
* Erlang/OTP
* rebar3
* pdflatex (optional)
* plantuml (optional)

Build & Documentation
----
```
$ rebar3 compile
$ rebar3 edoc
```

Type and cross-referency checks
----
```
$ rebar3 gradualizer
$ rebar3 xref
```

Tests
----
```
$ rebar3 eunit
$ rebar3 ct
$ rebar3 proper
$ rebar3 cover
```
