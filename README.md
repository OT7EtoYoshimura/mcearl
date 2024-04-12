McEarl
=====
A Minecraft Classic server written in Erlang/OTP

Developed as an individual project for my 6th semester
"Advanced Software" course at Fontys Hogeschool.

Dependencies
----
* Erlang/OTP
* rebar3
* pdflatex (optional; documentation)
* plantuml (optional; documentation)
* inotify-tools (optional; code reload)

Build & Documentation
----
```
$ rebar3 compile
$ rebar3 edoc
```

Type and cross-referency checks
----
```
$ rebar3 dialyzer
$ rebar3 xref
```

Tests
----
```
$ rebar3 eunit --cover
$ rebar3 ct --cover
$ rebar3 proper --cover
$ rebar3 cover --verbose
```
