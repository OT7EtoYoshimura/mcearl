FROM erlang:alpine
RUN mkdir /buildroot
WORKDIR /buildroot
COPY . mcearl
WORKDIR mcearl
RUN rebar3 as prod release

FROM alpine
RUN apk --no-cache add git openssl ncurses-libs libstdc++
COPY --from=0 /buildroot/mcearl/_build/prod/rel/mcearl /mcearl
EXPOSE 25565
CMD [ "/mcearl/bin/mcearl", "foreground" ]
