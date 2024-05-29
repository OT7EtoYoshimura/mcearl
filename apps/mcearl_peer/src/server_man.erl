-module(server_man).
-behaviour(gen_server).
-export([start_link/0, get_server/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {taken,avail}).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
get_server(Node, Ip, Proxy, Controller) -> gen_server:call({server_man, Node}, {get_server, Ip, Proxy, Controller}).

% ========= %
% Callbacks %
% ========= %
init([]) -> {ok, #state{taken=[], avail=lists:delete(-1, lists:seq(127, -128, -1))}}.

handle_call({get_server, Ip, Proxy, Controller}, _From, #state{taken=Taken,avail=Avail} = State)
	-> {ok, Pid} = server_sup:start_server()
	,  [Id|NewAvail] = Avail
	,  server:intro(Pid, Id, Ip, Proxy, Controller)
	,  Ref = monitor(process, Pid)
	,  {reply, Pid, State#state{taken=[{Ref, Id}|Taken], avail=NewAvail}}
	;
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_info({'DOWN', Ref, process, _Pid, _Info}, #state{taken=Taken, avail=Avail} = State)
	-> Id = proplists:get_value(Ref, Taken)
	,  NewTaken = proplists:delete(Ref, Taken)
	,  {noreply, State#state{taken=NewTaken, avail=[Id|Avail]}}
	;
handle_info(_Info, State)           -> {noreply, State}.

handle_cast(_Msg, State)            -> {noreply, State}.
terminate(_Rsn, _State)             -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
