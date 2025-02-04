-module(proxy).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket, ip, pid, transport}).

% === %
% API %
% === %
start_link(Ref, Transport, Opts) -> {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

% ========= %
% Callbacks %
% ========= %
init({Ref, Transport, _Opts = []})
	-> {ok, Socket} = ranch:handshake(Ref)
	,  Transport:setopts(Socket, [{active, once}, binary])
	,  {ok, {Ip, _Port}} = inet:peername(Socket)
	,  Pid = controller_man:get_server(Ip, self())
	,  gen_server:enter_loop(?MODULE, [], #state{socket=Socket, ip=Ip, pid=Pid, transport=Transport})
	.

handle_cast({reply, Pkt}, #state{socket=Socket, transport=Transport} = State)
	-> Transport:send(Socket, Pkt)
	,  {noreply, State}
	;
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, Socket, Data}, #state{transport=Transport, pid=Pid} = State)
	-> server:proxy(Pid, Data)
	,  Transport:setopts(Socket, [{active, once}])
	,  {noreply, State}
	;
handle_info({tcp_closed, _Socket}, #state{pid=Pid} = State)
	-> gen_server:stop(Pid)
	,  {stop, normal, State}
	;
handle_info({tcp_error , _Socket, _Reason}, #state{pid=Pid} = State)
	-> gen_server:stop(Pid)
	,  {stop, normal, State}
	;
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Req, _From, State)     -> {reply, ok, State}.
terminate(_Rsn, _State)             -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
