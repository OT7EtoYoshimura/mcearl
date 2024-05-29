-module(controller).
-behaviour(gen_server).
-export([start_link/0, get_server/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {pid, node}).

% === %
% API %
% === %
start_link() -> gen_server:start_link(?MODULE, [], []).
get_server(Pid, Ip, Proxy) -> gen_server:call(Pid, {get_server, Ip, Proxy}).

% ========= %
% Callbacks %
% ========= %
init([])
	-> {ok, PeerApp} = application:get_all_key(mcearl_peer)
	,  {ok, NBTApp}  = application:get_all_key(erl_nbt)
	,  {ok, PeerMods} = application:get_key(mcearl_peer, modules)
	,  {ok, NBTMods}  = application:get_key(erl_nbt    , modules)
	,  {ok, [Host | Hosts]} = application:get_env(available_hosts)
	,  application:set_env(mcearl_origin, available_hosts, Hosts)
	,  Ssh = os:find_executable("ssh")
	,  {ok, Pid, Node} = peer:start_link(
		#{connection => standard_io
		, exec => {Ssh, [Host, "erl26 -name " ++ Host ++ " -setcookie gabriel"]}
		}
	)
	,  net_kernel:connect_node(list_to_atom(Host))
	,  lists:map(fun c:nl/1, PeerMods ++ NBTMods)
	,  erpc:call(Node, application, load, [{application, mcearl_peer, PeerApp}])
	,  erpc:call(Node, application, load, [{application, erl_nbt, NBTApp}])
	,  erpc:call(Node, application, ensure_all_started, [mcearl_peer])
	,  {ok, #state{pid=Pid, node=Node}}
	.

handle_call({get_server, Ip, Proxy}, _From, #state{node=Node} = State)
	-> ServPid = server_man:get_server(Node, Ip, Proxy, self())
	,  {reply, ServPid, State}
	;
handle_call(_Req, _From, State)     -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Rsn, #state{node=Node})  -> erpc:call(Node, init, stop, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
