-module(windex_viz).

-export([init/2]).

init(Req, Opts) ->
	Index = cowboy_req:binding(index, Req),
	{ok, Body} = treeview_dtl:render([{index, Index}]),
	{ok, Reply} = cowboy_req:reply(200, [], Body, Req),
	{ok, Reply, Opts}.
