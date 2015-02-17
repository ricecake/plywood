-module(plywood_viz).

-export([init/2]).

init(Req, Opts) ->
	Index      = cowboy_req:binding(index, Req),
	{ok, Body} = treeview_dtl:render([{index, Index}]),
	Req2       = cowboy_req:reply(200, [], Body, Req),
	{ok, Req2, Opts}.

