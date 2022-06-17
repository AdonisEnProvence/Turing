-module(cors_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

% See https://github.com/ninenines/cowboy/pull/1001/files
% To understand how CORS works, see: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS.
-spec execute(Req, Env) -> {ok | stop, Req, Env} when Req :: cowboy_req:req(), Env :: any().
execute(#{headers := #{<<"origin">> := Origin}, method := Method} = Req, Env) ->
    % See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
    ReqWithAllowOrigin = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),

    % See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin#cors_and_caching
    ReqWithVary = cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, ReqWithAllowOrigin),

    case Method of
        <<"OPTIONS">> ->
            % See https://developer.mozilla.org/fr/docs/Web/HTTP/Headers/Access-Control-Allow-Methods
            ReqWithAllowMethods = cowboy_req:set_resp_header(
                <<"access-control-allow-methods">>, <<"POST, OPTIONS">>, ReqWithVary
            ),

            % See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Headers
            ReqWithAllowHeaders = cowboy_req:set_resp_header(
                <<"access-control-allow-headers">>, <<"Content-Type">>, ReqWithAllowMethods
            ),

            ReqWithStatus = cowboy_req:reply(200, ReqWithAllowHeaders),
            {stop, ReqWithStatus};
        _ ->
            {ok, ReqWithVary, Env}
    end;
execute(Req, Env) ->
    {ok, Req, Env}.
