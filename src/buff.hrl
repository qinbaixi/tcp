%%%-------------------------------------------------------------------
%%% @author Bycc Qin
%%% @copyright (C) 2023
%%% @doc
%%%
%%% @end
%%% Created : 16. 2月 2023 21:19
%%%-------------------------------------------------------------------
-ifndef(__BUFF__).
-define(__BUFF__, 1).

-record(buff, {
    fin = undefined,
    rsv1,
    rsv2,
    rsv3,
    opcode,
    mask_len = 0,
    is_mask = false,
    payload_len = 0,
    payload_data,
    is_end = false
}).

-endif.