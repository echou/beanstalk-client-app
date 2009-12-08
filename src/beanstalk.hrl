-define(PRINT(Fmt, Args), error_logger:info_msg("[~p] " ++ Fmt ++ "~n", [?MODULE|Args])).

