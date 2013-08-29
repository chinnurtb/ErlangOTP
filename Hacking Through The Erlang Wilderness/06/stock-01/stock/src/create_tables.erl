-module(create_tables).
-export([init_tables/0, load_data/2]).

-record(entry, {stock,date,open,high,low,close,volume,adjclose}).

init_tables() ->
    mnesia:create_table(entry,
        [{type,bag}, {attributes, record_info(fields, entry)}]).


load_data(FileName, StockName) ->
    {ok, FileDescriptor} = file:open(FileName, [read]),
    %error_logger:info_msg("load_data: FileName=~p, StockName=~p, FileDescriptor=~p~n", [FileName, StockName, FileDescriptor]),
    %discard first line
    file:read_line(FileDescriptor),
    process_file(FileDescriptor, StockName).

process_file(FD, SN) ->
    case file:read_line(FD) of
        {ok, Line} ->
            %error_logger:info_msg("process_file: Line=~p", [Line]),
            parse_line(Line, SN),
            process_file(FD, SN);
        _ ->
            {done}
    end.

parse_line(Line, StockName) ->
    [Date,Open,High,Low,Close,Volume,AdjClose] = string:tokens(Line, ","),
    Entry = #entry{stock = StockName, date = Date, open = Open, high = High, low = Low, close = Close, volume = Volume, adjclose = AdjClose},
    error_logger:info_msg("parse_line: Entry=~p", [Entry]),
    insert_in_database(Entry).

insert_in_database(NewEntry) ->
    error_logger:info_msg("insert_in_database: NewEntry=~p", [NewEntry]),
    mnesia:transaction(fun() -> mnesia:write(NewEntry) end).
