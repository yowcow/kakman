-module(kakman_file).

-include_lib("kernel/include/file.hrl").

-export([
         open/1,
         close/1,
         write/2,
         rotate/3
        ]).

-spec open(string()) -> {ok, file:io_device(), integer()} | {error, term()}.
open(FilePath) ->
    case filelib:ensure_dir(FilePath) of
        ok ->
            case file:open(FilePath, [write, append, binary]) of
                {ok, Fd} ->
                    case file:read_file_info(FilePath) of
                        {ok, FileInfo} ->
                            Size = FileInfo#file_info.size,
                            {ok, Fd, Size}
                    end;
                Err2 ->
                    Err2
            end;
        Err1 ->
            Err1
    end.

-spec close(file:io_device()) -> ok.
close(Fd) ->
    file:datasync(Fd),
    file:close(Fd).

-spec write(file:io_device(), binary()|list()) -> {ok, integer()}.
write(Fd, List) when is_list(List) ->
    write(Fd, list_to_binary(List));
write(Fd, Bin) when is_binary(Bin) ->
    case Bin of
        <<>> ->
            {ok, 0};
        _ ->
            Out = <<Bin/binary, <<"\n">>/binary>>,
            ok = file:write(Fd, Out),
            {ok, byte_size(Out)}
    end;
write(_, _) ->
    {ok, 0}.

-spec rotate(file:io_device(), string(), integer()) -> {ok, file:io_device(), integer()} | {error, term()}.
rotate(Fd, FilePath, Rotations) ->
    ok = rotate_old_files(FilePath, Rotations),
    ok = close(Fd),
    ok = file:rename(FilePath, FilePath++".0"),
    open(FilePath).

-spec rotate_old_files(string(), integer()) -> ok.
rotate_old_files(_, 0) ->
    ok;
rotate_old_files(BaseFilePath, Index) ->
    FilePathFrom = BaseFilePath ++ "." ++ integer_to_list(Index-1),
    FilePathTo = BaseFilePath ++ "." ++ integer_to_list(Index),
    file:rename(FilePathFrom, FilePathTo),
    rotate_old_files(BaseFilePath, Index-1).
