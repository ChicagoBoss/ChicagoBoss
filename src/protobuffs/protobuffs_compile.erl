%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(protobuffs_compile).
-export([scan_file/1, scan_file/2, generate_source/1, generate_source/2]).

-record(collected,{enum=[], msg=[], extensions=[]}).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string()) ->
		       ok | {error, _}.
scan_file(ProtoFile) ->
    scan_file(ProtoFile,[]).

%%--------------------------------------------------------------------
%% @doc Generats a built .beam file and header file .hrl
%%      Considerd option properties: output_include_dir, 
%%                                   output_ebin_dir,
%%                                   imports_dir
%%--------------------------------------------------------------------
-spec scan_file(ProtoFile :: string(), Options :: list()) ->
		       ok | {error, _}.
scan_file(ProtoFile,Options) when is_list(ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    {ok,FirstParsed} = parse(ProtoFile),
    ImportPaths = ["./", "src/" | proplists:get_value(imports_dir, Options, [])],
    Parsed = parse_imports(FirstParsed, ImportPaths),
    Collected = collect_full_messages(Parsed), 
    Messages = resolve_types(Collected#collected.msg,Collected#collected.enum),
    output(Basename, Messages, Collected#collected.enum, Options).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%--------------------------------------------------------------------
-spec generate_source(ProtoFile :: string()) ->
			     ok | {error, _}.
generate_source(ProtoFile) ->
    generate_source(ProtoFile,[]).

%%--------------------------------------------------------------------
%% @doc Generats a source .erl file and header file .hrl
%%      Consider option properties: output_include_dir, 
%%                                  output_src_dir,
%%                                  imports_dir
%%--------------------------------------------------------------------
-spec generate_source(ProtoFile :: string(), Options :: list()) ->
			     ok | {error, _}.
generate_source(ProtoFile,Options) when is_list (ProtoFile) ->
    Basename = filename:basename(ProtoFile, ".proto") ++ "_pb",
    {ok,FirstParsed} = parse(ProtoFile),
    ImportPaths = ["./", "src/" | proplists:get_value(imports_dir, Options, [])],
    Parsed = parse_imports(FirstParsed, ImportPaths),
    Collected = collect_full_messages(Parsed), 
    Messages = resolve_types(Collected#collected.msg,Collected#collected.enum),
    output_source (Basename, Messages, Collected#collected.enum, Options).

%% @hidden
parse_imports(Parsed, Path) ->
    parse_imports(Parsed, Path, []).

%% @hidden
parse_imports([], _Path, Acc) ->
    lists:reverse(Acc);
parse_imports([{import, File} = Head | Tail], Path, Acc) ->
    case file:path_open(Path, File, [read]) of
	{ok, F, Fullname} ->
	    file:close(F),
	    {ok,FirstParsed} = parse(Fullname),
	    Parsed = lists:append(FirstParsed, Tail),
	    parse_imports(Parsed, Path, [Head | Acc]);
	{error, Error} ->
	    error_logger:error_report([
				       "Could not do import",
				       {import, File},
				       {error, Error},
				       {path, Path}
				      ]),
	    parse_imports(Tail, Path, [Head | Acc])
    end;
parse_imports([Head | Tail], Path, Acc) ->
    parse_imports(Tail, Path, [Head | Acc]).

%% @hidden
output(Basename, Messages, Enums, Options) ->
    case proplists:get_value(output_include_dir,Options) of
	undefined ->
	    HeaderFile = Basename ++ ".hrl";
	HeaderPath ->
	    HeaderFile = filename:join(HeaderPath,Basename) ++ ".hrl"
    end,
    error_logger:info_msg("Writing header file to ~p~n",[HeaderFile]),
    ok = write_header_include_file(HeaderFile, Messages),
    PokemonBeamFile = filename:dirname(code:which(?MODULE)) ++ "/pokemon_pb.beam",
    {ok,{_,[{abstract_code,{_,Forms}}]}} = beam_lib:chunks(PokemonBeamFile, [abstract_code]),
    Forms1 = filter_forms(Messages, Enums, Forms, Basename, []),
    {ok, _, Bytes, _Warnings} = compile:forms(Forms1, [return, debug_info]),
    case proplists:get_value(output_ebin_dir,Options) of
	undefined ->
	    BeamFile = Basename ++ ".beam";
	BeamPath ->
	    BeamFile = filename:join(BeamPath,Basename) ++ ".beam"
    end,
    error_logger:info_msg("Writing beam file to ~p~n",[BeamFile]),
    file:write_file(BeamFile, Bytes).

%% @hidden
output_source (Basename, Messages, Enums, Options) ->
    case proplists:get_value(output_include_dir,Options) of
	undefined ->
	    HeaderFile = Basename ++ ".hrl";
	HeaderPath ->
	    HeaderFile = filename:join(HeaderPath,Basename) ++ ".hrl"
    end,
    error_logger:info_msg("Writing header file to ~p~n",[HeaderFile]),
    ok = write_header_include_file(HeaderFile, Messages),
    PokemonBeamFile = filename:dirname(code:which(?MODULE)) ++ "/pokemon_pb.beam",
    {ok,{_,[{abstract_code,{_,Forms}}]}} = beam_lib:chunks(PokemonBeamFile, [abstract_code]),
    Forms1 = filter_forms(Messages, Enums, Forms, Basename, []),
    case proplists:get_value(output_src_dir,Options) of
	undefined ->
	    SrcFile = Basename ++ ".erl";
	SrcPath ->
	    SrcFile = filename:join(SrcPath,Basename) ++ ".erl"
    end,
    error_logger:info_msg("Writing src file to ~p~n",[SrcFile]),
    file:write_file(SrcFile, erl_prettypr:format(erl_syntax:form_list (Forms1))).

%% @hidden
parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile,[]),
    file:close(InFile),
    protobuffs_parser:parse(Acc).

%% @hidden
loop(InFile,Acc) ->
    case io:request(InFile,{get_until,prompt,protobuffs_scanner,token,[1]}) of
        {ok,Token,_EndLine} ->
            loop(InFile,Acc ++ [Token]);
        {error,token} ->
            exit(scanning_error);    
        {eof,_} ->
            Acc
    end.

%% @hidden
filter_forms(Msgs, Enums, [{attribute,L,file,{_,_}}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,file,{"src/" ++ Basename ++ ".erl",L}}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,module,pokemon_pb}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,module,list_to_atom(Basename)}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,export,[{encode_pikachu,1},{decode_pikachu,1}]}|Tail], Basename, Acc) ->
    Exports = lists:foldl(
		fun({Name,_}, Acc1) ->
			[{list_to_atom("encode_" ++ string:to_lower(Name)),1},
			 {list_to_atom("decode_" ++ string:to_lower(Name)),1} | Acc1]
		end, [], Msgs),
    filter_forms(Msgs, Enums, Tail, Basename, [{attribute,L,export,Exports}|Acc]);

filter_forms(Msgs, Enums, [{attribute,L,record,{pikachu,_}}|Tail], Basename, Acc) ->
    Records = [begin
		   OutFields = [string:to_lower(A) || {_, _, _, A, _} <- lists:keysort(1, Fields)],
		   Frm_Fields = [{record_field,L,{atom,L,list_to_atom(OutField)}}|| OutField <- OutFields],
		   {attribute, L, record, {atomize(Name), Frm_Fields}}
	       end || {Name, Fields} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Records ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
		     {function,L,list_to_atom("encode_" ++ string:to_lower(Name)),1,[replace_atom(Clause, pikachu, atomize(Name))]} 
		 end || {Name, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,encode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_encode_function(Msgs, L, Clause)|Acc]);

 filter_forms(Msgs, Enums, [{function,L,iolist,2,[Clause]}|Tail], Basename, Acc) ->
     filter_forms(Msgs, Enums, Tail, Basename, [expand_iolist_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,decode_pikachu,1,[Clause]}|Tail], Basename, Acc) ->
    Functions = [begin
		     {function,
		      L,
		      list_to_atom("decode_" ++ string:to_lower(Name)),
		      1,
		      [replace_atom(Clause, pikachu, atomize(Name))]} 
		 end || {Name, _} <- Msgs],
    filter_forms(Msgs, Enums, Tail, Basename, Functions ++ Acc);

filter_forms(Msgs, Enums, [{function,L,decode,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_decode_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,to_record,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_to_record_function(Msgs, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,enum_to_int,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_enum_to_int_function(Enums, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [{function,L,int_to_enum,2,[Clause]}|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [expand_int_to_enum_function(Enums, L, Clause)|Acc]);

filter_forms(Msgs, Enums, [Form|Tail], Basename, Acc) ->
    filter_forms(Msgs, Enums, Tail, Basename, [Form|Acc]);

filter_forms(_, _, [], _, Acc) -> lists:reverse(Acc).

%% @hidden
expand_encode_function(Msgs, Line, Clause) ->
    {function,Line,encode,2,[filter_encode_clause(Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_encode_clause({MsgName, _Fields}, {clause,L,_Args,Guards,_Content}) ->
    ToBin = {call,L,{atom,L,iolist_to_binary},[{call,L,
                                                {atom,L,iolist},
                                                [{atom,L,atomize(MsgName)},{var,L,'Record'}]}]},
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Record'}],Guards,[ToBin]}.

expand_iolist_function(Msgs, Line, Clause) ->
    {function,Line,iolist,2,[filter_iolist_clause(Msg, Clause) || Msg <- Msgs]}.

filter_iolist_clause({MsgName, Fields}, {clause,L,_Args,Guards,_Content}) ->
    Cons = lists:foldl(
	     fun({FNum,Tag,SType,SName,Default}, Acc) ->
		     {cons,L,
		      {call,L,{atom,L,pack},[{integer,L,FNum},
					     {atom,L,Tag},
					     {call,L,
					      {atom,L,with_default},
					      [{record_field,L,
						{var,L,'Record'},atomize(MsgName),
						{atom,L,atomize(SName)}},
					       erl_parse:abstract(Default)]},
					     {atom,L,atomize(SType)},
					     {nil,L}]},
		      Acc}
	     end, {nil,L}, Fields),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Record'}],Guards,[Cons]}.

%% @hidden
expand_decode_function(Msgs, Line, Clause) ->
    {function,Line,decode,2, [{clause,Line,[{atom,Line,enummsg_values},{integer,Line,1}],[],[{atom,Line,value1}]}] ++ 
     [filter_decode_clause(Msgs, Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_decode_clause(Msgs, {MsgName, Fields}, {clause,L,_Args,Guards,[_,B,C]}) ->
    Types = lists:keysort(1, [{FNum, list_to_atom(SName), 
			       atomize(SType), 
			       decode_opts(Msgs, Tag, SType)} || 
				 {FNum,Tag,SType,SName,_} <- Fields]),
    Cons = lists:foldl(
	     fun({FNum, FName, Type, Opts}, Acc) ->
		     {cons,L,{tuple,L,[{integer,L,FNum},{atom,L,FName},{atom,L,Type},erl_parse:abstract(Opts)]},Acc}
	     end, {nil,L}, Types),
    A = {match,L,{var,L,'Types'},Cons},
    C1 = replace_atom(C, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},{var,L,'Bytes'}],Guards,[A,B,C1]}.

%% @hidden
decode_opts(Msgs, Tag, Type) ->
    Opts0 = if Tag == repeated -> [repeated]; Tag == repeated_packed -> [repeated_packed]; true -> [] end,
    case lists:keymember(Type, 1, Msgs) of
        true ->
            [is_record|Opts0];
        false ->
            Opts0
    end.

%% @hidden
expand_to_record_function(Msgs, Line, Clause) ->
    {function,Line,to_record,2,[filter_to_record_clause(Msg, Clause) || Msg <- Msgs]}.

%% @hidden
filter_to_record_clause({MsgName, _}, {clause,L,[_Param1,Param2],Guards,[Fold]}) ->
    Fold1 = replace_atom(Fold, pikachu, atomize(MsgName)),
    {clause,L,[{atom,L,atomize(MsgName)},Param2],Guards,[Fold1]}.

%% @hidden
expand_enum_to_int_function([], Line, Clause) ->
    {function,Line,enum_to_int,2,[Clause]};
expand_enum_to_int_function(Enums, Line, Clause) ->
    {function,Line,enum_to_int,2,[filter_enum_to_int_clause(Enum, Clause) || Enum <- Enums]}.

%% @hidden
filter_enum_to_int_clause({enum,EnumTypeName,IntValue,EnumValue}, {clause,L,_Args,Guards,_}) ->
    {clause,L,[{atom,L,atomize(EnumTypeName)},{atom,L,EnumValue}],Guards,[{integer,L,IntValue}]}.

%% @hidden
expand_int_to_enum_function([], Line, Clause) ->
    {function,Line,int_to_enum,2,[Clause]};
expand_int_to_enum_function(Enums, Line, Clause) ->
    {function,Line,int_to_enum,2, [filter_int_to_enum_clause(Enum, Clause) || Enum <- Enums] ++ [Clause]}.

%% @hidden
filter_int_to_enum_clause({enum,EnumTypeName,IntValue,EnumValue}, {clause,L,_Args,Guards,_}) ->
    {clause,L,[{atom,L,atomize(EnumTypeName)},{integer,L,IntValue}],Guards,[{atom,L,EnumValue}]}.

%% @hidden
%% [{"Location",
%%   [{2,required,"string","country",none},
%%    {1,required,"string","region",none}]},
%%  {"Person",
%%   [{5,optional,"Location","location",none},
%%    {4,required,"int32","age",none},
%%    {3,required,"string","phone_number",none},
%%    {2,required,"string","address",none},
%%    {1,required,"string","name",none}]}]
collect_full_messages(Data) -> collect_full_messages(Data, #collected{}).
collect_full_messages([{message, Name, Fields} | Tail], Collected) ->
    ListName = case erlang:is_list (hd(Name)) of
		   true -> Name;
		   false -> [Name]
	       end,
    
    FieldsOut = lists:foldl(
		  fun ({_,_,_,_,_} = Input, TmpAcc) -> [Input | TmpAcc];
		      (_, TmpAcc) -> TmpAcc
		  end, [], Fields),
    
    Enums = lists:foldl(
	      fun ({enum,C,D}, TmpAcc) -> [{enum, [C | ListName], D} | TmpAcc];
		  (_, TmpAcc) -> TmpAcc
	      end, [], Fields),
    
    Extensions = lists:foldl(
		   fun ({extensions, From, To}, TmpAcc) -> [{From,To}|TmpAcc];
		       (_, TmpAcc) -> TmpAcc
		   end, [], Fields),
			   
    SubMessages = lists:foldl(
		    fun ({message, C, D}, TmpAcc) -> [{message, [C | ListName], D} | TmpAcc];
			(_, TmpAcc) -> TmpAcc
		    end, [], Fields),

    NewCollected = Collected#collected{
		     msg=[{ListName, FieldsOut} | Collected#collected.msg],
		     extensions=[{ListName,Extensions} | Collected#collected.extensions]
		    },
    collect_full_messages(Tail ++ SubMessages ++ Enums, NewCollected);
collect_full_messages([{enum, Name, Fields} | Tail], Collected) ->
    ListName = case erlang:is_list (hd(Name)) of
		   true -> Name;
		   false -> [Name]
	       end,

    FieldsOut = lists:foldl(
		  fun (Field, TmpAcc) ->
			  case Field of
			      {EnumAtom, IntValue} -> [{enum, 
							type_path_to_type(ListName), 
							IntValue, 
							EnumAtom} | TmpAcc];
			      _ -> TmpAcc
			  end
		  end, [], Fields),
    
    NewCollected = Collected#collected{enum=FieldsOut++Collected#collected.enum},
    collect_full_messages(Tail, NewCollected);
collect_full_messages([{package, _PackageName} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{option,_,_} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{import, _Filename} | Tail], Collected) ->
    collect_full_messages(Tail, Collected);
collect_full_messages([{extend, Name, ExtendedFields} | Tail], Collected) ->
    ListName = case erlang:is_list (hd(Name)) of
		   true -> Name;
		   false -> [Name]
	       end,

    CollectedMsg = Collected#collected.msg,
    {ListName,FieldsOut} = lists:keyfind(ListName,1,CollectedMsg),
    {ListName,Extensions} = lists:keyfind(ListName,1,Collected#collected.extensions),
    
    FunNotInReservedRange = fun(Id) -> not(19000 =< Id andalso Id =< 19999) end,
    FunInRange = fun(Id,From,max) -> From =< Id andalso Id =< 16#1fffffff;
		    (Id,From,To) -> From =< Id andalso Id =< To
		 end,
    
    ExtendedFieldsOut = lists:append(FieldsOut,
			     lists:foldl(
			       fun ({Id, _, _, FieldName, _} = Input, TmpAcc) ->
				       case lists:any(fun({From,To}) -> FunNotInReservedRange(Id) 
									    andalso FunInRange(Id,From,To)
						      end,Extensions) of 
					   true ->
					       [Input | TmpAcc];
					   _ ->
					       error_logger:error_report(["Extended field not in valid range",
									  {message, Name},
									  {field_id,Id},
									  {field_name,FieldName},
									  {defined_ranges,Extensions},
									  {reserved_range,{19000,19999}},
									  {max,16#1fffffff}]),
					       throw(out_of_range)
				       end;
				   (_, TmpAcc) -> TmpAcc
			       end, [], ExtendedFields)
			     ),
    NewCollected = Collected#collected{msg=lists:keyreplace(ListName,1,CollectedMsg,{ListName,ExtendedFieldsOut})},
    collect_full_messages(Tail, NewCollected);
%% Skip anything we don't understand
collect_full_messages([_|Tail], Acc) ->
    collect_full_messages(Tail, Acc);
collect_full_messages([], Collected) ->
    Collected.

%% @hidden
resolve_types (Data, Enums) -> resolve_types (Data, Data, Enums, []).
resolve_types ([{TypePath, Fields} | Tail], AllPaths, Enums, Acc) ->
    FieldsOut = lists:foldl(
		  fun (Input, TmpAcc) ->
			  case Input of
			      {Index, Rules, Type, Identifier, Other} ->
				  case is_scalar_type (Type) of
				      true -> [Input | TmpAcc];
				      false ->
					  PossiblePaths =
					      case string:tokens (Type,".") of
						  [Type] ->
						      all_possible_type_paths (Type, TypePath);
						  FullPath ->
						% handle types of the form Foo.Bar which are absolute,
						% so we just convert to a type path and check it.
						      [lists:reverse (FullPath)]
					      end,
					  RealPath =
					      case find_type (PossiblePaths, AllPaths) of
						  false ->
						      case is_enum_type(Type, PossiblePaths, Enums) of
							  {true,EnumType} ->
							      EnumType;
							  false ->
							      throw (["Unknown Type ", Type])
						      end;
						  ResultType ->
						      ResultType
					      end,
					  [{Index, Rules, type_path_to_type (RealPath), Identifier, Other} | TmpAcc]
				  end;
			      _ -> TmpAcc
			  end
		  end, [], Fields),
    resolve_types (Tail, AllPaths, Enums, [{type_path_to_type (TypePath), lists:reverse (FieldsOut) } | Acc]);
resolve_types ([], _, _, Acc) ->
    Acc.

%% @hidden
write_header_include_file(Basename, Messages) ->
    {ok, FileRef} = file:open(Basename, [write]),
    [begin
	 OutFields = [{string:to_lower(A), Optional, Default} || {_, Optional, _, A, Default} <- lists:keysort(1, Fields)],
	 if
	     OutFields /= [] ->
		 io:format(FileRef, "-record(~s, {~n    ", [string:to_lower(Name)]),
		 WriteFields = generate_field_definitions(OutFields),
		 FormatString = string:join(["~s" || _ <- lists:seq(1, length(WriteFields))], ",~n    "),
		 io:format(FileRef, FormatString, WriteFields),
		 io:format(FileRef, "~n}).~n~n", []);
	     true ->
		 ok
	 end
     end || {Name, Fields} <- Messages],
    file:close(FileRef).

%% @hidden
generate_field_definitions(Fields) ->
    generate_field_definitions(Fields, []).

%% @hidden
generate_field_definitions([], Acc) ->
    lists:reverse(Acc);
generate_field_definitions([{Name, required, _} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = erlang:error({required, ~s})", [Name, Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, _, none} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s", [Name])),
    generate_field_definitions(Tail, [Head | Acc]);
generate_field_definitions([{Name, optional, Default} | Tail], Acc) ->
    Head = lists:flatten(io_lib:format("~s = ~p", [Name, Default])),
    generate_field_definitions(Tail, [Head | Acc]).

%% @hidden
atomize(String) ->
    list_to_atom(string:to_lower(String)).

%% @hidden
replace_atom(Find, Find, Replace) -> Replace;
replace_atom(Tuple, Find, Replace) when is_tuple(Tuple) ->
    list_to_tuple([replace_atom(Term, Find, Replace) || Term <- tuple_to_list(Tuple)]);
replace_atom(List, Find, Replace) when is_list(List) ->
    [replace_atom(Term, Find, Replace) || Term <- List];
replace_atom(Other, _Find, _Replace) ->
    Other.

%% @hidden
is_scalar_type ("double") -> true;
is_scalar_type ("float") -> true;
is_scalar_type ("int32") -> true;
is_scalar_type ("int64") -> true;
is_scalar_type ("uint32") -> true;
is_scalar_type ("uint64") -> true;
is_scalar_type ("sint32") -> true;
is_scalar_type ("sint64") -> true;
is_scalar_type ("fixed32") -> true;
is_scalar_type ("fixed64") -> true;
is_scalar_type ("sfixed32") -> true;
is_scalar_type ("sfixed64") -> true;
is_scalar_type ("bool") -> true;
is_scalar_type ("string") -> true;
is_scalar_type ("bytes") -> true;
is_scalar_type (_) -> false.

%% @hidden
is_enum_type(_Type, [], _Enums) ->
    false;
is_enum_type(Type, [TypePath|Paths], Enums) ->
    case is_enum_type(type_path_to_type(TypePath), Enums) of
      true ->
	{true,TypePath};
      false ->
	is_enum_type(Type, Paths, Enums)
    end.
is_enum_type(Type, Enums) ->
    case lists:keysearch(Type,2,Enums) of
	false ->
	    false;
	{value,_} ->
	    true
    end.

%% @hidden
sublists(List) when is_list(List) ->
    sublists(List,[]).
sublists([],Acc) ->
    [ [] | Acc ];
sublists(List,Acc) ->
    sublists (tl (List), [ List | Acc ]).

%% @hidden
all_possible_type_paths (Type, TypePath) ->
    lists:foldl (fun (TypeSuffix, AccIn) ->
			 [[Type | TypeSuffix] | AccIn]
		 end,
		 [],
		 sublists (TypePath)).

%% @hidden
find_type ([], _KnownTypes) ->
    false;
find_type ([Type | TailTypes], KnownTypes) ->
    case lists:keysearch (Type, 1, KnownTypes) of
	false ->
	    find_type (TailTypes, KnownTypes);
	{value, {RealType, _}} ->
	    RealType
    end.

%% @hidden
type_path_to_type (TypePath) ->
    string:join (lists:reverse (TypePath), "_").

