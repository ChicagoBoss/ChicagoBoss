%% Copyright (c) 2008 Luke Galea www.ideaforge.org

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% Inflector - v0.1

%% @author Luke Galea <luke@ideaforge.org>
%% @copyright 2008 Luke Galea.

%% @doc A string inflection and general convenience library inspired by Ruby/Rails's ActiveSupport Inflector. Converts strings from plural to singular, etc.

-module(inflector).
-author('Luke Galea <luke@ideaforge.org>').
-export([pluralize/1, singularize/1, camelize/1, lower_camelize/1, titleize/1, 
	 capitalize/1, humanize/1, underscore/1, dasherize/1, tableize/1, moduleize/1,
	 foreign_key/1, ordinalize/1, cached_re/2]).

-include_lib("eunit/include/eunit.hrl").

%% External API
singularize(Word) ->
    pluralize_or_singularize( Word, singulars() ).

pluralize(Word) ->
    pluralize_or_singularize( Word, plurals() ).

camelize(LowerCaseAndUnderscoredWord) ->
    lists:flatten( 
      lists:map( 
	fun ([L|Rest]) -> [ string:to_upper(L) | Rest ] end, 
	underscore_tokens( LowerCaseAndUnderscoredWord ) ) ).

lower_camelize(LowerCaseAndUnderscoredWord) ->
    [First|Rest] = underscore_tokens( LowerCaseAndUnderscoredWord ),
    First ++ camelize( lists:flatten(Rest) ).

%% Capitalizes every word
titleize(WordOrSentence) ->
    string:join( lists:map( fun capitalize/1, string:tokens( WordOrSentence, "_ " ) ), " " ).

%% Capitalizes the first letter, lower cases the rest
capitalize([F|Rest]) ->
    [string:to_upper(F) | string:to_lower(Rest)].

%% Capitalizes the first word and turns underscores into spaces
humanize(Word) ->
    [First|Rest] = token_words(Word),
    string:join( [capitalize(First) | Rest], " ").

underscore(CamelCasedWord) ->
    RE1 = re_compile("([A-Z]+)([A-Z][a-z])"),
    RE2 = re_compile("([a-z\\d])([A-Z])"),
    string:to_lower( 
      re_replace( 
	re_replace( CamelCasedWord, RE1, "\\1_\\2" ), 
	RE2, "\\1_\\2" ) ).

dasherize(UnderscoredWord) ->
    lists:map( fun ($_) -> $-; (C) -> C end, UnderscoredWord ).

tableize(ModuleName) ->
    pluralize( underscore( ModuleName ) ).

foreign_key(ClassName) ->
    underscore(ClassName) ++ "_id".

moduleize(TableName) ->
    camelize(singularize(TableName)).


ordinalize(N) ->
    lists:flatten( ord(N) ).

ord(N) when (N rem 100 >= 11) and (N rem 100 =< 13) -> io_lib:format("~Bth", N);
ord(N) when (N rem 10) =:= 1 -> io_lib:format("~Bst", [N]);
ord(N) when (N rem 10) =:= 2 -> io_lib:format("~Bnd", [N]);
ord(N) when (N rem 10) =:= 3 -> io_lib:format("~Brd", [N]);
ord(N) -> io_lib:format("~Bth", [N]).
    

%% Helpers		       
re_compile( RE ) ->
    { ok, Compiled } = cached_re( RE, [] ),
    Compiled.

re_replace( In, RE, Out ) ->
    re:replace( In, RE, Out, [{return, list}, global] ).

underscore_tokens(S) ->
    string:tokens( S, "_" ).

token_words(S) ->
    string:tokens( S, "_ " ).

pluralize_or_singularize( Word, List ) ->
    case is_uncountable(Word) of
	true -> Word;
	false -> replace(Word, List )
    end.

is_uncountable(Word) ->
    lists:member(Word, uncountables()).

replace(Word, [] ) ->
    Word;
replace(Word, [ {Regex, Replacement} | Remainder ] ) ->
    { ok, RE } = cached_re(Regex, [caseless]),
    case re:run( Word, RE ) of
	{ match, _ } ->
	    re_replace( Word, RE, Replacement );
	nomatch ->
	    replace(Word, Remainder)
    end.

%% Cached Regular Expressions
cached_re( RE, Options ) ->
    CachePid = re_cache(),
    CachePid ! { get, self(), RE, Options },
    receive
	{ CachePid, CompiledRE } -> 
	    CompiledRE
    end.

re_cache() ->
    case whereis( re_cache ) of
	undefined -> 
	    Pid = spawn_link( fun() -> re_cache_loop( ets:new(cached_regexps,[]) ) end ),
	    register( re_cache, Pid ),
	    Pid;
	Pid -> Pid 
    end.

re_cache_loop( CachedREs ) ->
    receive
	{ get, Caller, RE, Options } ->
	    Caller ! { self(), re_find_or_compile( CachedREs, RE, Options ) },
	    re_cache_loop( CachedREs )
    end.

re_find_or_compile( CachedREs, RE, Options ) ->
    case ets:lookup( CachedREs, { RE, Options } ) of
	[] ->
	    CompiledRE = re:compile( RE, Options ), 
	    true = ets:insert( CachedREs, { { RE, Options }, CompiledRE } ),
	    CompiledRE;
	[ { { RE, Options }, StoredRE } ] -> StoredRE
    end.

%% Rules
plurals() ->
    irregulars() ++
    [ {"(quiz)$",               "\\1zes"  },
      {"^(ox)$",                "\\1en"   },
      {"(quiz)$",               "\\1zes"  },
      {"^(ox)$",                "\\1en"   },
      {"([m|l])ouse$",          "\\1ice"  },
      {"(matr|vert|ind)ix|ex$", "\\1ices" },
      {"(x|ch|ss|sh)$",         "\\1es"   },
      {"([^aeiouy]|qu)y$",      "\\1ies"  },
      {"(hive)$",               "\\1s"    },
      {"(?:([^f])fe|([lr])f)$", "\\1\\1ves" },
      {"sis$",                  "ses"   },
      {"([ti])um$",             "\\1a"    },
      {"(buffal|tomat)o$",      "\\1oes"  },
      {"(bu)s$",                "\\1ses"  },
      {"(alias|status)$",       "\\1es"   },
      {"(octop|vir)us$",        "\\1i"    },
      {"(ax|test)is$",          "\\1es"   },
      {"s$",                    "s"     },
      {"$",                     "s"     } ].

singulars() ->    
    [ {"(quiz)zes$",            "\\1"     },
      {"(matr)ices$",           "\\1ix"   },
      {"(vert|ind)ices$",       "\\1ex"   },
      {"(ox)en",                "\\1"     },
      {"(alias|status)es$",     "\\1"     },
      {"(octop|vir)i$",         "\\1us"   },
      {"(cris|ax|test)es$",     "\\1is"   },
      {"(shoe)s$",               "\\1"     },
      {"(o)es$",                 "\\1"     },
      {"(bus)es$",               "\\1"     },
      {"([m|l])ice$",            "\\1ouse" },
      {"(x|ch|ss|sh)es$",        "\\1"     },
      {"(m)ovies$",              "\\1ovie" },
      {"(s)eries$",              "\\1eries"},
      {"([^aeiouy]|qu)ies$",     "\\1y"    },
      {"([lr])ves$",             "\\1f"    },
      {"(tive)s$",               "\\1"     },
      {"(hive)s$",               "\\1"     },
      %{"([^f])ves$",             "\\1fe"   },
      {"(^analy)ses$",           "\\1sis"  },
      {"((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$", "\\1\\2sis"},
      {"([ti])a$",               "\\1um"   },
      {"(n)ews$",                "\\1ews"  },
      {"s$",                     ""      } ] ++ reversed_irregulars().

irregulars() ->
    [{"move",   "moves"   },
     {"sex",    "sexes"   },
     {"child",  "children"},
     {"man",    "men"     },
     {"person", "people"  }].

reversed_irregulars() ->
    F = fun ({A, B}) -> {B, A} end,
    lists:map(F, irregulars()).

uncountables() ->
    ["sheep",
     "fish",
     "series",
     "species",
     "money",
     "rice",
     "information",
     "equipment" ].


%% Tests
replace_test() ->
    SampleList = [ {"abc", "def"},
		   {"a(b|c)d", "e\\1e"},
		   {"howdy", "doody"} ],
    "I know my defs" = replace( "I know my abcs", SampleList ),
    "Nothing changed" = replace( "Nothing changed", SampleList ),
    "Blah ebe" = replace("Blah abd", SampleList ),
    "Howdy ece" = replace("Howdy acd", SampleList ),
    "doody ho" = replace("howdy ho", SampleList ),
    "doody" = replace("howdy", SampleList ).

singularize_test() ->
    "dog" = singularize("dogs"),
    "mouse" = singularize("mice"),
    "bus" = singularize("buses"),
    "sex" = singularize("sexes"),
    "Sex" = singularize("Sexes"),
    "sheep" = singularize("sheep"),
    "child" = singularize("children"),
    "dog" = singularize("dog").
    
pluralize_test() ->
    "dogs" = pluralize("dog"),
    "dogs" = pluralize("dogs"),
    "buses" = pluralize("bus"),
    "sexes" = pluralize("sex"),
    "sheep" = pluralize("sheep"),
    "children" = pluralize("child").

camelize_test() ->
    "CamelCase" = camelize("camel_case").

lower_camelize_test() ->
    "camelCase" = lower_camelize("camel_case").

humanize_test() ->
    "Employee salary" = humanize("employee_salary").

titleize_test() ->
    "Army Of Darkness" = titleize("army of darkness"),
    "Army Of Darkness" = titleize("army_of_darkness").

capitalize_test() ->
    "This" = capitalize("this"),
    "This" = capitalize("tHiS"),
    "This" = capitalize("THIS").

underscore_test() ->
    "this_is_a_test" = underscore("ThisIsATest").

dasherize_test() ->
    "this-has-dashes-now" = dasherize("this_has_dashes_now").

tableize_test() ->
    "raw_scaled_scorers" = tableize("RawScaledScorer"),
    "egg_and_hams" = tableize("egg_and_ham"),
    "fancy_categories" = tableize("fancyCategory").

moduleize_test() ->
    "FancyCategory" = moduleize("fancy_categories"),
    "FancyCategory" = moduleize("fancy_category").

ordinalize_test() ->
    "1st" = ordinalize(1),
    "2nd" = ordinalize(2),
    "1002nd" = ordinalize(1002),
    "4th" = ordinalize(4),
    "104th" = ordinalize(104).

foreign_key_test() ->
    "message_id" = foreign_key("Message").

cached_re_test() ->
    {ok, RE1} = cached_re("Abcdefg", []),
    {ok, RE2} = cached_re("yuuuu", []),
    {ok, RE1_1} = cached_re("Abcdefg", []),
    true = (RE1 =:= RE1_1),
    false = (RE2 =:= RE1), 
    "QQQ?UUU" == re_replace( "QQQAbcdefgUUU", RE1_1, "?" ).
