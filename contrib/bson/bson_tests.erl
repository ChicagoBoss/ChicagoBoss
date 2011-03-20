-module(bson_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

bson_test() ->
	Doc = {b, {x, 2, y, 3},
		   a, 1,
		   c, [mon, tue, wed]},
	{1} = bson:lookup (a, Doc),
	{} = bson:lookup (d, Doc),
	1 = bson:at (a, Doc),
	{'EXIT', {missing_field, _}} = (catch bson:at (d, Doc)),
	{a, 1} = bson:include ([a], Doc),
	{a, 1} = bson:exclude ([b,c], Doc),
	{b, {x, 2, y, 3}, a, 1, c, 4.2} = bson:update (c, 4.2, Doc),
	{b, 0, a, 1, c, 2, d, 3} = bson:merge ({c, 2, d, 3, b, 0}, Doc),
	{a, 1, b, 2, c, 3, d, 4} = bson:append ({a, 1, b, 2}, {c, 3, d, 4}),
	[{b, {x, 2, y, 3}}, {a, 1}, {c, [mon, tue, wed]}] = bson:fields (Doc).

time_test() ->
	{MegaSecs, Secs, _} = bson:timenow(),
	{MegaSecs, Secs, 0} = bson:secs_to_unixtime (bson:unixtime_to_secs ({MegaSecs, Secs, 0})).

objectid_test() ->
	{<<1:32/big, 2:24/big, 3:16/big, 4:24/big>>} = bson:objectid (1, <<2:24/big, 3:16/big>>, 4),
	UnixSecs = bson:unixtime_to_secs (bson:timenow()),
	UnixTime = bson:objectid_time (bson:objectid (UnixSecs, <<2:24/big, 3:16/big>>, 4)),
	UnixSecs = bson:unixtime_to_secs (UnixTime).

binary_test() ->
	Doc = {'BSON', [<<"awesome">>, 5.05, 1986]},
	Bin = bson_binary:put_document (Doc),
	Bin = <<49,0,0,0,4,66,83,79,78,0,38,0,0,0,2,48,0,8,0,0,0,97,119,101,115,111,109,101,0,1,49,0,51,51,51,51,51,51,20,64,16,50,0,194,7,0,0,0,0>>,
	VBin = <<200,12,240,129,100,90,56,198,34,0,0>>,
	Time = bson:timenow(),
	Doc1 = {a, -4.230845,
			b, <<"hello">>,
			c, {x, -1, y, 2.2001},
			d, [23, 45, 200],
			eeeeeeeee, {bin, bin, VBin},
			f, {bin, function, VBin},
			g, {bin, uuid, Bin},
			h, {bin, md5, VBin},
			i, {bin, userdefined, Bin},
			j, bson:objectid (bson:unixtime_to_secs (Time), <<2:24/big, 3:16/big>>, 4),
			k1, false,
			k2, true,
			l, Time,
			m, null,
			n, {regex, <<"foo">>, <<"bar">>},
			o1, {javascript, {}, <<"function(x) = x + 1;">>},
			o2, {javascript, {x, 0, y, <<"foo">>}, <<"function(a) = a + x">>},
			p, atom,
			q1, -2000444000,
			q2, -8000111000222001,
			r, {mongostamp, 100022, 995332003},
			s1, 'MIN_KEY',
			s2, 'MAX_KEY'},
	Bin1 = bson_binary:put_document (Doc1),
	{Doc1, <<>>} = bson_binary:get_document (Bin1).
