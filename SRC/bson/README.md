This is the Bson implementation for Erlang. Bson is a record-like data type with a standard binary representation defined at <http://www.bsonspec.org>. This implements version 1.0 of that spec. The standard binary form allows for easy data interchange between systems. In particular, [MongoDB](http://www.mongodb.org) uses it for exchanging data between the MongoDB server and its clients.

The root Bson data type is `bson:document()`. Conceptually, it is a list of name-value pairs, analogous to an associative array, dictionary, or record. However, in this implementation, for writability and readability, the list of pairs is flattened, (ie. the tuples for each pair are elided), and the list is actually a tuple to distinguish it from list (array) of values. So a document is a tuple with alternating name and value elements, where a name is an `atom()` and a value is a `bson:value()`, which includes basic types like `boolean()`, `number()`, `atom()`, `bson:utf8()` (string), and compound types like `[bson:value()]` and `bson:document()`. For example,

	> Doc = {x,<<"abc">>, y,[1,2,3], z,{a,'Foo', b,4.2}}.

is a document with three fields: `{x,<<"abc">>}` and `{y,[1,2,3]}`, and `{z,{a,'Foo', b,4.2}}`. There is a function `bson:fields` that converts a document to a list of fields but normally you don't need it. Instead you should use the following operations on documents: `bson:lookup`, `bson:at`, `bson:include`, `bson:exclude`, `bson:update`, `bson:merge`, and `bson:append`.

	> {[1,2,3]} = bson:lookup (y, Doc).
	> {} = bson:lookup (w, Doc).
	> [1,2,3] = bson:at (y, Doc). % error if missing
	> {x,<<"abc">>, y,[1,2,3]} = bson:include ([x, y], Doc).
	> {z,{a,'Foo', b,4.2}} = bson:exclude ([x, y], Doc).
	> {x,<<"abc">>, y,[1,2,3], z,null} = bson:update (z, null, Doc).
	> {x,<<"abc">>, y,[1,2,3], z,null, w,1} = bson:merge ({w,1, z,null}, Doc).
	> {w,1, x,<<"abc">>, y,[1,2,3], z,{a,'Foo', b,4.2}} = bson:append ({w,1}, Doc).

For the full list of `bson:value()` types see the [bson module](http://github.com/TonyGen/bson-erlang/blob/master/src/bson.erl). Notice that an Erlang `string()` will be interpreted as a list of integers, so remember to alway delimit your literal strings with binary brackets (eg. `<<"abc">>`) and convert string variables using `bson:utf8`. You may be tempted to use atoms instead of strings, but you should only use atoms for enumerated types.

There are some special `bson:value()` types like `bson:javascript()` that are tagged tuples, eg. `{javascript, {x,1}, <<"function (y) {return y + x}">>}`. But embedded documents are also tuples, so how do we distinguish between the two? The answer is the `bson:value()` types that are tagged tuples are purposely defined to have an odd number of elements to distinguish them from documents which have an even number of elements.
