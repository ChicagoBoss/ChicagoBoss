# Contributing to Chicago Boss
Zachary Kessin
Dec 30 2013

If you would like to contribute to Chicago Boss or the related
projects including boss_db, tiny_pq and so on then we welcome your
contributions. However in order to keep our code quality as high as
we can we ask that everyone read this guide before sending pull
requests. (And if you don't get it perfect we won't bite your head
off)

## Simple Pull Requests and named branches

Please keep your pull requests simple, if you want to make changes
to the mongo db driver and add a driver for couch db do the first on a
*mongo* branch and the second on a *couch* branch, and then send 2
pull requests.

## Make it pass Dialyzer

I run dialyzer on everything, if it breaks it probably means that I
have more work to do, so try to make it pass.

## Tests are required
I have been adding tests with proper to boss_db and they will show up
in boss sooner or later. While historically most of the code does not
have any sort of tests we need to start adding it. Unit tests are
good, property based tests are better.

## No deeply nested code
If your code has more than 1 level of nested case, fun or foldl, It
needs to be re factored. Really keep the functions short.

## Documentation is required
Please use EDoc (http://www.erlang.org/doc/apps/edoc/chapter.html) 
compatible comments for your functions and modules. In-code comments 
are welcome too. 
