# Filters

The functionality of Chicago Boss can be extended with filter modules. Filters
are ideal for implementing site-wide or controller-specific features, such as
authentication. Filters can be chained together and reused, which makes them
ideal for implementing and distributing plug-in features. Chicago Boss uses
filters internally to implement caching.

CB supports three kinds of filters: before filters, middle filters, and
after filters. A filter module can implement multiple kinds of filters as
described below.


## Before filters

Before filters can either transform an incoming request before it is handled
by a controller action, or it can short-circuit the request processing and
return an action return value itself. Before filters are ideal for implementing
authentication and authorization; the filter can attach the identity of the
logged-in user to the incoming request, and immediately redirect unidentified or 
unauthorized users.

A before filter should export a function `before_filter/2`:

    before_filter(FilterConfig, RequestContext) -> {ok, RequestContext} | <controller return value>

The first argument is the filter's configuration value (see "[Filter
configuration](#filter_configuration)" below). This value can be defined in the configuration file and
overridden by controllers on a per-request basis.

The second argument is the request context. The request context is a proplist
with the following keys:

* `request`
* `session_id`
* `action`
* `controller_module`
* `tokens`

If the `before_filter` function returns `{ok, NewContext}`, the `NewContext` will be
used for the rest of the request. You are free to modify values in the context
or insert new values (e.g. `logged_in_user`).

Example:

    -module(my_before_filter).
    -export([before_filter/2]).

    before_filter(_Config, RequestContext) ->
        IsAdmin = is_admin(RequestContext),
        {ok, [{is_admin, IsAdmin}|RequestContext]}.


## Middle filters

A middle filter transforms a controller return value to another controller
return value. Because controllers can return `{StatusCode, Payload, Headers}`,
you can also use it to implement custom return values. 

A middle filter should export a function `middle_filter/3`:

    middle_filter(ReturnValue, FilterConfig, RequestContext) -> NewReturnValue

For example, if you wanted to implement a file handler `{file, PathToFile}`:

    -module(my_middle_filter).
    -export([middle_filter/3]).

    middle_filter({file, PathToFile}, _Config, _RequestContext) ->
        FileContents = read_file_somehow(PathToFile),
        FileMIMEType = figure_out_mime_type(PathToFile),
        {200, FileContents, [{"Content-Type", FileMIMEType}]};

    middle_filter(Other, _, _) -> Other.

You might also use middle filters to insert commonly used values into
the variable list before template rendering.


## After filters

An after filter transforms a `{StatusCode, Payload, Headers}` tuple just
before a response is returned to the client:

    after_filter({StatusCode, Payload, Headers}, FilterConfig, RequestContext) -> {NewStatusCode, NewPayload, NewHeaders}

You might use it to implement a custom compression or caching scheme.


## <a name="filter_installation"></a>Filter installation

Filter module can be installed with the `controller_filter_modules` config
option:

    {controller_filter_modules, [my_awesome_filter1, my_awesome_filter2]}

Filters are applied in order. For a particular controller, you can override the
default filter list with the following three functions:

    -module(my_awesome_controller, [Req, SessionID]).
    -export([before_filters/2, middle_filters/2, after_filters/2]).

    before_filters(DefaultFilters, RequestContext) -> BeforeFilters
    middle_filters(DefaultFilters, RequestContext) -> MiddleFilters
    after_filters(DefaultFilters, RequestContext) -> AfterFilters

That way you can rearrange, insert, or delete filters based on the current
request.

For now just put filter modules into your project's "lib" directory.


## <a name="filter_configuration"></a>Filter configuration

The `FilterConfig` argument passed to the filter functions is set in your
boss.config and can be overridden by the controllers.

To set a default config value for `my_awesome_filter` in your boss.config:

    {boss, [
        {controller_filter_config, [
            {my_awesome_filter, [{awesomeness, 100}]}
            ]}
        ]}

Then to override the value for a given request, export a `config/2` function
from your controller:

    -module(my_cool_controller, [Req, SessionId]).
    -export([config/2]).

    config(my_awesome_filter, _DefaultValue, RequestContext) ->
        [{awesomeness, 200}].

The first argument is the name of the filter. It will either be the name of
filter module itself or a short name provided by the filter. Short names can
be shared by multiple filters, in which case they will receive the same config
value.


## Setting a short name and default config value

Filter modules can export two functions to set a short name for themselves and
to provide a default config value:

    -module(my_awesome_filter).
    -export([config_key/0, config_default_value/0]).

    config_key() -> 'awesome'.
    config_default_value() -> [{awesomeness, 50}].


## Accessing the request context

If a controller action function takes three arguments, the request context proplist
will be passed in as the third argument.

## Build in filters

CB has 4 build in filters:

 - `boss_lang_filter`
 - `boss_cache_page_filter`
 - `boss_cache_vars_filter`
 - `boss_csrf_filter`

First 3 are enabled by default, CSRF verification filter is not
enabled.

## `boss_csrf_filter`

### Installing CSRF verification filter

CSRF verification isn't enabled by default,
please read [Filter Installation](#filter_installation) to enabled it.

### Using CSRF verification

* Templates: you can use `{{ csrf_token }}` variable to display hidden
  input with csrf token in it.
* Controllers: `csrf_token` string is passed into controllers as part
  of `RequestContext` proplist (third parameter passed into controller)

### Disabling CSRF verification

Add `{do_not_enforce_csrf_checks, true}` tuple into filter config ([see
above](#filter_configuration)).
CSRF verification can be disabled application wide or on per
controller basis.
