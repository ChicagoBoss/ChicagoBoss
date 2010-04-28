%%%-------------------------------------------------------------------
%%% File    : etcher.erl
%%% Project : Etcher (http://github.com/jinsky/etcher)
%%% Author  : Rory Byrne <rory [at] jinsky [dot] com>
%%% License : BSD
%%%
%%% Copyright (c) 2010 Rory Byrne
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   * Redistributions of source code must retain the above copyright
%%%     notice, this list of conditions and the following disclaimer.
%%%
%%%   * Redistributions in binary form must reproduce the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer in the documentation and/or other materials provided
%%%     with the distribution.
%%%
%%%   * Neither the names of the copyright holders, nor the names of its
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(etcher).
-export([compile/1,
         compile/2,
         add_record_defs/1,
         % add_record_defs/2,
         render/2,
         render/3
         ]).

-include("internal.hrl").

%% @type template() = term().
%%       You can treat a template as an opaque object. If you are interested 
%%       in what it contains, take a look at the <code>include/api.hrl</code>
%%       file. Note that each template is unique, so even if you compile the 
%%       same code twice, it will result in two different templates.
%% @type chardata() = charlist() | unicode_binary().
%% @type charlist() = [unicode_char() | unicode_binary() | charlist()].
%%       A unicode_binary is allowed as the tail of the list.
%% @type custom_filter() = #filter_def{}.
%%       A custom_filter() is a record of type <code>filter_def</code>. You
%%        can find the record definition of <code>filter_def</code> in
%%        <code>include/api.hrl</code>.
%% @type custom_tag() = #tag_def{}.
%%       A custom_tag() is a record of type <code>tag_def</code>. You can 
%%       find the record definition of <code>tag_def</code> in 
%%       <code>include/api.hrl</code>.
%% @type context() = term().
%%       At it's top-most level, a context is always a proplist, but below 
%%       that it can contain nested records, lists and proplists. For example,
%%       this is what a context for a blog page might look like:
%%       <pre>
%%        [{blog_title, "Freds Blog"},
%%         {post, #blog_post{
%%                    title="Getting Ahead by Failing",
%%                    content="And another thing ...",
%%                    comments=[
%%                        #comment{
%%                            author="Sam",
%%                            email="sam@example.com",
%%                            says="You said ..."},
%%                        #comment{
%%                            author="Bob",
%%                            email="bob@example.com",
%%                            says="Always the ..."}]}},
%%         {page_hits, 8143}]
%%       </pre>
%%       If you are using nested records, as this example does, you will need to
%%       inform the <code>etcher</code> application about these records in 
%%       advance so it can recognise them and auto-expand them. You tell 
%%       <code>etcher</code> about record definitions using 
%%       {@link add_record_defs/1} below.
%% @type template_loader() = {Mod, Fun, Args} | {file, TemplatedDirs}.
%%       Allows you to specify a way to load templates (either at 
%%       compile-time or at render-time). Currently there is only one 
%%       built in loader - {@link etcher_loader:file_loader/2}. The 
%%       option <code>{file, TemplateDirs}</code> is a short-cut 
%%       notation for the file loader and it just gets translated to 
%%       <code>{etcher_loader, file_loader, TemplateDirs}</code>.
%%
%%       You can use your own template loaders. The function you 
%%       specify will need to accept exactly 2 arguments: (1) A file path
%%       argument; (2) Whatever <code>Args</code> were specified. That's right, 
%%       you receive the <code>Args</code> as a list. So, if you store 
%%       your templates in CouchDB and you might write a function to 
%%       retrieve them:
%%       <pre>
%%           couchdb_load_template(FilePath, [DbName]) ->
%%               case couch_find_doc(FilePath, DbName) of
%%                   {ok, Template} ->
%%                       Template;
%%                   not_found ->
%%                       undefined
%%               end.
%%       </pre>
%%       So, you use the <code>FilePath</code> as a key. If you are 
%%       successful, you return the <code>Template</code>. If you are 
%%       not successful, you return <code>undefined</code>. Then, to use 
%%       this loader, you provide the argument:
%%       <pre>
%%           {template_loaders, [{my_module, couchdb_load_template, [DbName]}]}
%%       </pre>
%%       to either {@link compile/2} or {@link render/3}.
%% @type template_path() = filepath().
%%       This is a relative filepath to a template. This filepath will is used
%%       by a template_loader() to locate the template (usually as a file). The
%%       template object that is located can be in source or compiled form - it
%%       will be compiled on the fly if required.
%% @end

%% @spec compile(Source::Source) -> {ok, template()}
%%       Source = chardata()
%% @doc
%% Calls <code>compile(Source, [])</code>.
%% @end
compile(Source) ->
    compile(Source, []).

%% @spec compile(Source::Source, Options::Options) -> {ok, template()}
%%       Source = chardata()
%%       Options = [ Option ]
%%       Option = {filters, [custom_filter()]} | {tags, [custom_tag()]} | 
%%                {template_loaders, [template_loader()]}
%% @doc
%% Compiles unicode/UTF-8-encoded template source code into a 
%% template. You can do what you want with this term - store it 
%% in an mneisa database, call <code>erlang:term_to_binary/1</code>
%% on it and write it to a file.
%%
%% Most template inclusion happens at runtime, but the 'include' tag 
%% will include templates at compile-time if the template path is 
%% specified as a string literal, like
%% "<code>{% include 'articles/123.txt' %}</code>". To find templates
%% at compile-time, you must provide a <code>template_loaders</code>
%% option. It works exactly the same as the <code>template_loaders</code>
%% option provided to {@link render/3}.
%% @end
compile(Source, Options) when is_list(Options) ->
    etcher_compiler:compile(Source, Options).

%% @spec add_record_defs(RecDefs::RecDefs) -> ok
%%       RecDefs = filename() | [{record_name(), [record_field()]}]
%% @doc
%% Use this function to inform the <code>etcher</code> application
%% about records you would like to use in rendering contexts. 
%% 
%% The easiest way to inform <code>etcher</code> about your record 
%% defintions is to pass in the name of a <code>.hrl</code> file 
%% containing those definitions. 
%%
%% You can also pass in a list of {record_name(), [record_fields()]} 
%% if it's more convenient.
%%
%% Note that there is currently no way to get <code>etcher</code> to 
%% forget about record definitions it's already been informed about. 
%% This functionality will be added later.
%% @end
add_record_defs(T) ->
    etcher_rec_resolver:add_record_defs(T).

% TODO - re-enable this
% add_record_defs(T, Namespace) ->
%     etcher_rec_resolver:add_record_defs(T, Namespace).

%% @spec render(Template::Template, Context::Context) -> chardata()
%%       Template = template() | template_path()
%%       Context = context()
%% @doc
%% Calls <code>render(Template, Context, [])</code>. 
%% @end
render(Template, Context) -> 
    render(Template, Context, []).

%% @spec render(Template::Template, Context::Context, Options::Options) -> RenderedContent
%%       Template = template() | template_path()
%%       Context = context()
%%       Options = [ Option ]
%%       Option = {return, list} | {return, binary} | {auto_escape, bool()} | 
%%                {url_mapper, function()} | {allowed_include_roots, PathList} |
%%                {template_loaders, [template_loader()]} | 
%%                {compiler_opts, list()}
%%       PathList = [filename()]
%%       RenderedContent = chardata() | unicode_binary() | list()
%% @doc
%% Renders the Template using the Context you provide.
%%
%% The returned content defaults to chardata(), but using the return 
%% option you can get a binary() (<code>{return, binary}</code>) or a 
%% list() (<code>{return, list}</code>).
%%
%% If the <code>Template</code> argument is a template_path(), you will
%% need to include a <code>template_loaders</code> option so that the
%% template file (it's usually a file) can be located and loaded.
%%
%% Auto-escape is on by default, as it is with Django. You can use 
%% use the <code>{auto_escape, false}</code> option to turn it off.
%% You'll certainly want to do this if you are  generating 
%% non-XML/non-HTML documents, such as emails.
%%
%% The <code>url_mapper</code> option is specifically for use with
%% the <code>{% url ... %}</code> tag. Django has it's own way of 
%% using this tag which ties in with the rest of it's framework.
%% We obviously don't have a 'rest of a framework' so we'll have 
%% to do something a little different. If you pass in a 
%% <code>fun()</code> of Arity 1 as a <code>url_mapper</code>,
%% then when the <code>url</code> tag is used, it's parameters 
%% will be passed to your fun as a list(). The parameter list 
%% will contain either one, or two parameters. If the tag looks 
%% like <code>{% url required_param arg1,arg2,name1=value1 %}</code>,
%% then your UrlMapper fun() will receive 
%% <code>["required_param", "arg1,arg2,name1=value1"]</code>.
%% You are then responsible for returning a URL string. Note that 
%% no checking, or URL-encoding, will be done on the URL string 
%% you return. So, you need to make sure you get it right. There's
%% a couple of url encoding functions exported from 
%% {@link etcher_util} that you can use if you need to.
%%
%% <code>allowed_include_roots</code> is for the {@link etcher_std_tags:tag_ssi/2. ssi} 
%% standard tag (and any custom tag that might want to use it). It
%% contains a list of absolute path prefixes for safe paths that 
%% ssi is allowed to use. It serves the same purpose as Django's
%% <a target="_blank" href="http://docs.djangoproject.com/en/1.1/ref/settings/#setting-ALLOWED_INCLUDE_ROOTS">ALLOWED_INCLUDE_ROOTS</a>.
%% By default <code>allowed_include_roots</code> is an empty 
%% list <code>[]</code>. 
%% 
%% Note, if you choose to use <code>allowed_include_roots</code>
%% in custom tags, make sure to normalize the filepath before 
%% checking against these prefixes - prefix checking alone won't save 
%% you from paths like <code>"/opt/safe/../../etc/passwd"</code>. 
%% You can use {@link etcher_util:normalize_absolute_path/1} for this. 
%%
%% Use the <code>template_loaders</code> option to tell etcher how to 
%% find included/extended templates. Most template inclusion is handled 
%% at render-time rather than at compile time, and you will certainly 
%% need to provide this option if you are using template inheritence.
%%
%% The <code>compiler_opts</code> option is used when templates are 
%% compiled on the fly during rendering. This happens more than you 
%% might think. For example, if you reference templates as a filename
%% in an <code>'extends'</code> tag, then the file will be compiled 
%% at render-time (unless it was pre-compiled and saved as a 
%% <code>term_to_binary</code> entity). The <code>compiler_opts</code>
%% setting allows you to specifiy any of the options used in 
%% {@link compile/2} and they will be used when compiling. One exception
%% is the <code>template_loaders</code> option. You should specify
%% this as an ordinary render option, and it will be used at compile
%% time if it's needed.
%% @end
render(_Template, Context, _Options) when not is_list(Context) ->
    throw({invalid_context_arg, list_expected});
render(_Template, _Context, Options) when not is_list(Options) ->
    throw({invalid_options_arg, list_expected});
render(Template, Context, Options) ->
    {RetType, RenderOpts} = get_option(return, Options, chardata),
    RecResolver = etcher_rec_resolver:get_record_resolver(),
    RS = etcher_renderer:new(Context, RecResolver, RenderOpts),
    Tpl = 
        case Template of
            #etcher_template{} ->
                Template;
            TemplatePath when ?IS_STRING(TemplatePath) ->
                load_template(RS, TemplatePath);
            _ ->
                throw({template_not_recognised, Template})
        end,
    {_RS1, RenderedData} = etcher_renderer:render_template(RS, Tpl),
    return_as(RetType, RenderedData).

%%------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------

get_option(Name, List, Default) ->
    extract(List, Name, Default, []).

extract([{Name, Value} | Rest], Name, _Current, Acc) ->
    extract(Rest, Name, Value, Acc);
extract([T | Rest], Name, Current, Acc) ->
    extract(Rest, Name, Current, [T | Acc]);
extract([], _Name, Current, Acc) ->
    {Current, lists:reverse(Acc)}.
 
return_as(chardata, CharData) ->
    CharData;
return_as(binary, CharData) ->
    unicode:characters_to_binary(CharData);
return_as(list, CharData) ->
    unicode:characters_to_list(CharData).

load_template(RS, TemplatePath) ->
    case etcher_loader:get_template(RS, TemplatePath) of
        #etcher_template{} = Tpl ->
            Tpl;
        undefined ->
            throw({failed_to_load_template, TemplatePath})
    end.

