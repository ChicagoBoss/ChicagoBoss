-module({{appid}}_custom_tags).
-compile(export_all).

% put custom tags in here, e.g.
%
% reverse(Variables, Options) ->
%     lists:reverse(binary_to_list(proplists:get_value(string, Variables))).
%
% {% reverse string="hello" %} => "olleh"
%
% Variables are the passed-in vars in your template
% Options is a proplist from CB containing:
% - application (application name, an atom)
% - controller (current controller name, an atom)
% - action (current action name, an atom)
% - router_pid (Pid of the router process, mainly used by the "url" tag)
