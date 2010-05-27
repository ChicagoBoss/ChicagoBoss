%% Author: dave
%% Created: Feb 25, 2010
%% Description: Bridge between erlydtl compiler and gettext server 
-module(erlydtl_i18n).

%%
%% Include files
%%
%% Exported Functions
%%
-export([translate/2]).

%%
%% API Functions
%%
%% Makes i18n conversion using gettext
translate(String, Locale) -> gettext:key2str(String, Locale).
