Using LFE with Chicago Boss
==

Lisp-Flavoured Erlang is a Lisp-y language for the Erlang VM, which you're
welcome to use if you like both parentheses and metaprogramming. More info on
the LFE project is here: http://lfe.github.io/

Setup
--

LFE is included with Chicago Boss. You don't need to do anything special except
give your source files the extension ".lfe" instead of ".erl".

Controller API
--

The controller API is pretty much the same as the CB Erlang API, but note that
LFE does not support the auto-routing magic where URL parameters are inferred
from the controller arguments.

Example controller module:

    (defmodule (myapp_cool_controller req session_id)
        (export all))

    (defun index (http_method tokens)
        (tuple 'output '"Yeee-haw!"))

...then visit /cool/index for a special greeting.

Model Files
--

LFE is not supported in BossDB model files (src/model).

Library Files
--

LFE is supported for library files sitting in your project's src/lib/
directory.
