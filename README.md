# Erlang-Elixir UUID Integration Example

This repository demonstrates how to use Elixir's UUID library within an Erlang application using the `rebar_mix` plugin.

## Overview

The project shows how to:
1. Set up an Erlang application with Elixir dependency management
2. Call Elixir's UUID functions from Erlang code
3. Generate different UUID formats and retrieve UUID information

## Prerequisites

- Erlang/OTP 23 or newer
- Elixir 1.11 or newer
- rebar3

## Setup Steps

### 1. Configure rebar.config

```erlang
{erl_opts, [debug_info]}.

{plugins, [
    {rebar_mix, "0.4.0"}
]}.

{provider_hooks, [
    {pre, [{compile, {mix, find_elixir_libs}}]},
    {post, [{compile, {mix, consolidate_protocols}}]}
]}.

{deps, [
    {uuid, "1.1.8"}
]}.

{shell, [
    {apps, [rebar_elixir_uuid_demo]}
]}.
```

### 2. Create application module

```erlang
-module(rebar_elixir_uuid_demo_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([generate_uuid/0, generate_binary_uuid/0, generate_uuid_with_info/0]).

start(_StartType, _StartArgs) ->
    io:format("Application rebar_elixir_uuid_demo started~n"),
    io:format("Example UUID v4: ~p~n", [generate_uuid()]),
    io:format("Example UUID v4 binary: ~p~n", [generate_binary_uuid()]),
    io:format("Example UUID v1 with info: ~p~n", [generate_uuid_with_info()]),
    {ok, self()}.

stop(_State) ->
    ok.

generate_uuid() ->
    'Elixir.UUID':uuid4().

generate_binary_uuid() ->
    'Elixir.UUID':uuid4(default).

generate_uuid_with_info() ->
    UUID = 'Elixir.UUID':uuid1(),
    'Elixir.UUID':info(UUID).
```

### 3. Create application resource file

Create `src/rebar_elixir_uuid_demo.app.src`:

```erlang
{application, rebar_elixir_uuid_demo, [
    {description, "Demo of using Elixir UUID in Erlang"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, {rebar_elixir_uuid_demo_app, []}},
    {applications, [
        kernel,
        stdlib,
        elixir
    ]},
    {env, []},
    {modules, []},
    {licenses, ["MIT"]},
    {links, []}
]}.
```

## Building and Running

1. Compile the project:
   ```
   $ rebar3 compile
   ```

2. Start the application in a shell:
   ```
   $ rebar3 shell
   ```

You should see output similar to:
```
Application rebar_elixir_uuid_demo started
Example UUID v4: "f47ac10b-58cc-4372-a567-0e02b2c3d479"
Example UUID v4 binary: <<244,122,193,11,88,204,67,114,165,103,14,2,178,195,212,121>>
Example UUID v1 with info: #{binary => <<107,236,113,70,235,0,17,232,160,81,0,31,112,251,242,20>>,
                             type => :uuid1,
                             uuid => "6be9714f-eb00-11e8-a051-001f70fbf214",
                             version => 1}
```

## Available Functions

The example demonstrates three main UUID functions:

1. `generate_uuid/0` - Generates a v4 UUID string
2. `generate_binary_uuid/0` - Generates a v4 UUID in binary format
3. `generate_uuid_with_info/0` - Generates a v1 UUID and returns info about it

## How it Works

The `rebar_mix` plugin allows your Erlang project to use Elixir libraries. During compilation:

1. The plugin finds and installs the Elixir dependencies specified in your rebar.config
2. It compiles these dependencies alongside your Erlang code
3. It makes the Elixir modules available to your Erlang application

To call an Elixir module from Erlang, prefix the module name with `'Elixir.'`. For example:
```erlang
'Elixir.UUID':uuid4()  % Calls the uuid4/0 function in Elixir's UUID module
```

## Extending the Example

You can explore other UUID functions by adding new exported functions. For example:

```erlang
generate_v5_uuid() ->
    Namespace = 'Elixir.UUID':uuid4(),
    Name = "test",
    'Elixir.UUID':uuid5(Namespace, Name).
```

