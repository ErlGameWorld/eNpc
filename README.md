# Erlang nif port compiler
    windows linux mac下erlang nif或者port_driver通用编译脚本
    改造自 erlang-native-compiler


## Usage
default_env
1. Clone this repository
1. Run `make` in this directory
1. Copy `erlNpc` to your project "c_src" dir and commit it
1. Add these (or similar) hooks to your rebar.config:

```erlang
{pre_hooks, [{"", compile, "escript c_src/erlNpc compile"}]}.
{post_hooks, [{"", clean, "escript c_src/erlNpc clean"}]}.
```

After that enc should read your old rebar.config `port\_specs` and `port\_env` settings as expected (it is rebar2's port compiler after all...).