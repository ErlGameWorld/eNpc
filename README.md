# Erlang nif port compiler
    windows linux mac下erlang nif或者port_driver通用编译脚本
    改造自 erlang-native-compiler


## Usage
default_env
1. Clone this repository
1. Run `make` in this directory
1. Copy `eNpc` to your "path" environment variable
1. Add these (or similar) hooks to your rebar.config:

```erlang
{pre_hooks, [{"", compile, "eNpc compile"}]}.
{post_hooks, [{"", clean, "eNpc clean"}]}.
```

After that eNpc should read your old rebar.config `port_specs` and `port_env` settings as expected (it is rebar2's port compiler after all...).
