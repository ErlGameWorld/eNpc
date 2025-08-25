# Erlang nif port compiler
    windows linux mac下erlang nif或者port_driver通用编译脚本
    改造自 erlang-native-compiler


## Usage
default_env
1. Clone this repository
1. Run `make` in this directory
1. Copy `eNpc` to your project "c_src" dir and commit it
1. Add these (or similar) hooks to your rebar.config:

```erlang
{pre_hooks, [{"", compile, "escript c_src/eNpc compile"}]}.
{post_hooks, [{"", clean, "escript c_src/eNpc clean"}]}.
```

After that eNpc should read your old rebar.config `port_specs` and `port_env` settings as expected (it is rebar2's port compiler after all...).

###
```
 Supported configuration variables:
 * port_specs - Erlang list of tuples of the forms
                {ArchRegex, TargetFile, Sources, Options}
                {ArchRegex, TargetFile, Sources}
                {TargetFile, Sources}

                Note that if you want to use any of the rebar3 variables
                below you must MUST use a ${}-style to get the expansion
                to work. e.g. to expand REBAR_DEPS_DIR, do something like:

                {port_specs, [{"priv/nif.so",
                               ["c_src/nif.c",
                                "${REBAR_DEPS_DIR}/foo/bar.c"]}]}.

                This is a _very_ good way to be able to use your code both
                as a top level app and a dependency.

                CAVEAT! Not using {} is broken for the moment.

 * port_env - Erlang list of key/value pairs which will control
              the environment when running the compiler and linker.
              Variables set in the surrounding system shell are taken
              into consideration when expanding port_env. Note that
              for ERL_LDFLAGS, -lerl_interface is used for only those
              Erlang/OTP versions where it exists (those prior to
              version 23.0).

              By default, the following variables are defined:
              CC       - C compiler
              CXX      - C++ compiler
              CFLAGS   - C compiler
              CXXFLAGS - C++ compiler
              LDFLAGS  - Link flags
              ERL_CFLAGS  - default -I paths for erts and ei
              ERL_LDFLAGS - default -L and -lerl_interface -lei
              DRV_CFLAGS  - flags that will be used for compiling
              DRV_LDFLAGS - flags that will be used for linking
              EXE_CFLAGS  - flags that will be used for compiling
              EXE_LDFLAGS - flags that will be used for linking
              ERL_EI_LIBDIR - ei library directory
              DRV_CXX_TEMPLATE      - C++ command template
              DRV_CC_TEMPLATE       - C command template
              DRV_LINK_TEMPLATE     - C Linker command template
              DRV_LINK_CXX_TEMPLATE - C++ Linker command template
              EXE_CXX_TEMPLATE      - C++ command template
              EXE_CC_TEMPLATE       - C command template
              EXE_LINK_TEMPLATE     - C Linker command template
              EXE_LINK_CXX_TEMPLATE - C++ Linker command template

              Note that if you wish to extend (vs. replace) these variables,
              you MUST include a shell-style reference in your definition.
              e.g. to extend CFLAGS, do something like:

              {port_env, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}

              It is also possible to specify platform specific options
              by specifying a triplet where the first string is a regex
              that is checked against Erlang's system architecture string.
              e.g. to specify a CFLAG that only applies to x86_64 on linux
              do:

              {port_env, [{"x86_64.*-linux", "CFLAGS",
                           "$CFLAGS -X86Options"}]}

              Cross-arch environment variables to configure toolchain:
              GET_ARCH to set the tool chain name to use
              GET_ARCH_WORDSIZE (optional - to determine word size)"
              word size is 32
              GET_ARCH_VSN (optional - "
              l version of CC/CXX is requested),


{port_specs, [
    {"priv/jiffy.so", [
        "c_src/*.c",
        "c_src/*.cc",
        "c_src/double-conversion/*.cc"
    ]}
]}.

{port_env, [
    % Drop -lerl_interface
    {"ERL_LDFLAGS", " -L$ERL_EI_LIBDIR -lei"},
    {"win32", "ERL_LDFLAGS", " /LIBPATH:$ERL_EI_LIBDIR ei.lib"},

    {".*", "FLTO_FLAG", ""},

    {"(linux|solaris|freebsd|netbsd|openbsd|dragonfly|darwin|gnu)", "CFLAGS", "$CFLAGS -Ic_src/ -g -Wall $FLTO_FLAG -Werror -O3"},
    {"(linux|solaris|freebsd|netbsd|openbsd|dragonfly|darwin|gnu)", "CXXFLAGS", "$CXXFLAGS -Ic_src/ -g -Wall $FLTO_FLAG -Werror -O3"},

    {"(linux|solaris|freebsd|netbsd|openbsd|dragonfly|darwin|gnu)", "LDFLAGS", "$LDFLAGS $FLTO_FLAG -lstdc++"},

    %% OS X Leopard flags for 64-bit
    {"darwin9.*-64$", "CXXFLAGS", "-m64"},
    {"darwin9.*-64$", "LDFLAGS", "-arch x86_64"},

    %% OS X Snow Leopard flags for 32-bit
    {"darwin10.*-32$", "CXXFLAGS", "-m32"},
    {"darwin10.*-32$", "LDFLAGS", "-arch i386"},

    {"win32", "CXXFLAGS", "$CXXFLAGS /O2 /DNDEBUG"}
]}.
```