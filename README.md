# ekstat
ekstat is a interface to the solaris libkstat, it started out as a port of [node-kstat](https://github.com/bcantrill/node-kstat) and later was rewritten using parts of the [C version of the kstat command line utility](https://github.com/illumos/illumos-gate/tree/861bfc8941f2c557f1bcb7a6d7f62f9da98ee4db/usr/src/cmd/stat/kstat).

## exports
The interface is rather simple it has 4 functions:

### `open/0`
Simply returns a new kstat handle.
```erlang
{ok, Handle} = ekstat:open().
```

### `update/1`
Creates a new snapshot for the handle.
```erlang
{ok, NumberOfInstancesUpdated} = ekstat:update(Handle).
```

### `clear/1`
Clears the snapshot for the handle.
```erlang
{ok, NumberOfInstancesCleared} = ekstat:clear(Handle).
```

### `read/1,2,3,4,5,6`
```erlang
read(Handle) -> Result
read(Handle, Class) -> Result
read(Handle, Class, Module) -> Result
read(Handle, Class, Module, Instance) -> Result
read(Handle, Class, Module, Instance, Name) -> Result
read(Handle, Class, Module, Instance, Name, Statistic) -> Result

  kstat_pattern = '_' | integer() | string()

  Handle = ekstat:kstat_handle()
  Class = kstat_pattern()
  Module = kstat_pattern()
  Instance = kstat_pattern()
  Name = kstat_pattern()
  Statistic = kstat_pattern()

  kstat_statistic = {Key::string(), Value::integer()} | {Key::string(), Value::string()}

  Result = [{Class::string(), Module::string(), Instance::integer(), Name::string(), [kstat_statistic()]}]
```

Reads data from the snapshot (possibly selectively) and returns it:

```erlang
Class = "misc",
Module = "unix",
Instance = 1,
Name = "callout",
Statistic = "callout_timeouts",
Data = ekstat:read(Handle, Class, Module, Instance, Name, Statistic).
Data = [{"misc","unix",1,"callout",[{"callout_timeouts",21286}]}].
```

Atom `'_'` and list `"*"` are special types that will match anything in that column.

Simple glob matching can be done with `*`, for example:

```erlang
Data = ekstat:read(H, '_', '_', '_', '_', "freem*").
Data = [{"pages","unix",0,"system_pages",[{"freemem",296009}]},
 {"vm","unix",0,"vminfo",[{"freemem",257625338}]}].
```

Regular expressions are also supported in each column, for example:

```erlang
Data = ekstat:read(H, '_', '_', '_', '_', "/freemem|updates/").
Data = [{"misc","unix",0,"pset",[{"updates",78926}]},
 {"misc","unix",0,"sysinfo",[{"updates",790}]},
 {"pages","unix",0,"system_pages",[{"freemem",296009}]},
 {"vm","unix",0,"vminfo",
  [{"freemem",257625338},{"updates",790}]}].
```

Invalid regular expressions will return an error:

```erlang
Error = ekstat:read(Handle, "/*/").
Error = {error,{kstat,"repetition-operator operand invalid"}}.
```
