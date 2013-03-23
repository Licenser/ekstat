# ekstat
ekstat is a interface to the solaris libkstat, it is pretty much a port of [node-kstat](https://github.com/bcantrill/node-kstat).

## interface
The interface is rather simple it has 3 functions:

### open/0
Simply returns a new kstat handle.
```erlang
{ok, Handle} = ekstat:open().
```

### update/1
Creates a new snapshot for the handle.
```erlang
ekstat:update(Handle).
```

### read/1,2,3,4,5
Reads data (possibly selectively) and returns it:
```erlang
Class = "misc",
Module = "unix",
Instance = 1,
Name = "callout".
Data = read(Handle, Class, Module, Instance, Name).
Data = [{"misc","unix",1,"callout",
  [{"callout_cleanups",1},
   {"callout_allocations",25},
   {"callout_expirations",1281196},
   {"callout_untimeouts_expired",245},
   {"callout_untimeouts_executing",0},
   {"callout_untimeouts_unexpired",14136},
   {"callout_timeouts_pending",10},
   {"callout_timeouts",1295342}]}]
```