## 0.2.0 - 30 July, 2013

* Add `ekstat:read/6` and `ekstat:clear/1`.

* Rewrite `ekstat.c` based on [kstat comand line utility ported to C](https://github.com/illumos/illumos-gate/tree/861bfc8941f2c557f1bcb7a6d7f62f9da98ee4db/usr/src/cmd/stat/kstat) (see also illumos issue [#749](https://www.illumos.org/issues/749)).

* Atom `'_'` and list `"*"` are special types that will match anything in that column.

* Simple glob matching can be done with `*`, for example:

  ```erlang
  Data = ekstat:read(H, '_', '_', '_', '_', "freem*").
  Data = [{"pages","unix",0,"system_pages",[{"freemem",296009}]},
   {"vm","unix",0,"vminfo",[{"freemem",257625338}]}].
  ```

* Regular expressions are also supported in each column, for example:

  ```erlang
  Data = ekstat:read(H, '_', '_', '_', '_', "/freemem|updates/").
  Data = [{"misc","unix",0,"pset",[{"updates",78926}]},
   {"misc","unix",0,"sysinfo",[{"updates",790}]},
   {"pages","unix",0,"system_pages",[{"freemem",296009}]},
   {"vm","unix",0,"vminfo",
    [{"freemem",257625338},{"updates",790}]}].
  ```

* The following raw types are now available, which were not available before.
  * `misc:cpu_stat:*:*:*`
  * `misc:unix:*:var:*`
  * `misc:unix:*:ncstats:*`
  * `misc:unix:*:sysinfo:*`
  * `vm:unix:*:vminfo:*`
  * `misc:nfs:*:mntinfo:*`
  * `misc:unix:*:ncstats:*`
  * And some other SPARC specific stats (see `ekstat.h` for more details).
  * These stats are listed as `class:module:instance:name:statistic` which (for example with `vm:unix:*:vminfo:*`) would look like this:

    ```erlang
    Data = ekstat:read(H, "vm", "unix", "*", "vminfo").
    Data = [{"vm","unix",0,"vminfo",
      [{"crtime",0},
       {"freemem",257625338},
       {"snaptime",0},
       {"swap_alloc",36870808},
       {"swap_avail",659675142},
       {"swap_free",671578280},
       {"swap_resv",48773946},
       {"updates",790}]}].
    ```

* Timer and interrupt statistics are also available now (see [KSTAT(3KSTAT)](http://illumos.org/man/3kstat/kstat)).

## 0.1.0 - 22 March, 2013
* Initial release.