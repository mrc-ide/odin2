# Details

This vignette collects together other details for odin that we might
want to link from elsewhere.

## Order of events

This is only an issue for discrete-time models, as for a continuous time
model it is always “now”.

Consider discrete time model that compares to data. There will be some
series of “updates” then a comparison to data, then we repeat until we
reach the end of the data. The order of events as we move from
`time = t0` to to `time = t0 + dt` is:

**Update**

1.  Reset any variables that use `zero_every`
2.  Read from variables
3.  Look up interpolation (using `t0`)
4.  Evaluate all assignments (therefore with `time = t0` and all
    variables having the value at the beginning of the step)
5.  Write out new values of state
6.  Update `time` to `t0 + dt`

**Compare**

1.  Read from variables
2.  Look up interpolation (using `t0 + dt`)
3.  Compare to data

## Options

You can control the *default* behaviour of some odin functions by
setting options. All options are prefixed with `odin2.` (this will
change to `odin.` once we start actively replacing the old version). To
set an option for a session, you can use

``` r
options(odin2.whatever = TRUE)
```

If you want to make this permanent, you can add this line to your
`~/.Rprofile` file. The easiest way to edit that file is to run
`usethis::edit_r_profile()` which will find the correct file and open it
in your editor (e.g., RStudio, if you are using that).

### `odin2.compatibility`

The default behaviour of the odin1 to odin2 compatibility checks. If
set, this should be a string suitable to pass to the `compatibility`
argument of
[`odin2::odin`](https://mrc-ide.github.io/odin2/reference/odin.md) (one
of `warning`, `error` or `silent`); see the help for
[`odin2::odin`](https://mrc-ide.github.io/odin2/reference/odin.md) for a
description of the possible options.

### `odin2.check_bounds`

The default behaviour of odin2 array bounds checks. If set, this should
be a string suitable to pass to the `check_bounds` argument of
[`odin2::odin`](https://mrc-ide.github.io/odin2/reference/odin.md) (one
of `error`, `warning` or `disable`); see the help for
[`odin2::odin`](https://mrc-ide.github.io/odin2/reference/odin.md) for a
description of the possible options.

### `odin2.target`

The default value for the `target` argument to
[`odin2::odin`](https://mrc-ide.github.io/odin2/reference/odin.md). If
set, this should be one of `cpp` or `js` (or, in future, `cuda`).
