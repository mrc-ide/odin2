# Update odin code in package

Update generated code in a package that uses odin and dust to provide a
model. This will generate new dust code in `inst/dust` and from that
generate a full model in `src`, and an R interface in `R/dust.R`, along
with the cpp11 attributes that are needed to use the model.

## Usage

``` r
odin_package(path, quiet = FALSE, compatibility = NULL, check_bounds = NULL)
```

## Arguments

- path:

  Path to the package root (the directory that contains `DESCRIPTION`),
  or any path within that package.

- quiet:

  Logical, indicating if compilation messages from `pkgbuild` should be
  displayed. Error messages will be displayed on compilation failure
  regardless of the value used. If `NULL` is given, then we take the
  value from `DUST_QUIET` if set, or `FALSE` otherwise.

- compatibility:

  Compatibility mode to use. Valid options are "warning", which updates
  code that can be fixed, with warnings, and "error", which will error.
  The option "silent" will silently rewrite code, but this is not
  recommended for general use as eventually the compatibility mode will
  be removed (this option is primarily intended for comparing output of
  odin1 and odin2 models against old code). The default, `NULL`,
  currently corresponds to `warning`.

- check_bounds:

  Control over static array bounds checking. This is enabled by default,
  but is prone to false positives, erroring where a read or write
  appears out of bounds but is actually ok. This argument exists to
  allow you to disable the check and compile the model anyway. Future
  versions may allow specific lines to be ignored, which will provide
  finer control and allow you to use the bits of the checks that are
  actually helpful. You can also pass `TRUE` here to mean "error" or
  `FALSE` to mean "disabled".

## Value

Invisibly, the path to the package. However, this function is typically
called for its side effect of updating files in `inst/dust` and `src`
within this package after you have changed the odin code in `inst/odin`.

## Details

This function is powered by
[dust2::dust_package](https://mrc-ide.github.io/dust2/reference/dust_package.html),
and the same pre-requisites apply here:

For your `DESCRIPTION` file:

- `dust2` must be in `Imports`

- `cpp11`, `dust2` and `monty` must be in `LinkingTo`

For your `NAMESPACE` file:

- you must have a suitable `useDynLib()` call with
  `.registration = TRUE`

If you do not satisfy these requirements,
[`dust2::dust_package`](https://mrc-ide.github.io/dust2/reference/dust_package.html)
will fail with a message indicating actions you should take. Once set
up, generally things will keep working.

If you want your packages to build on GitHub actions, or be installable
via `remotes::install_github` you should add to your `DESCRIPTION`:

    Remotes: mrc-ide/dust2, mrc-ide/monty

Note that you do not need to include odin2 itself as a dependency.

## See also

[`vignette("packaging")`](https://mrc-ide.github.io/odin2/articles/packaging.md)
for more details, and
[`dust2::dust_package()`](https://mrc-ide.github.io/dust2/reference/dust_package.html),
which does most of the work here.

## Examples

``` r
# An example package structure
fs::dir_tree(path)
#> /tmp/RtmpIBVk7h/file1b4dda5e018
#> ├── DESCRIPTION
#> ├── NAMESPACE
#> └── inst
#>     └── odin
#>         └── sir.R

# Generate odin code:
odin_package(path)
#> ℹ Found 1 odin code file in 'inst/odin'
#> ✔ Wrote 'inst/dust/sir.cpp'
#> ℹ Working in package 'example' at '/tmp/RtmpIBVk7h/file1b4dda5e018'
#> ℹ Found 1 system
#> ✔ Wrote 'src/sir.cpp'
#> ✔ Wrote 'R/dust.R'
#> ✔ Wrote 'src/Makevars'
#> ℹ 12 functions decorated with [[cpp11::register]]
#> ✔ generated file cpp11.R
#> ✔ generated file cpp11.cpp

# Resulting files:
fs::dir_tree(path)
#> /tmp/RtmpIBVk7h/file1b4dda5e018
#> ├── DESCRIPTION
#> ├── NAMESPACE
#> ├── R
#> │   ├── cpp11.R
#> │   └── dust.R
#> ├── inst
#> │   ├── dust
#> │   │   └── sir.cpp
#> │   └── odin
#> │       └── sir.R
#> └── src
#>     ├── Makevars
#>     ├── cpp11.cpp
#>     └── sir.cpp
```
