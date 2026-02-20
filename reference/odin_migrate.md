# Migrate odin code

Migrate odin code. This function takes a path to existing odin code and
writes out migrated code to a new file. It is possible that no code will
be migrated, in which case the written contents will be identical to
those read.

## Usage

``` r
odin_migrate(path, dest)
```

## Arguments

- path:

  Path of the odin code to read

- dest:

  Path of the destination code. It can be the same as `dest`, in which
  case the file will be overwritten.

## Value

Nothing; called for side effects only

## Examples

``` r
# A file 'path' contains odin code using old features:
writeLines(readLines(path))
#> initial(x) <- 0
#> deriv(x) <- x * r
#> r <- user()

# Migrate this file in place (by overwriting)
odin_migrate(path, path)
#> ℹ Migrating 1 statement
#> ✔ Wrote '/tmp/RtmpIBVk7h/file1b4d2e0110ed.R'

writeLines(readLines(path))
#> initial(x) <- 0
#> deriv(x) <- x * r
#> r <- parameter()
```
