# Show generated odin code

Show generated code from compiling an odin model.

## Usage

``` r
odin_show(
  expr,
  input_type = NULL,
  compatibility = NULL,
  check_bounds = NULL,
  what = NULL
)
```

## Arguments

- expr:

  Odin code as the path to a file (a string), a character vector of
  code, or as an expression (typically within braces
  [`{}`](https://rdrr.io/r/base/Paren.html)).

- input_type:

  An optional string describing the type of input for `expr` - must be
  one of `file`, `text` or `expression`. If given, this skips the type
  detection logic and odin will throw an error if the wrong type of
  input is given. Using this may be beneficial in programmatic
  environments.

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

- what:

  Optional string, being a single method to show. Popular options are
  `update`, `rhs` and `compare_data`.

## Value

A character vector, with class `odin_code` that has a pretty-print
method defined. Returns `NULL` if `what` was given but the model lacks
this part.

## Examples

``` r
# Show generated code for the whole system
odin_show({
  initial(x) <- 1
  update(x) <- a
  a <- Normal(x, 1)
})
#> 
#> ── odin code: ──────────────────────────────────────────────────────────────────
#> #include <dust2/common.hpp>
#> // [[dust2::class(odin_system)]]
#> // [[dust2::time_type(discrete)]]
#> class odin_system {
#> public:
#>   odin_system() = delete;
#>   using real_type = double;
#>   using rng_state_type = monty::random::generator<real_type>;
#>   struct shared_state {
#>     struct odin_internals_type {
#>       struct {
#>         dust2::packing state;
#>       } packing;
#>       struct {
#>         std::array<size_t, 1> state;
#>       } offset;
#>     } odin;
#>   };
#>   struct internal_state {};
#>   using data_type = dust2::no_data;
#>   static dust2::packing packing_state(const shared_state& shared) {
#>     return shared.odin.packing.state;
#>   }
#>   static shared_state build_shared(cpp11::list parameters) {
#>     shared_state::odin_internals_type odin;
#>     odin.packing.state = dust2::packing{
#>       {"x", {}}
#>     };
#>     odin.packing.state.copy_offset(odin.offset.state.begin());
#>     return shared_state{odin};
#>   }
#>   static internal_state build_internal(const shared_state& shared) {
#>     return internal_state{};
#>   }
#>   static void update_shared(cpp11::list parameters, shared_state& shared) {
#>   }
#>   static void update_internal(const shared_state& shared, internal_state& internal) {
#>   }
#>   static void initial(real_type time, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state) {
#>     state[0] = 1;
#>   }
#>   static void update(real_type time, real_type dt, const real_type* state, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state_next) {
#>     const auto x = state[0];
#>     const real_type a = monty::random::normal<real_type>(rng_state, x, 1);
#>     state_next[0] = a;
#>   }
#> };

# Just the update method
odin_show({
  initial(x) <- 1
  update(x) <- a
  a <- Normal(x, 1)
}, what = "update")
#> 
#> ── odin code (update): ─────────────────────────────────────────────────────────
#> static void update(real_type time, real_type dt, const real_type* state, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state_next) {
#>   const auto x = state[0];
#>   const real_type a = monty::random::normal<real_type>(rng_state, x, 1);
#>   state_next[0] = a;
#> }
```
