# nolint start
method_args <- list(
  packing_state = "static dust2::packing packing_state(const shared_state& shared) {",
  packing_gradient = "static dust2::packing packing_gradient(const shared_state& shared) {",
  build_shared = "static shared_state build_shared(cpp11::list parameters) {",
  build_data = "static data_type build_data(cpp11::list data, const shared_state& shared) {",
  initial_discrete = "static void initial(real_type time, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state) {",
  initial_continuous = "static void initial(real_type time, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state) {",
  update_shared = "static void update_shared(cpp11::list parameters, shared_state& shared) {",
  build_internal = "static internal_state build_internal(const shared_state& shared) {",
  update_internal = "static void update_internal(const shared_state& shared, internal_state& internal) {",
  update = "static void update(real_type time, real_type dt, const real_type* state, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state_next) {",
  rhs = "static void rhs(real_type time, const real_type* state, const shared_state& shared, internal_state& internal, real_type* state_deriv) {",
  rhs_delays = "static void rhs(real_type time, const real_type* state, const shared_state& shared, internal_state& internal, const dust2::ode::delay_result_type<real_type>& delays, real_type* state_deriv) {",
  output = "static void output(real_type time, real_type* state, const shared_state& shared, internal_state& internal) {",
  output_delays = "static void output(real_type time, real_type* state, const shared_state& shared, internal_state& internal, const dust2::ode::delay_result_type<real_type>& delays) {",
  size_output = "static size_t size_output() {",
  compare_data = "static real_type compare_data(real_type time, const real_type* state, const data_type& data, const shared_state& shared, internal_state& internal, rng_state_type& rng_state) {",
  delays = "static auto delays(const shared_state& shared) {",
  zero_every = "static auto zero_every(const shared_state& shared) {",
  adjoint_update = "static void adjoint_update(real_type time, real_type dt, const real_type* state, const real_type* adjoint, const shared_state& shared, internal_state& internal, real_type* adjoint_next) {",
  adjoint_compare_data = "static void adjoint_compare_data(real_type time, const real_type* state, const real_type* adjoint, const data_type& data, const shared_state& shared, internal_state& internal, real_type* adjoint_next) {",
  adjoint_initial = "static void adjoint_initial(real_type time, const real_type* state, const real_type* adjoint, const shared_state& shared, internal_state& internal, real_type* adjoint_next) {")
# nolint end


test_struct_offset <- function(state) {
  c("  struct offset_type {",
    "    struct {",
    sprintf("      size_t %s;", state),
    "    } state;",
    "  } offset;")
}

test_pkg_setup <- function(path, name = "pkg") {
  dir.create(file.path(path, "inst/odin"), FALSE, TRUE)
  writeLines(c(
    paste("Package:", name),
    "LinkingTo: cpp11, dust2, monty, odin2",
    "Imports: dust2",
    "Version: 0.0.1",
    "Authors@R: c(person('A', 'Person', role = c('aut', 'cre'),",
    "    email = 'person@example.com'))",
    "Title: An Example",
    "Description: Example of odin for testing.",
    "License: CC0"),
    file.path(path, "DESCRIPTION"))
  writeLines(sprintf("useDynLib(%s, .registration = TRUE)", name),
             file.path(path, "NAMESPACE"))
}
