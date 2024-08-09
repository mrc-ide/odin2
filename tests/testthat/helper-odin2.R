# nolint start
method_args <- list(
  size_state = "static size_t size_state(const shared_state& shared) {",
  build_shared = "static shared_state build_shared(cpp11::list parameters) {",
  build_data = "static data_type build_data(cpp11::list data) {",
  initial_discrete = "static void initial(real_type time, real_type dt, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state) {",
  initial_continuous = "static void initial(real_type time, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state) {",
  update_shared = "static void update_shared(cpp11::list parameters, shared_state& shared) {",
  build_internal = "static internal_state build_internal(const shared_state& shared) {",
  update_internal = "static void update_internal(const shared_state& shared, internal_state& internal) {",
  update = "static void update(real_type time, real_type dt, const real_type* state, const shared_state& shared, internal_state& internal, rng_state_type& rng_state, real_type* state_next) {",
  rhs = "static void rhs(real_type time, const real_type* state, const shared_state& shared, internal_state& internal, real_type* state_deriv) {",
  compare_data = "static real_type compare_data(real_type time, real_type dt, const real_type* state, const data_type& data, const shared_state& shared, internal_state& internal, rng_state_type& rng_state) {",
  zero_every = "static auto zero_every(const shared_state& shared) {",
  adjoint_size = "static size_t adjoint_size(const shared_state& shared) {",
  adjoint_update = "static void adjoint_update(real_type time, real_type dt, const real_type* state, const real_type* adjoint, const shared_state& shared, internal_state& internal, real_type* adjoint_next) {",
  adjoint_compare_data = "static void adjoint_compare_data(real_type time, real_type dt, const real_type* state, const real_type* adjoint, const data_type& data, const shared_state& shared, internal_state& internal, real_type* adjoint_next) {",
  adjoint_initial = "static void adjoint_initial(real_type time, real_type dt, const real_type* state, const real_type* adjoint, const shared_state& shared, internal_state& internal, real_type* adjoint_next) {")
# nolint end
