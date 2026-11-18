# nolint start
method_args <- list(
  build_params = "buildParams(imports, params) {",
  build_internal = "buildInternal(imports, params) {",
  initial = "initial(imports, time, params, internal, state) {",
  update = "update(imports, time, dt, state, params, internal, stateNext, random) {",
  rhs = "rhs(imports, params, internal, time, state, stateDeriv) {",
  output = "output(imports, params, internal, time, state) {"
)
# nolint end

test_that("generate params and internals for trivial system", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {};",
      "},"))
})


test_that("can generate build shared with calculations", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- b
    a <- parameter()
    b <- a * 2
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const a = imports.io.readReal(params, "a");',
      "  const b = a * 2;",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.b = b;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("generate trivial discrete time initial conditions", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_initial(dat),
    c(method_args$initial,
      "  state[0] = 1;",
      "},"))
})


test_that("generate trivial continuous time initial conditions", {
  dat <- odin_parse({
    initial(x) <- 1
    deriv(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_initial(dat),
    c(method_args$initial,
      "  state[0] = 1;",
      "},"))
})


test_that("can generate simple update method", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  stateNext[0] = 1;",
      "},"))
  expect_null(generate_js_system_rhs(dat))
})


test_that("can generate simple deriv method", {
  dat <- odin_parse({
    initial(x) <- 1
    deriv(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_rhs(dat),
    c(method_args$rhs,
      "  stateDeriv[0] = 1;",
      "},"))
  expect_null(generate_dust_system_update(dat))
})


test_that("generate stack equations during update", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + a
    a <- 1 / x
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x');",
      "  const a = 1 / x;",
      "  stateNext[0] = x + a;",
      "},"))
})


test_that("can look up lhs for bits of data", {
  packing <- parse_packing(c("x", "y", "b", "z"), NULL, NULL, "state")
  dat <- list(storage = list(
                location = c(a = "stack",
                             b = "state",
                             c = "outerspace"),
                type = c("a" = "real", b = "real", c = "real"),
                packing = list(state = packing)))
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_lhs(list(name = "a"), dat, "state", list()),
    "const a")
  expect_equal(
    generate_js_lhs(list(name = "b"), dat, "state", list()),
    "state[2]")
  expect_error(
    generate_js_lhs(list(name = "c"), dat, "state", list()),
    "Unsupported location")
})


test_that("can generate simple stochastic system", {
  dat <- odin_parse({
    update(x) <- Normal(x, 1)
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x');",
      "  stateNext[0] = random.normal(x, 1);",
      "},"))
})


test_that("generate defaults for parameters", {
  dat <- odin_parse({
    a <- parameter(2)
    update(x) <- x + a
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const a = imports.io.readReal(params, "a", 2);',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate models with commonly used mathematical functions", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- c
    a <- log(x)
    b <- ceiling(a)
    c <- a^b
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x');",
      "  const a = Math.log(x);",
      "  const b = Math.ceil(a);",
      "  const c = Math.pow(a, b);",
      "  stateNext[0] = c;",
      "},"))
})


test_that("can generate a simple array equation", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    a[] <- Normal(0, 1)
    dim(a) <- 2
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.a.dim[0]; i++) {",
      "    internal.a[i - 1] = random.normal(0, 1);",
      "  }",
      "  stateNext[0] = internal.a[0] + internal.a[1];",
      "},"))
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {",
      "    a: Array(params.dim.a.size).fill(0),",
      "  };",
      "},"))
})


test_that("can generate a simple array within shared", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2] + a[3]
    a[] <- i
    dim(a) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  stateNext[0] = params.a[0] + params.a[1] + params.a[2];",
      "},"))

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [3];",
      "  const a = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    a[i - 1] = i;",
      "  }",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  return { ...params, dim, odin };",
      "},"))

  ## internal state empty despite having arrays:
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {};",
      "},"))
})


test_that("can generate non-range access to arrays", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1]
    a[1] <- Normal(0, 1)
    dim(a) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  internal.a[0] = random.normal(0, 1);",
      "  stateNext[0] = internal.a[0];",
      "},"))
})


test_that("can generate stochastic initial conditions", {
  dat <- odin_parse({
    a0 <- Poisson(N / 100)
    N <- parameter(1000)
    initial(a) <- a0
    initial(b) <- N - a0
    update(a) <- a
    update(b) <- b
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const N = imports.io.readReal(params, \"N\", 1000);",
      "  const shape = new Map([",
      '    ["a", []],',
      '    ["b", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.N = N;",
      "  return { ...params, dim, odin };",
      "},"))

  expect_equal(
    generate_js_system_initial(dat),
    c(method_args$initial,
      "  const a0 = random.poisson(params.N / 100);",
      "  state[0] = a0;",
      "  state[1] = params.N - a0;",
      "},"))
})


test_that("can generate system with array variable", {
  dat <- odin_parse({
    initial(x[]) <- 0
    update(x[]) <- x[i]
    initial(y) <- 0
    update(y) <- y
    dim(x) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.x = [3];",
      "  const shape = new Map([",
      '    ["x", dim.x.dim],',
      '    ["y", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x').data;",
      "  const y = unpacked.get('y');",
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    stateNext[i - 1 + 0] = x[i - 1];",
      "  }",
      "  stateNext[3] = y;",
      "},"))
  expect_equal(
    generate_js_system_initial(dat),
    c(method_args$initial,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    state[i - 1 + 0] = 0;",
      "  }",
      "  state[3] = 0;",
      "},"))
})


test_that("can generate system with array from user", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1] + a[2]
    dim(a) <- 2
    a <- parameter()
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [2];",
      '  const a = imports.io.readRealArray(params, "a", dim.a);',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate system with aliased array", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1] + b[1]
    dim(a) <- 1
    dim(b) <- dim(a)
    a[] <- 1
    b[] <- 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [1];",
      "  const a = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    a[i - 1] = 1;",
      "  }",
      "  const b = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    b[i - 1] = 2;",
      "  }",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.b = b;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate system with aliased arrays dimmed together", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1] + b[1]
    dim(a, b) <- 1
    a[] <- 1
    b[] <- 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [1];",
      "  const a = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    a[i - 1] = 1;",
      "  }",
      "  const b = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    b[i - 1] = 2;",
      "  }",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.b = b;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate system with 2 aliased arrays dimmed together", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1] + b[1] + c[1]
    dim(c, a, b) <- 1
    a[] <- 1
    b[] <- 2
    c[] <- 3
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.c = [1];",
      "  const a = Array(dim.c.size).fill(0);",
      "  for (let i = 1; i <= dim.c.dim[0]; i++) {",
      "    a[i - 1] = 1;",
      "  }",
      "  const b = Array(dim.c.size).fill(0);",
      "  for (let i = 1; i <= dim.c.dim[0]; i++) {",
      "    b[i - 1] = 2;",
      "  }",
      "  const c = Array(dim.c.size).fill(0);",
      "  for (let i = 1; i <= dim.c.dim[0]; i++) {",
      "    c[i - 1] = 3;",
      "  }",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.b = b;",
      "  params.c = c;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate system with dependent-arrays dimmed together", {
  dat <- odin_parse({
    update(x) <- sum(a) + sum(b) + sum(c)
    initial(x) <- 0
    dim(a) <- 1
    dim(b, c) <- dim(a)
    a[] <- 1
    b[] <- 2
    c[] <- 3
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [1];",
      "  const a = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    a[i - 1] = 1;",
      "  }",
      "  const c = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    c[i - 1] = 3;",
      "  }",
      "  const b = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    b[i - 1] = 2;",
      "  }",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.c = c;",
      "  params.b = b;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate system with length and sum of aliased array", {
  dat <- odin_parse({
    update(x) <- sb + lb + a[1]
    initial(x) <- 0
    dim(a) <- 5
    dim(b) <- dim(a)
    lb <- length(b)
    sb <- sum(b)
    a[] <- 1
    b[] <- 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [5];",
      "  const a = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    a[i - 1] = 1;",
      "  }",
      "  const lb = dim.a.size;",
      "  const b = Array(dim.a.size).fill(0);",
      "  for (let i = 1; i <= dim.a.dim[0]; i++) {",
      "    b[i - 1] = 2;",
      "  }",
      "  const sb = imports.array.sumAll(b);",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.lb = lb;",
      "  params.b = b;",
      "  params.sb = sb;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("Can read parameters into var with aliased dimension", {
  dat <- odin_parse({
    update(x[]) <- x[i] + a
    initial(x[]) <- x0[i]

    dim(x) <- n_x
    dim(x0) <- dim(x)
    x0 <- parameter()
    n_x <- parameter()
    a <- parameter()
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const n_x = imports.io.readInt(params, "n_x");',
      '  const a = imports.io.readReal(params, "a");',
      "  dim.x = [n_x];",
      '  const x0 = imports.io.readRealArray(params, "x0", dim.x);',
      "  const shape = new Map([",
      '    ["x", dim.x.dim]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n_x = n_x;",
      "  params.a = a;",
      "  params.x0 = x0;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate system with simple variable sized array", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    n <- 2
    a[] <- Normal(0, 1)
    dim(a) <- n
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const n = 2;",
      "  dim.a = [n];",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {",
      "    a: Array(params.dim.a.size).fill(0),",
      "  };",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.a.dim[0]; i++) {",
      "    internal.a[i - 1] = random.normal(0, 1);",
      "  }",
      "  stateNext[0] = internal.a[0] + internal.a[1];",
      "},"))
})


test_that("can store arrays in state", {
  dat <- odin_parse({
    n <- 2
    initial(a[]) <- 1
    update(a[]) <- a[i] + 1
    dim(a) <- n
    initial(b[]) <- 1
    update(b[]) <- b[i] + 2
    dim(b) <- n + 1
    initial(c[]) <- 1
    update(c[]) <- c[i] + 3
    dim(c) <- n + 2
    initial(d[]) <- 1
    update(d[]) <- d[i] + 4
    dim(d) <- n + 3
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const n = 2;",
      "  dim.a = [n];",
      "  dim.b = [n + 1];",
      "  dim.c = [n + 2];",
      "  dim.d = [n + 3];",
      "  const shape = new Map([",
      '    ["a", dim.a.dim],',
      '    ["b", dim.b.dim],',
      '    ["c", dim.c.dim],',
      '    ["d", dim.d.dim]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {};",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const a = unpacked.get('a').data;",
      "  const b = unpacked.get('b').data;",
      "  const c = unpacked.get('c').data;",
      "  const d = unpacked.get('d').data;",
      "  for (let i = 1; i <= params.dim.a.dim[0]; i++) {",
      "    stateNext[i - 1 + 0] = a[i - 1] + 1;",
      "  }",
      "  for (let i = 1; i <= params.dim.b.dim[0]; i++) {",
      "    stateNext[i - 1 + params.odin.packing.idx.b.start] = b[i - 1] + 2;",
      "  }",
      "  for (let i = 1; i <= params.dim.c.dim[0]; i++) {",
      "    stateNext[i - 1 + params.odin.packing.idx.c.start] = c[i - 1] + 3;",
      "  }",
      "  for (let i = 1; i <= params.dim.d.dim[0]; i++) {",
      "    stateNext[i - 1 + params.odin.packing.idx.d.start] = d[i - 1] + 4;",
      "  }",
      "},"))
})


test_that("generate variable sized array from parameters", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    n <- 2
    a <- parameter()
    dim(a) <- n
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const n = 2;",
      "  dim.a = [n];",
      '  const a = imports.io.readRealArray(params, "a", dim.a);',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  params.a = a;",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {};",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  stateNext[0] = params.a[0] + params.a[1];",
      "},"))
})


test_that("can include integer parameters", {
  dat <- odin_parse({
    a <- parameter(type = "integer")
    b <- parameter(type = "logical")
    c <- parameter(type = "real")
    initial(x) <- 1
    update(x) <- if (b) a else c
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const a = imports.io.readInt(params, "a");',
      '  const b = imports.io.readBool(params, "b");',
      '  const c = imports.io.readReal(params, "c");',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  params.b = b;",
      "  params.c = c;",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  stateNext[0] = (params.b ? params.a : params.c);",
      "},"))
})


test_that("can generate a simple multidimensional array equation", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1, 1] / a[2, 1] + a[1, 2] / a[2, 2] + a[1, 3] / a[2, 3]
    a[, ] <- Normal(0, 1)
    dim(a) <- c(2, 3)
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [2, 3];",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.a.dim[0]; i++) {",
      "    for (let j = 1; j <= params.dim.a.dim[1]; j++) {",
      "      internal.a[i - 1 + (j - 1) * params.dim.a.mult[1]] = random.normal(0, 1);",
      "    }",
      "  }",
      "  stateNext[0] = internal.a[0] / internal.a[1] + internal.a[params.dim.a.mult[1]] / internal.a[1 + params.dim.a.mult[1]] + internal.a[2 * params.dim.a.mult[1]] / internal.a[1 + 2 * params.dim.a.mult[1]];",
      "},"))
})


test_that("can use length() on the rhs", {
  dat <- odin_parse({
    update(x[]) <- x[i] + length(x)
    initial(x[]) <- 0
    dim(x) <- 4
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x').data;",
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    stateNext[i - 1 + 0] = x[i - 1] + params.dim.x.size;",
      "  }",
      "},"))
})


test_that("can use nrow() and ncol() on the rhs", {
  dat <- odin_parse({
    update(x[, ]) <- x[i, j] + nrow(x) / ncol(x)
    initial(x[, ]) <- 0
    dim(x) <- c(4, 3)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x').data;",
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    for (let j = 1; j <= params.dim.x.dim[1]; j++) {",
      "      stateNext[i - 1 + (j - 1) * params.dim.x.mult[1] + 0] = x[i - 1 + (j - 1) * params.dim.x.mult[1]] + params.dim.x.dim[0] / params.dim.x.dim[1];",
      "    }",
      "  }",
      "},"))
})


test_that("can generate complete sums over arrays", {
  dat <- odin_parse({
    update(x) <- sum(y)
    initial(x) <- 0
    y[] <- Normal(0, 1)
    dim(y) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.y.dim[0]; i++) {",
      "    internal.y[i - 1] = random.normal(0, 1);",
      "  }",
      "  stateNext[0] = imports.array.sumAll(internal.y);",
      "},"))
})


test_that("can generate partial sums over arrays", {
  dat <- odin_parse({
    update(x[]) <- sum(y[i, ])
    initial(x[]) <- 0
    y[, ] <- Normal(0, 1)
    dim(y) <- c(3, 4)
    dim(x) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.y.dim[0]; i++) {",
      "    for (let j = 1; j <= params.dim.y.dim[1]; j++) {",
      "      internal.y[i - 1 + (j - 1) * params.dim.y.mult[1]] = random.normal(0, 1);",
      "    }",
      "  }",
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    stateNext[i - 1 + 0] = imports.array.sum2(internal.y, params.dim.y, [i - 1, i - 1], [0, params.dim.y.dim[1] - 1]);",
      "  }",
      "},"))
})


test_that("can add interpolation", {
  dat <- odin_parse({
    update(x) <- y
    initial(x) <- 0
    y <- interpolate(at, ay, "constant")
    at <- parameter()
    ay <- parameter()
    dim(at) <- n
    dim(ay) <- n
    n <- 5
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const y = params.interpolate_y.eval(time);;",
      "  stateNext[0] = y;",
      "},"))
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const n = 5;",
      "  dim.at = [n];",
      "  dim.ay = [n];",
      '  const at = imports.io.readRealArray(params, "at", dim.at);',
      '  const ay = imports.io.readRealArray(params, "ay", dim.ay);',
      '  const interpolate_y = new imports.interpolate.InterpolateConstant(at, ay, "at", "ay");',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  params.at = at;",
      "  params.ay = ay;",
      "  params.interpolate_y = interpolate_y;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate multi-part array", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- b
    b <- a[1] + a[2]
    n <- parameter(type = "integer", constant = TRUE)
    dim(a) <- n
    a[1] <- 1
    a[2] <- Normal(0, 1)
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  internal.a[0] = 1;",
      "  internal.a[1] = random.normal(0, 1);",
      "  const b = internal.a[0] + internal.a[1];",
      "  stateNext[0] = b;",
      "},"))
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const n = imports.io.readInt(params, "n");',
      "  dim.a = [n];",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate multi-part array in variables", {
  dat <- odin_parse({
    initial(x[]) <- 1
    update(x[1]) <- a[1]
    update(x[2]) <- a[2]
    dim(a) <- 2
    a[] <- Normal(0, 1)
    dim(x) <- 2
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.a.dim[0]; i++) {",
      "    internal.a[i - 1] = random.normal(0, 1);",
      "  }",
      "  stateNext[0] = internal.a[0];",
      "  stateNext[1 + 0] = internal.a[1];",
      "},"))
})


test_that("can generate self-referential multi-part array", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + a[n]
    n <- parameter(type = "integer", constant = TRUE)
    a[1] <- 1
    a[2] <- 1
    a[3:length(a)] <- a[i - 2] + a[i - 1]
    dim(a) <- n
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const n = imports.io.readInt(params, "n");',
      "  dim.a = [n];",
      "  const a = Array(dim.a.size).fill(0);",
      "  a[0] = 1;",
      "  a[1] = 1;",
      "  for (let i = 3; i <= dim.a.size; i++) {",
      "    a[i - 1] = a[i - 2 - 1] + a[i - 1 - 1];",
      "  }",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  params.a = a;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can accept multidimensional array as parameter", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + sum(a)
    a <- parameter()
    dim(a) <- c(3, 4, 5)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.a = [3, 4, 5];",
      '  const a = imports.io.readRealArray(params, "a", dim.a);',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.a = a;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can interpolate arrays", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + sum(a)
    a <- interpolate(at, ay, "constant")
    at <- parameter()
    ay <- parameter()
    nt <- 50
    na <- 3
    dim(at) <- nt
    dim(ay) <- c(na, nt)
    dim(a) <- na
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const nt = 50;",
      "  const na = 3;",
      "  dim.at = [nt];",
      "  dim.ay = [na, nt];",
      "  dim.a = [na];",
      '  const at = imports.io.readRealArray(params, "at", dim.at);',
      '  const ay = imports.io.readRealArray(params, "ay", dim.ay);',
      '  const interpolate_a = new imports.interpolate.InterpolateConstantArray(at, ay, dim.a, "at", "ay");',
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.nt = nt;",
      "  params.na = na;",
      "  params.at = at;",
      "  params.ay = ay;",
      "  params.interpolate_a = interpolate_a;",
      "  return { ...params, dim, odin };",
      "},"))
  expect_equal(
    generate_js_system_build_internal(dat),
    c(method_args$build_internal,
      "  return {",
      "    a: Array(params.dim.a.size).fill(0),",
      "  };",
      "},"))
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x');",
      "  internal.a = params.interpolate_a.eval(time);;",
      "  stateNext[0] = x + imports.array.sumAll(internal.a);",
      "},"))
})


test_that("support min/max", {
  dat <- odin_parse({
    update(x) <- min(a) + max(b, c)
    initial(x) <- 0
    a[] <- i
    dim(a) <- 10
    b <- 20
    c <- 30
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  stateNext[0] = imports.array.minAll(params.a) + Math.max(params.b, params.c);",
      "},"))
})


test_that("cast index variables to int when compared to integers", {
  dat <- odin_parse({
    n <- parameter(type = "integer")
    a[] <- if (i == n) 0 else time
    dim(a) <- 3
    update(x) <- a[n]
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.a.dim[0]; i++) {",
      "    internal.a[i - 1] = (i === params.n ? 0 : time);",
      "  }",
      "  stateNext[0] = internal.a[params.n - 1];",
      "},"))
})


test_that("cast array size to int when compared to integers", {
  dat <- odin_parse({
    n <- parameter(type = "integer")
    a[] <- time
    b <- if (length(a) == n) 0 else 1
    dim(a) <- 3
    update(x) <- a[1] + b
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const n = imports.io.readInt(params, "n");',
      "  dim.a = [3];",
      "  const b = (dim.a.size === n ? 0 : 1);",
      "  const shape = new Map([",
      '    ["x", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.n = n;",
      "  params.b = b;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate pi", {
  dat <- odin_parse({
    initial(x) <- pi
    a <- sin(180 / pi)
    update(x) <- x + a
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_initial(dat),
    c(method_args$initial,
      "  state[0] = Math.PI;",
      "},"))
})


test_that("Array assignment with index on lhs and rhs (1)", {
  ## See (mrc-5894)
  dat <- odin_parse({
    seed_age_band <- as.integer(4)
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n
    initial(y) <- 0
    update(y) <- sum(x)
    x[] <- Poisson(1)
    x[seed_age_band] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    internal.x[i - 1] = random.poisson(1);",
      "  }",
      "  {",
      "    const i = params.seed_age_band;",
      "    internal.x[params.seed_age_band - 1] = internal.x[i - 1] + 1;",
      "  }",
      "  stateNext[0] = imports.array.sumAll(internal.x);",
      "},")
  )
})


test_that("Array assignment with index on lhs and rhs (1)", {
  ## See (mrc-5894)
  dat <- odin_parse({
    seed_age_band <- as.integer(4)

    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n

    initial(y) <- 0
    update(y) <- sum(x)

    lambda <- parameter()
    dim(lambda) <- n

    x[] <- Poisson(lambda[i])
    x[seed_age_band] <- x[i] + 1 + seed_age_band * 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    internal.x[i - 1] = random.poisson(params.lambda[i - 1]);",
      "  }",
      "  {",
      "    const i = params.seed_age_band;",
      "    internal.x[params.seed_age_band - 1] = internal.x[i - 1] + 1 + params.seed_age_band * 0;",
      "  }",
      "  stateNext[0] = imports.array.sumAll(internal.x);",
      "},")
  )
})


test_that("Array assigment lhs: integer index, rhs: i", {
  ## See (mrc-5894)
  dat <- odin_parse({
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n

    initial(y) <- 0
    update(y) <- sum(x)

    x[] <- Poisson(1)
    x[4] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    internal.x[i - 1] = random.poisson(1);",
      "  }",
      "  {",
      "    const i = 4;",
      "    internal.x[3] = internal.x[i - 1] + 1;",
      "  }",
      "  stateNext[0] = imports.array.sumAll(internal.x);",
      "},")
  )
})


test_that("Array assigment lhs: expression index, rhs: i", {
  ## See (mrc-5894)
  dat <- odin_parse({
    seed_age_band <- as.integer(4)
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n
    initial(y) <- 0
    update(y) <- sum(x)
    x[] <- Poisson(1)
    x[seed_age_band + 1] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    internal.x[i - 1] = random.poisson(1);",
      "  }",
      "  {",
      "    const i = params.seed_age_band + 1;",
      "    internal.x[params.seed_age_band + 1 - 1] = internal.x[i - 1] + 1;",
      "  }",
      "  stateNext[0] = imports.array.sumAll(internal.x);",
      "},")
  )
})


test_that("Array assigment lhs: length index, rhs: i", {
  ## See (mrc-5894)
  dat <- odin_parse({
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n
    initial(y) <- 0
    update(y) <- sum(x)
    x[] <- Poisson(1)
    x[length(x)] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    internal.x[i - 1] = random.poisson(1);",
      "  }",
      "  {",
      "    const i = params.dim.x.size;",
      "    internal.x[params.dim.x.size - 1] = internal.x[i - 1] + 1;",
      "  }",
      "  stateNext[0] = imports.array.sumAll(internal.x);",
      "},")
  )
})


test_that("cast integers to sizes", {
  dat <- odin_parse({
    n <- parameter(5, type = "integer")
    dim(x) <- n
    x[1:n] <- time
    initial(y) <- 0
    update(y) <- x[1]
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  for (let i = 1; i <= Number(params.n); i++) {",
      "    internal.x[i - 1] = time;",
      "  }",
      "  stateNext[0] = internal.x[0];",
      "},"))
})


test_that("can generate system with output", {
  dat <- odin_parse({
    initial(x[]) <- 0
    deriv(x[]) <- x[i] * r[i]
    r <- parameter()
    n <- 3
    dim(x) <- n
    dim(r) <- n
    output(tot) <- sum(x)
  })
  dat <- generate_prepare(dat)

  expect_equal(dat$variables, c("x", "tot"))
  expect_equal(dat$output, "tot")
  expect_equal(dat$storage$packing$state$name, c("x", "tot"))

  expect_equal(
    generate_js_system_initial(dat),
    c(method_args$initial,
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    state[i - 1 + 0] = 0;",
      "  }",
      "},"))

  expect_equal(
    generate_js_system_rhs(dat),
    c(method_args$rhs,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x').data;",
      "  for (let i = 1; i <= params.dim.x.dim[0]; i++) {",
      "    stateDeriv[i - 1 + 0] = x[i - 1] * params.r[i - 1];",
      "  }",
      "},"))

  expect_equal(
    generate_js_system_output(dat),
    c(method_args$output,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x').data;",
      "  const tot = imports.array.sumAll(x);",
      "  state[params.odin.packing.idx.tot.start] = tot;",
      "},"))
})


test_that("can generate code for parameter constraints", {
  dat <- odin_parse({
    r <- parameter(min = 2)
    initial(y) <- 0
    update(y) <- y * r
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      '  const r = imports.io.readReal(params, "r");',
      '  imports.io.checkMinScalar(r, 2, "r");',
      "  const shape = new Map([",
      '    ["y", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.r = r;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("can generate code for array parameter constraints", {
  dat <- odin_parse({
    r <- parameter(max = 2)
    dim(r) <- 5
    initial(y) <- 0
    update(y) <- y * sum(r)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  dim.r = [5];",
      '  const r = imports.io.readRealArray(params, "r", dim.r);',
      '  imports.io.checkMaxArray(r, 2, "r");',
      "  const shape = new Map([",
      '    ["y", []]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.r = r;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("Can read parameters into var with aliased dimension", {
  dat <- odin_parse({
    update(x[]) <- x[i] + a
    initial(x[]) <- x0[i]

    dim(x) <- dim(x0)
    dim(x0) <- n_x
    x0 <- parameter()
    n_x <- parameter()
    a <- parameter()
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_update(dat),
    c(method_args$update,
      "  const unpacked = params.odin.packing.unpackArray(state);",
      "  const x = unpacked.get('x').data;",
      "  for (let i = 1; i <= params.dim.x0.dim[0]; i++) {",
      "    stateNext[i - 1 + 0] = x[i - 1] + params.a;",
      "  }",
      "},"))
})


test_that("correct packing with aliased arrays", {
  dat <- odin_parse({
    dim(a, b) <- c(x, y)
    x <- 2
    y <- 3
    initial(a[, ]) <- 0
    initial(b[, ]) <- 0
    update(a[, ]) <- 1
    update(b[, ]) <- 1
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_js_system_build_params(dat),
    c(method_args$build_params,
      "  const dim = imports.array.getDimObj();",
      "  const odin = {};",
      "  const x = 2;",
      "  const y = 3;",
      "  dim.a = [x, y];",
      "  const shape = new Map([",
      '    ["a", dim.a.dim],',
      '    ["b", dim.a.dim]',
      "  ]);",
      "  odin.packing = new imports.Packer({ shape });",
      "  params.x = x;",
      "  params.y = y;",
      "  return { ...params, dim, odin };",
      "},"))
})


test_that("output <- TRUE version generates same code", {
  dat1 <- odin_parse({
    initial(x[]) <- 0
    deriv(x[]) <- x[i] * r[i]
    r <- parameter()
    n <- 3
    dim(x) <- n
    dim(r) <- n
    output(tot) <- sum(x)
  })

  dat2 <- odin_parse({
    initial(x[]) <- 0
    deriv(x[]) <- x[i] * r[i]
    r <- parameter()
    n <- 3
    dim(x) <- n
    dim(r) <- n
    tot <- sum(x)
    output(tot) <- TRUE
  })

  expect_equal(generate_js_system(dat2), generate_js_system(dat1))
})


test_that("cope with array output", {
  dat1 <- odin_parse({
    initial(x[]) <- 0
    deriv(x[]) <- x[i]
    a[] <- x[i] + 1
    output(a) <- TRUE
    dim(x, a) <- 5
  })

  dat2 <- odin_parse({
    initial(x[]) <- 0
    deriv(x[]) <- x[i]
    output(a[]) <- x[i] + 1
    dim(x, a) <- 5
  })

  expect_equal(generate_js_system(dat2), generate_js_system(dat1))
})


test_that("can use output as well as return it", {
  dat <- odin_parse({
    deriv(x) <- a
    initial(x) <- 0
    a <- time * 2
    output(a) <- TRUE
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_rhs(dat),
    c(method_args$rhs,
      "  const a = time * 2;",
      "  stateDeriv[0] = a;",
      "},"))
})


test_that("can use depend on output variables correctly", {
  dat <- odin_parse({
    deriv(x) <- a
    initial(x) <- 0
    a <- interpolate(at, ay, "constant")
    at <- parameter()
    ay <- parameter()
    dim(at, ay) <- 5
    output(a) <- TRUE
    output(b) <- 2 * a
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_js_system_rhs(dat),
    c(method_args$rhs,
      "  const a = params.interpolate_a.eval(time);;",
      "  stateDeriv[0] = a;",
      "},"))
  expect_equal(
    generate_js_system_output(dat),
    c(method_args$output,
      "  const a = params.interpolate_a.eval(time);;",
      "  const b = 2 * a;",
      "  state[1] = a;",
      "  state[2] = b;",
      "},"))
})
