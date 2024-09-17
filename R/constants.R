INDEX <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")

## TODO: it would be great if monty could advertise what is in its
## maths library.
##
## TODO: %% and %/% for monty
FUNCTIONS_MONTY_MATH <-
  c("^" = "pow",
    "%%" = "fmod",
    "%/%" = "fintdiv",
    ceiling = "ceil",
    sign = "sign",
    floor = "floor",
    round = "round",
    trunc = "trunc",
    sqrt = "sqrt",
    abs = "abs",
    exp = "exp",
    expm1 = "expm1",
    log = "log",
    log2 = "log2",
    log10 = "log10",
    log1p = "log1p",
    cos = "cos",
    sin = "sin",
    tan = "tan",
    acos = "acos",
    asin = "asin",
    atan = "atan",
    atan2 = "atan2",
    cosh = "cosh",
    sinh = "sinh",
    tanh = "tanh",
    acosh = "acosh",
    asinh = "asinh",
    atanh = "atanh")


FUNCTIONS <- list(
  "+" = 1:2,
  "-" = 1:2,
  "*" = 2,
  "/" = 2,
  "^" = 2,
  "(" = 1,
  "%%" = 2,
  "%/%" = 2,
  "if" = 3,
  "!" = 1,
  ">" = 2,
  "<" = 2,
  ">=" = 2,
  "<=" = 2,
  "==" = 2,
  "!=" = 2,
  "&&" = 2,
  "||" = 2,
  length = 1,
  nrow = 1,
  ncol = 1,
  sum = 1,
  as.logical = 1,
  as.integer = 1,
  as.numeric = 1,
  ceiling = ceiling,
  sign = sign,
  floor = floor,
  round = function(x) NULL, # no digits support yet
  trunc = function(x) NULL,
  abs = abs,
  exp = exp,
  expm1 = expm1,
  log = function(x) NULL, # no general base yet
  log2 = log2,
  log10 = log10,
  log1p = log1p,
  cos = cos,
  sin = sin,
  tan = tan,
  acos = acos,
  asin = asin,
  atan = atan,
  atan2 = atan2,
  cosh = cosh,
  sinh = sinh,
  tanh = tanh,
  acosh = acosh,
  asinh = asinh,
  atanh = atanh)


## TODO: rlang struggles with matching primitives in
## parse_expr_usage/match_call, so we just squash them manually; later
## we'll work out how to cope with this properly.
FUNCTIONS <- lapply(FUNCTIONS, function(x) {
  if (is.primitive(x)) length(formals(args(x))) else x
})
