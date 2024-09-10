#pragma once

// Better than including this in the package, I think we might copy it
// into the model, essentially vendoring at the point of generation?

#include <dust2/common.hpp>
#include <dust2/array.hpp>

namespace odin2 {

template <typename T>
T reduce_sum(T start, T end) {
  return std::accumulate(start, end, static_cast<T>(0));
}

template <typename T>
T reduce_prod(T start, T end) {
  return std::accumulate(start, end, static_cast<T>(1), std::multiplies<T>());
}

}
