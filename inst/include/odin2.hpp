#pragma once

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
