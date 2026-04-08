#include <Cordic_E2E_DC_CXX.cpp>

#include <execution>
#include <algorithm>
#include <chrono>
#include <iostream>
#include <limits>
#include <array>
#include <format>
#include <cmath>
using namespace std;


template<typename T>
class stats
{
  int nbre_points;
  T the_sum_avg, the_sum_stddev, the_sum_skew, the_sum_kurt;
  T the_min, the_max;
public:
  stats();
  stats<T>&operator+=(const T&);
  operator string()const;
};

template<typename T>
class diffs
{

};
