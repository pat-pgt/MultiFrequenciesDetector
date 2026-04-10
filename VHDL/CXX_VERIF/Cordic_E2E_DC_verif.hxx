#include <Cordic_E2E_DC_CXX.cpp>

#include <execution>
#include <algorithm>
#include <chrono>
#include <iostream>
#include <limits>
#include <vector>
#include <format>
#include <cmath>
#include <stdexcept>

using namespace std;


class InitialValueData
{
  const int X_init;
  const int Y_init;

public:
  InitialValueData() = delete;
  InitialValueData(const int&X_init,const int&Y_init);

  constexpr int Get_X_init_31()const{ return X_init & 0x7fffffff; }
  constexpr int Get_Y_init_31()const{ return Y_init & 0x7fffffff; }
  constexpr int Get_X_init_32()const{ return X_init; }
  constexpr int Get_Y_init_32()const{ return Y_init; }
  constexpr long long GetModuleSquared()const {  
	long long X = X_init;
	long long Y = Y_init;
	return X * X + Y * Y;
  }
};

template<typename T>
class stats
{
  int nbre_points;
  T the_sum_avg, the_sum_stddev, the_sum_skew, the_sum_kurt;
  T the_min, the_max;
  T normalize, offset;
public:
  typedef T value_type;

  stats();
  stats<T>&operator+=(const T&);
  constexpr void SetNormalize(const T&normalize){
	if ( nbre_points > 0 )
	  throw invalid_argument("Internal: The normalization can NOT be changed after the first value");
	this -> normalize = normalize;
  }
  constexpr void SetOffset(const T&offset){
	if ( nbre_points > 0 )
	  throw invalid_argument("Internal: The offset can NOT be changed after the first value");
	this -> offset = offset;
  }
  operator string()const;
};

template<typename T>
class diffs
{

};

struct SimulDataType
  {	
  public:

	const float GetInitAngle()const;

	struct {
	  stats<long double>check_module_constant;
	  stats<double>check_Z_converges;
	}  Z_2_0;
	struct {
	  stats<double>check_X_converges;
	  stats<double>check_Y_converges;
	}  Y_2_0;

	SimulDataType()=delete;
	SimulDataType(const long double&module_vector,
				  const unsigned short &Z_2_0_stages,const unsigned short&Y_2_0_stages);
};
