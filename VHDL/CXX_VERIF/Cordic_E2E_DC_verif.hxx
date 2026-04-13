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
#include <optional>

using namespace std;

/** @file Cordic_E2E_DC_verif.hxx
 * @brief Run a formal verification
 *
 * The filtering and the down-sampling are removed.\n
 * Then, the output of the Z to 0 stage should always be
 * * The module of X and Y should be the one of the input grown by a constant
 *   due to the product of the inverses of the Z to 0 cosines.
 * * The scalar product of consecutive X and Y,
 *   according with the octave and the note should be a constant
 *   equal to the cosine of the angle shift multiplied by the product of the modules
 * * The should be as close as possible to 0.\n
 * Then, the output of the Z to 0 stage should always be
 * * The X contains the module of the input value grown by a constant
 *   due to the product of the inverses of the cosines (both Z to 0 and Y to 0.
 * * The Y should be close as possible to 0
 * * The Z should grow continuously, according with the octave and the note.\n
 * The test has to be run for a set of values, including Y not 0.
 * It verifies the computations and if there are some "cross talk" in the design.
 */

/* @brief Holds the initial values
 *
 * Since the Cordic algorithm grows the values, the input is divided by 2.
 * Functions are provided for standard IT formats and for the vector size in VHDL
 * (CXXRTL expects the unused bits are 0).\n
 * This is a first version that runs ONLY for 32 bits reg_size.
 * It should be moved into an implementation using bit wise.
 * However, due to the implementation, one bigger and one smaller test
 *   can validate the design.
 * That means a future 16 and a future 48 bits may be enough
 */ 
class InitialValueData
{
  int X_init;
  int Y_init;

public:
  InitialValueData() = delete;
  InitialValueData(const int&X_init,const int&Y_init,bool sanity_check = true);

  InitialValueData&operator=(const InitialValueData&a){
	X_init=a.X_init;
	Y_init=a.Y_init;
	return*this;
  }

  int value_type()const;
  long double module_value_type()const;
  /** @brief Get the X value for the VHDL
   *
   * @return the value
   */
  constexpr int Get_X_init_31()const{ return X_init & 0x7fffffff; }
/** @brief Get the Y value for the VHDL
   *
   * @return the value
   */
  constexpr int Get_Y_init_31()const{ return Y_init & 0x7fffffff; }
/** @brief Get the X value based on a standard it format
   *
   * @return the value
   */
  constexpr int Get_X_init_32()const{ return X_init; }
/** @brief Get the Y value based on a standard it format
   *
   * @return the value
   */
  constexpr int Get_Y_init_32()const{ return Y_init; }
/** @brief Get the module squared
 *
 * The values, in general, are intended to be cast into floating types
 * Then this function returns the value before the square root
 */
  constexpr long long GetModuleSquared()const {  
	long long X = X_init;
	long long Y = Y_init;
	return X * X + Y * Y;
  }
  constexpr long long GetScalarProduct(const InitialValueData&a)const {
	long long X1 = X_init;
	long long Y1 = Y_init;
	long long X2 = a.X_init;
	long long Y2 = a.Y_init;
	return X1 * X2 + Y1 * Y2;
  }
  operator string()const{
	return format("{},{}\t",X_init,Y_init);
  }
  /** @brief Get size to compare with the VHDL
   *
   * This function returns the number of BITS.
   */
  const unsigned char GetRegSize()const{ return 32;}
};

/* @brief Computes the statistics of the results
 *
 * This class is generic as it has no direct connection with the VHDL.
 * The type should be enough large to avoid any precision lost.
 */
template<typename T>
class stats
{
  unsigned int nbre_points;
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
  operator unsigned int()const{return nbre_points;}
};

template<typename T>
class diffs
{

};

/* @brief Stores the result of all the verifications
 *
 * The types are hard-coded.\n
 * They can fit all the sizes up to reg_size = 48.\n
 * Since it is only involved for data collection and statistics
 *   it would have been irrelevant to try smaller size,
 *   for instance for 16 or 24 bits.
 */
struct SimulDataType
  {	
  public:

	const float GetInitAngle()const;

	struct {
	  stats<long double>check_module_constant;
	  map< pair< unsigned char, unsigned char >,
		   pair<InitialValueData,stats<long double> > > check_scalar_prod_per_ON_constant;
	  stats<double>check_Z_converges;
	}  Z_2_0;
	struct {
	  stats<double>check_X_converges;
	  stats<double>check_Y_converges;
	  map< pair< unsigned char, unsigned char >,
		   pair<unsigned int, stats<double> > > check_spin_per_ON_constant;
	}  Y_2_0;

	SimulDataType()=delete;
	SimulDataType(const long double&module_vector,
				  const unsigned short &Z_2_0_stages,const unsigned short&Y_2_0_stages);
	optional<unsigned int> GetAndCheck_nbre_points()const;
};
