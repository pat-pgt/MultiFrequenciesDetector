#include <Cordic_E2E_DC_CXX_noDS.cpp>
#include <Cordic_E2E_DC_CXX_DS1.cpp>
#include <Cordic_E2E_DC_CXX_DS2.cpp>

#include <algorithm>
#include <execution>
#include <chrono>
#include <iostream>
#include <limits>
#include <set>
#include <vector>
#include <format>
#include <cmath>
#include <stdexcept>
#include <optional>
#include <limits>
#include <ranges>

using namespace std;

/** @file Cordic_E2E_DC_verif.hxx
 * @brief Run a formal verification
 *
 * This test has to be re-run after every parameters change or design tools change.\n
 * The filtering is removed. The down-sampling is optional.
 * Both cases should be run.\n
 * Then, the output of the Z to 0 stage should always be
 * * The module of X and Y should be the one of the input grown by a constant
 *   due to the product of the inverses of the Z to 0 cosines.
 * * The scalar product of consecutive X and Y,
 *   according with the octave and the note should be a constant
 *   equal to the cosine of the angle shift multiplied by the product of the modules
 * * The Z should be as close as possible to 0.\n
 * Then, the output of the Y to 0 stage should always be
 * * The X contains the module of the input value grown by a constant
 *   due to the product of the inverses of the cosines (both Z to 0 and Y to 0).
 * * The Y should be close as possible to 0
 * * The Z should grow continuously, according with the octave and the note.
 * ** Without the down-sampling, one should see angles spinning from slow to fast
 *      when the note and the octave grow.
 * ** With the down-sampling, one should see the same angle spinning for
 *      all the octaves a given note. The angles spinning's should grow when the note grows.
 *      The ratio between the slowest and the fastest should be lower than 2.\n
 * The test has to be run for a set of initial values.
 * In normal run, the input is placed to the X and the Y is set to 0.
 * The test should be done for multiple starting angles (Y equal or not to 0).
 * It verifies the computations and if there are some "cross talk" in the design.
 */

/* @brief Holds an X and a Y value
 *
 * The second template member looks like redundant.
 * Since the interface between the C++ test software and the C++ VHDL emulator
 * Uses only standard C++ types, the reg_size is not always the size of the reg_type.
 */ 
template <typename cxx_reg_type, unsigned short reg_size>
class XY_Data
{
protected:
  cxx_reg_type X_init;
  cxx_reg_type Y_init;
public:
  XY_Data() = delete;
  XY_Data(const int&X_init,const int&Y_init);

  XY_Data&operator=(const XY_Data&a){
	X_init=a.X_init;
	Y_init=a.Y_init;
	return*this;
  }

  constexpr cxx_reg_type value_type()const{};
  const unsigned short value_size = reg_size;

/** @brief Get the X value based on a standard it format
   *
   * @return the value
   */
  constexpr int Get_X_full()const{ return X_init; }
/** @brief Get the Y value based on a standard it format
   *
   * @return the value
   */
  constexpr int Get_Y_full()const{ return Y_init; }

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
  constexpr long double GetCosine(const XY_Data<cxx_reg_type,reg_size>&a)const {
	long long X1 = X_init;
	long long Y1 = Y_init;
	long long X2 = a.X_init;
	long long Y2 = a.Y_init;
	long double scalarProduct = X1 * X2 + Y1 * Y2;
	// Since it has been checked the modules squared are always constant
	// It is assumed here the sqrt( module_1 ** 2 ).sqrt( module_2 ** 2 ) = module_2 ** 2
	return scalarProduct / GetModuleSquared();
  }
  operator string()const{
	return format("{},{}\t",X_init,Y_init);
  }
  string string_light()const{
	return format("{: 4},{: 4}\t",X_init/1000000,Y_init/1000000);
  }
};
/* @brief Holds the initial values
 *
 * Since the Cordic algorithm grows the values, the input is divided by 2.
 * The Python emulations validate that, for the 3 first stages done "by hand".\n
 * Functions are provided for standard IT formats and for the vector size in VHDL
 * (CXXRTL expects the unused bits are 0).\n
 * It is a template instantiation. However, for now, it is specific instantiations.
 */ 
  template <typename cxx_reg_type, unsigned short reg_size>
  class InitialValueData : public XY_Data<cxx_reg_type, reg_size>
{
public:
  InitialValueData() = delete;
  InitialValueData(const int&X_init,const int&Y_init);

  long double module_value_type()const;
  /** @brief Get the X value for the VHDL
   *
   * @return the value
   */
  constexpr cxx_reg_type Get_X_init_2divided()const;
/** @brief Get the Y value for the VHDL
   *
   * @return the value
   */
  constexpr cxx_reg_type Get_Y_init_2divided()const;
  /** @brief Get size to compare with the VHDL
   *
   * This function returns the number of BITS.
   */
  const unsigned char GetRegSize()const{ return reg_size;}
};

/** @brief Computes the statistics of the results
 *
 * This class is generic as it has no direct connection with the VHDL.\n
 * The type should be enough large to avoid any precision lost.\n
 * If the average value is an expected value, it is a good idea to set here.
 * All the computation are going to be done on the difference to improve the precision.
 * If needed, the offset can be add back on the result.
 */
template<typename T>
class stats
{
  unsigned int nbre_points;
  T the_sum_avg, the_sum_stddev, the_sum_skew, the_sum_kurt;
  T the_min, the_max;
  T normalize, offset;
  optional< T* >avg_ratios_between_stats;
public:
  typedef T value_type;

  stats();
  stats(T* avg_ratios_between_stats);

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
  string Basic_display();
  string Display_without_offset_normalize()const;
  string Display_arccos_degrees()const;
  operator unsigned int()const{return nbre_points;}
  string Display_arccos_Nth_turns()const;
};

/** @brief Stores the data to count the number of times a vector passes the origin.
 *
 * The assume is the spin, at each iteration, is always less than PI.
 * The last high bit is stored. If it was 1 and now it is 0,
 *   the counter is incremented
 */
template<typename cxx_reg_type,unsigned short reg_size>
class Axis_counter
{
  const cxx_reg_type high_bit_mask;
  bool last_sign;
  unsigned long the_counter;
public:
  // TODO TODO TODO set the bit properly  in order to run with other sizes than 32
  Axis_counter():high_bit_mask(0x80000000),last_sign(false),the_counter(0){}
  constexpr Axis_counter<cxx_reg_type,reg_size>&operator+=(const cxx_reg_type&the_input){
	bool this_sign;
	if ( ( the_input & high_bit_mask ) == 0 )
	  this_sign = false;
	else
	  this_sign = true;
	if ( this_sign == false && last_sign == true )
	  the_counter += 1;
	last_sign = this_sign;
	return*this;
 }
  constexpr operator const unsigned long()const{return the_counter;}
};
/** @brief Holds the last angle, the statistics and the number of X axis passes
 */
template <typename stats_type, typename cxx_reg_type, unsigned short reg_size>
struct Z_spin_data
{
  cxx_reg_type last_Z;
  stats<stats_type> the_stats;
  Axis_counter<cxx_reg_type,reg_size> the_X_counter;
  Z_spin_data()=delete;
  Z_spin_data(const cxx_reg_type&last_Z,const stats<stats_type>&the_stats): last_Z(last_Z),the_stats(the_stats){}
};
/** @brief Stores the result of all the verifications
 *
 * The types are hard-coded.\n
 * They can fit all the sizes up to reg_size = 48.\n
 * Since it is only involved for data collection and statistics
 *   it would have been irrelevant to try smaller size,
 *   for instance for 16 or 24 bits.
 */
template <typename stats_type, typename stats_long_type, typename cxx_reg_type, unsigned short reg_size>
struct SimulDataType
  {	
  public:

	const float GetInitAngle()const;

	struct {
	  stats<stats_long_type>check_module_constant;
	  map< pair< unsigned char, unsigned char >,
		   pair<XY_Data<cxx_reg_type,reg_size>,stats<stats_long_type> > > check_scalar_prod_per_ON_constant;
	  stats<stats_type>check_Z_converges;
	}  Z_2_0;
	struct {
   	  stats_type avg_ratios_between_stats;
	  stats<stats_type>check_X_converges;
	  stats<stats_type>check_Y_converges;
	  map< pair< unsigned char, unsigned char >,
		   Z_spin_data< stats_type, cxx_reg_type, reg_size > > check_spin_per_ON_constant;
	}  Y_2_0;

	void Init(const stats_long_type&module_vector,
		 const unsigned short &Z_2_0_stages,const unsigned short&Y_2_0_stages);
	optional<unsigned int> GetAndCheck_nbre_points()const;
};


