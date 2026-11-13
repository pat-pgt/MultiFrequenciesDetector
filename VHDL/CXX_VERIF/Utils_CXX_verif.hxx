#include <algorithm>
#include <execution>
#include <chrono>
#include <iostream>
#include <limits>
#include <set>
#include <map>
#include <vector>
#include <deque>
#include <format>
#include <cmath>
#include <stdexcept>
#include <optional>
#include <limits>
#include <ranges>
#include <bitset>
#include <cassert>

using namespace std;

/** @brief
 */ 
template <typename cxx_reg_type, unsigned short reg_size>
class Value_Data
{
  cxx_reg_type value_init;
public:
  Value_Data() = delete;
  Value_Data(const cxx_reg_type&value_init);
  Value_Data&operator=(const Value_Data&a){
	value_init=a.value_init;
	return*this;
  }

  operator cxx_reg_type()const{ return value_init; }
  /** @brief Get_Positive_Negative_Value
   *
   * Used by the end to end light-filter,
   *   this functions return the value 
   */
  cxx_reg_type Get_Positive_Negative_Value(const bool&isNeg)const{
	if ( isNeg )
	  // the value of the 2'nd complement are in in the interval -2**N to (2**N)-1
	  // Trying to take minus -2**N would have cause wrong result (-2**N again).
	  // Since the number are checked to be in the -max/2 and +max/2 mask,
	  //   there is no risk.
	  return -value_init;
	else
	  return value_init;
  } 

  constexpr long long GetValueSquared()const {  
	long long X = value_init;
	return X * X;
  }

  /** Two divided validation
   * Since the Cordic algorithm grows the values, the input is divided by 2.
   * The Python emulations validate that, for the 3 first stages done "by hand".\n
   * Functions are provided for standard IT formats and for the vector size in VHDL
   * (CXXRTL expects the unused bits are 0).\n
   * It is a template instantiation. However, for now, it is specific instantiations.
   */
  void TwoDividedValidation()const;

  constexpr cxx_reg_type value_type()const{};
  const unsigned short value_size = reg_size;
    /** @brief Get size to compare with the VHDL
   *
   * This function returns the number of BITS.
   */
  const unsigned char GetRegSize()const{ return reg_size;}

  long double module_value_type()const;
};

/** @brief Holds an X and a Y value
 *
 * The second template member looks like redundant.
 * Since the interface between the C++ test software and the C++ VHDL emulator
 * Uses only standard C++ types, the reg_size is not always the size of the reg_type.
 */ 
template <typename cxx_reg_type, unsigned short reg_size>
class XY_Data
{
protected:
  Value_Data<cxx_reg_type, reg_size> X_init;
  Value_Data<cxx_reg_type, reg_size> Y_init;
public:
  XY_Data() = delete;
  XY_Data(const cxx_reg_type&X_init,const cxx_reg_type&Y_init);

  XY_Data&operator=(const XY_Data&a){
	X_init=a.X_init;
	Y_init=a.Y_init;
	return*this;
  }
  constexpr void TwoDividedValidation()const{
	X_init.TwoDividedValidation();
	Y_init.TwoDividedValidation();
  }

  constexpr cxx_reg_type value_type()const{};
  const unsigned short value_size = reg_size;

/** @brief Get the X value based on a standard it format
   *
   * @return the value
   */
  constexpr cxx_reg_type Get_X()const{ return X_init; }
/** @brief Get the Y value based on a standard it format
   *
   * @return the value
   */
  constexpr cxx_reg_type Get_Y()const{ return Y_init; }

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
  /** @brief Get size to compare with the VHDL
   *
   * This function returns the number of BITS.
   */
  const unsigned char GetRegSize()const{ return reg_size;}

  long double module_value_type()const;
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

/** @brief Computes some high level statistics about the Y to 0 angle Z
 *
 * This class has 2 sub classes:
 * * If the vector is known to spin only CCW.
 *   Large values are allowed from 0 to 31/16 PI
 * * If the vector can spin CZ or CCW.
 *   The values should be known to be limited to 7/16 PI
 * For more information, see the sub classes.
 */
template<unsigned short reg_size>
class Axis_counter_base
{
protected:
  uint8_t init_decimal;
  uint8_t last_decimal;
public:
  Axis_counter_base(const bitset<reg_size>&val_Z):
	init_decimal(Slice_4_high_bits( val_Z )),last_decimal(Slice_4_high_bits( val_Z ))
  {}
  constexpr uint8_t Slice_4_high_bits( const bitset< reg_size >&the_input )
  {
	uint8_t the_return = 0;
	// Some new C++ versions handles slices, then this is for now
	if ( the_input.test( reg_size - 1 ) )
	  the_return += 8;
	if ( the_input.test( reg_size - 2 ) )
	  the_return += 4;
	if ( the_input.test( reg_size - 3 ) )
	  the_return += 2;
	if ( the_input.test( reg_size - 4 ) )
	  the_return += 1;
	return the_return;
  }
};
/** @brief CCW only specific compute of some high level statistics about the Y to 0 angle Z
 *
 * * Counts the number of times a vector passes the origin.
 * * Computes the delta between the first and the last time,
 *     in order to get decimals to the number of spins.
 * It is mostly useful for the end to end DC test as
 *   one can compare different results from different initial conditions of X and Y
 */
template<unsigned short reg_size>
class Axis_counter_CCW : public Axis_counter_base<reg_size>
{
  unsigned long the_counter;
public:
  Axis_counter_CCW(const bitset<reg_size>&val_Z):
	Axis_counter_base<reg_size>( val_Z ),the_counter(0)
  {}
  constexpr Axis_counter_CCW<reg_size>&operator+=(const bitset< reg_size >&the_input){
	uint8_t new_decimal = this->Slice_4_high_bits( the_input );
	// It is not that elegant. It consume more resources but neglectable
	// But it is more easy to validate.

	// If they are equal, we are not going to trigger the counter again
	while( new_decimal != this->last_decimal )
	  {
		if ( this->last_decimal == 0xf )
		  this->last_decimal = 0;
		else
		  this->last_decimal += 1;
		// We trigger only once, because last_decimal is spinning
		if ( this->last_decimal == this->init_decimal )
		  the_counter += 1;
	  }
	return*this;
  }
  /** @brief Displays the number of times time angle spun with some decimals
   *
   * Displays, in this order:
   * * the number of times the angle passed through the (simulation) initial angle.
   * * the number of quadrants (from 0 to 3) the final angle "over ran" 
   * * the decimals inside the quadrant from 0 to 3/4
   */
  constexpr operator const string()const{
	uint32_t the_spin = (bitset<4>(this->last_decimal - this->init_decimal ) ).to_ulong();

	string the_return = to_string(the_counter);
	the_return += " ";
	the_return += to_string( the_spin / 4 );
	the_return += " ";
	the_spin -= 4 * ( the_spin / 4 );
	switch ( the_spin ) {
	case 0: return the_return + " 0 ";
	case 1: return the_return + "1/4";
	case 2: return the_return + "1/2";
	default: return the_return + "3/4";
	}
  }
};
/** @brief Holds the last angle, the statistics and the number of X axis passes
 */
template <typename stats_type, typename cxx_reg_type, unsigned short reg_size>
class Z_spin_data
{
  /** The cxx_reg_type is a standard IT type, with its size.
	  The VHDL register is a reg_size length.
	  Then, in case of non standard one, the mask fixes the Arithmetic **/
  cxx_reg_type reg_size_mask;
  cxx_reg_type last_Z;
 public:
  stats<stats_type> the_stats;
  Axis_counter_CCW<reg_size> the_X_counter;
  Z_spin_data()=delete;
  Z_spin_data(const cxx_reg_type&last_Z,const stats<stats_type>&the_stats):
	last_Z(last_Z),
	the_stats(the_stats),
    the_X_counter(bitset<reg_size>(last_Z)){
	reg_size_mask = 0x0;
	for ( auto ind = 0; ind < reg_size; ind++ )
	  {
		reg_size_mask <<= 1;
		reg_size_mask |= 0x1;
	  }
  }
  constexpr Z_spin_data<stats_type,cxx_reg_type,reg_size>&operator+=(const cxx_reg_type&new_Z){
	the_X_counter += bitset<reg_size>(new_Z);

	cxx_reg_type the_diff = new_Z - last_Z;
	the_diff &= reg_size_mask;

	the_stats += (double)the_diff;

	last_Z = new_Z;

	return*this;
  }
};
