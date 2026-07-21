/** @file Cordic_E2E_DC_verif.hxx
 * @brief Run a formal verification
 *
 * There is no more documentation, for future information, see the .hxx file documentation
 */


template <typename cxx_reg_type, unsigned short reg_size>
XY_Data<cxx_reg_type,reg_size>::XY_Data(const int&X_init,const int&Y_init):
  X_init(X_init),Y_init(Y_init)
{};

/** @brief InitialValueData constructor
 *
 * Constructs for reg_size equal to 32
 * Other sizes are not yet supported, see in the .hxx file
 */
template <>
InitialValueData<int,32>::InitialValueData(const int&X_init,const int&Y_init):
  XY_Data(X_init,Y_init)
{
  // Check if the numbers are 31 bits
  if ( ( X_init & 0xc0000000 ) == 0x80000000 || ( X_init & 0xc0000000 ) == 0x40000000 )
	throw length_error("The initial X value is out of range, should be signed 31 bits ");
  if ( ( Y_init & 0xc0000000 ) == 0x80000000 || ( Y_init & 0xc0000000 ) == 0x40000000 )
	throw length_error("The initial Y value is out of range, should be signed 31 bits ");
}


template <>
int InitialValueData<int,32>::Get_X_init_2divided()const
{
  return X_init & 0x7fffffff;
}
template <>
int InitialValueData<int,32>::Get_Y_init_2divided()const
{
  return Y_init & 0x7fffffff;
}


/** @brief stats class constructor
 *
 */
template<typename T>stats<T>::stats():
  nbre_points( 0 ),
  the_sum_avg( 0 ),the_sum_stddev( 0 ),the_sum_skew( 0 ),the_sum_kurt( 0 ),
  the_min( numeric_limits<T>::max() ),
  the_max( numeric_limits<T>::min() ),
  offset( 0.0 ),
  normalize( 1.0 )  //,
  // avg_ratios_between_stats( nullopt )
{}
/** @brief stats class constructor
 *
 */
template<typename T>stats<T>::stats(T* avg_ratios_between_stats):
  nbre_points( 0 ),
  the_sum_avg( 0 ),the_sum_stddev( 0 ),the_sum_skew( 0 ),the_sum_kurt( 0 ),
  the_min( numeric_limits<T>::max() ),
  the_max( numeric_limits<T>::min() ),
  offset( 0.0 ),
  normalize( 1.0 ),
  avg_ratios_between_stats( avg_ratios_between_stats )
  {}

/** @brief Add a new value for the statistics
 *
 * It computes all the powers and the additions
 * The code such as X * X has been preferred to pow( x, 2 )
 */
template<typename T>stats<T>&stats<T>::operator+=(const T&input_val)
{
  T val = input_val - offset;
  val /= normalize;
  nbre_points += 1;
  the_sum_avg += val;
  T val_power_N = val * val;
  the_sum_stddev += val_power_N;
  val_power_N *= val;
  the_sum_skew += val_power_N;
  val_power_N *= val;
  the_sum_kurt += val_power_N;  
  if ( val > the_max )
	the_max = val;
  if ( val < the_min )
	the_min = val;
  return*this;
}


template<typename T>string stats<T>::Basic_display()
{
  T the_avg( the_sum_avg / nbre_points );
  T the_stddev =  sqrt( the_sum_stddev / nbre_points - the_avg * the_avg ) ;

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display
  if ( avg_ratios_between_stats )
	{
	  if ( *(*avg_ratios_between_stats) == 0.0 )
		*(*avg_ratios_between_stats) = the_avg;
	  return format("{: E}, {: E}, {:1.3E}, {: E}",
					the_max - the_min, the_avg, the_stddev, the_avg / *(*avg_ratios_between_stats) );
	}
	else
	{
	  return format("{: E}, {: E}, {:1.3E},         ",
					the_max - the_min, the_avg, the_stddev);

	}
}

template<typename T>string stats<T>::Display_without_offset_normalize()const
{
  T the_avg( the_sum_avg / nbre_points );
  T the_stddev =  sqrt( the_sum_stddev / nbre_points - the_avg * the_avg ) ;

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display

  return format("{: E}, {: E}, {:1.3E}",
				the_max * normalize + offset - the_min * normalize + offset,
				the_avg * normalize + offset,
				the_stddev * normalize + offset); 
}

template<typename T>string stats<T>::Display_arccos_degrees()const
{
  T the_avg( the_sum_avg / nbre_points );
  T the_stddev =  sqrt( the_sum_stddev / nbre_points - the_avg * the_avg ) ;

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display

  return format("{: E}, {: E}, {:1.3E}",
				acos( the_max * normalize + offset) * 360.0 / ( 2.0 * numbers::pi ) -
				acos( the_min * normalize + offset) * 360.0 / ( 2.0 * numbers::pi ),
				acos( the_avg * normalize + offset) * 360.0 / ( 2.0 * numbers::pi ),
				the_stddev * normalize + offset); 
}

template<typename T>string stats<T>::Display_arccos_Nth_turns()const
{
  T the_avg( the_sum_avg / nbre_points );

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display

  return format("{: E}",
				( 2.0 * numbers::pi ) / acos( the_avg * normalize + offset) );
}
