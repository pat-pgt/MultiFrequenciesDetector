#include "Cordic_E2E_DC_verif.hxx"

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
  normalize( 1.0 )
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


template<typename T>stats<T>::operator string()const
{
  T the_avg( the_sum_avg / nbre_points );
  T the_stddev =  sqrt( the_sum_stddev / nbre_points - the_avg * the_avg ) ;

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display

  return format("{: E}, {: E}, {: E}, {:1.3E}",
				the_min, the_avg, the_max, the_stddev); 
}

template<typename T>string stats<T>::Display_without_offset_normalize()const
{
  T the_avg( the_sum_avg / nbre_points );
  T the_stddev =  sqrt( the_sum_stddev / nbre_points - the_avg * the_avg ) ;

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display

  return format("{: E}, {: E}, {: E}, {:1.3E}",
				the_min * normalize + offset,
				the_avg * normalize + offset,
				the_max * normalize + offset,
				the_stddev * normalize + offset); 
}

template<typename T>string stats<T>::Display_arccos_degrees()const
{
  T the_avg( the_sum_avg / nbre_points );
  T the_stddev =  sqrt( the_sum_stddev / nbre_points - the_avg * the_avg ) ;

  // TODO find the formula to get the kurtosis and the skew

  // TODO improve the display

  return format("{: E}, {: E}, {: E}, {:1.3E}",
				acos( the_min * normalize + offset) * 360.0 / numbers::pi,
				acos( the_avg * normalize + offset) * 360.0 / numbers::pi,
				acos( the_max * normalize + offset) * 360.0 / numbers::pi,
				the_stddev * normalize + offset); 
}

template <typename stats_type, typename stats_long_type,typename cxx_reg_type, unsigned short reg_size>
SimulDataType<stats_type,stats_long_type,cxx_reg_type,reg_size>::
SimulDataType(const long double&module_vector,
			  const unsigned short &Z_2_0_stages,const unsigned short&Y_2_0_stages)
{
  unsigned short ind;
  // Compute how much the vector should grow in module.
  stats_long_type Z_2_0_cumul_cos = 1.0;
  for ( ind = 0; ind < Z_2_0_stages; ind ++ )
	// The inversion is done at the end for performance reasons
	Z_2_0_cumul_cos *= cos( atan( 1.0 / (float) pow( 2, ind + 1))); 
  // The expected module value is the initial module divided by the cosines
  Z_2_0.check_module_constant.SetOffset( module_vector / Z_2_0_cumul_cos );
  // The expected module value is the initial module divided by the cosines
  cout << Z_2_0_stages << '\t' << module_vector << " * " << 1.0 / Z_2_0_cumul_cos << " = " << module_vector / Z_2_0_cumul_cos << '\t';
  // The expected Z value is 0, no offset
  Z_2_0.check_Z_converges.SetNormalize( pow( 2, reg_size ) / 360 );


  stats_long_type Y_2_0_cumul_cos = 1.0;
  for ( ind = 0; ind < Y_2_0_stages; ind ++ )
	Y_2_0_cumul_cos *= cos( atan( 1.0 / (float)pow( 2, ind + 1 )));
  // The expected X value is the initial module divided by the cosines of both Z and Y to 0
  Y_2_0.check_X_converges.SetOffset( module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ));
  cout << ", " << module_vector << " * " << 1.0 / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ) << " = " << module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ) << endl;
  // The expected Y value is 0, no offset, but normalize to compare to X at the end
  Y_2_0.check_Y_converges.SetNormalize( module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ));
  // Diff Z value, no offset
}


template <typename stats_type, typename stats_long_type,typename cxx_reg_type, unsigned short reg_size>
optional<unsigned int> SimulDataType<stats_type,stats_long_type,cxx_reg_type,reg_size>::
GetAndCheck_nbre_points()const
{
  unsigned int nbre_points = (unsigned int)Z_2_0.check_module_constant;
  if ( (unsigned int)Z_2_0.check_Z_converges != nbre_points )
	return nullopt;

  nbre_points = (unsigned int)Y_2_0.check_X_converges;
  if ( (unsigned int)Y_2_0.check_Y_converges != nbre_points )
	return nullopt;
  return nbre_points;
}


int main()
{
  /** In some cases, the same result is expected for different initial values.
   *  A statistic (of the statistic) displays one time. TODO TODO
   * However, the detail can be displayed
   */
  const bool display_details = true;

  cout << "--------------------------------------------------------------------------------------------" << endl;
  cout << "All the results issued here, are with a minimum explanations to reduce the number of lines." << endl;
  cout << "The documentation is inside the code, especially after this paragraph." << endl;
  cout << "--------------------------------------------------------------------------------------------" << endl;
  cout << "The tests results are displayed as for each test for each initial data do it" << endl;
  cout << "If something is wrong in a test, it is irrelevant to go future." << endl; 
  cout << "--------------------------------------------------------------------------------------------" << endl;

  vector<InitialValueData<int,32>>theInitialData = { 
	InitialValueData<int,32>( 0x3fffffff, 0 ),
	//	InitialValueData<int,32>( 0, 0x3fffffff ),
	// InitialValueData<int,32>( 0xc0000000, 0 ),
	InitialValueData<int,32>( 0, 0xc0000000 )
  };
  vector<SimulDataType<double,long double,int,32>>theSimulData;

  auto chrono_start = chrono::high_resolution_clock::now();
  /** Run all the test of all the initial values and the Down-sampling
   *    and place them in structures.
   *  Indeed a future version is going to run in parallel on multiple CPU machines
   */
  transform( // execution::par,
			 theInitialData.begin(), theInitialData.end(),
			 back_inserter(theSimulData),
			 [&](const InitialValueData<int,32>&dat) {
			   
			   unsigned long long ind,ind2;

	  cxxrtl_design::p_Cordic__E2E__DC__CXX__test top;

	  /** Spin the clock in order to run the reset
	   */
	  cout << "Resetting the circuits ..." << endl;
	  cout.flush();
	  top.p_RST.set<bool>(true);
	  for (ind = 0; ind < 50; ind++ )
		{
		  top.p_CLK.set<bool>(true);
		  top.step();
		  top.p_CLK.set<bool>(false);
		  top.step();
		}
	  top.p_RST.set<bool>(false);

	  if ( dat.GetRegSize() != top.p_reg__size__4__verif.get<unsigned char>() )
		throw length_error("The C++ and the VHDL registers size mismatch");

	  SimulDataType<double,long double,int,32> simulData(sqrt((long double)dat.GetModuleSquared()),
							  top.p_nbre__Z__2__0__stages__out.get<unsigned short>(),
							  top.p_nbre__Y__2__0__stages__out.get<unsigned short>());

	  /** Set the DC value for all the simulation.
	   * The Y is used for this test as well, even if in the run mode it is set to 0
	   */
	  top.p_input__X.set<decltype(dat.value_type())>(dat.Get_X_init_2divided());
	  top.p_input__Y.set<decltype(dat.value_type())>(dat.Get_Y_init_2divided());

	  cout << '(' << dat.Get_X_full() << ',' << dat.Get_Y_full() << ")\t";  

	  cout << "Running until the reset and the input values propagated to the output ..." << endl;
	  cout.flush();
	  /** Run until the DC input reaches the output.
		  It is mostly a number of frames (reg_sync) equal to
		    the sum of the number of Z to 0 stages plus the number of Y to 0 stages
		    plus 6, plus 7 and plus 3 to count the first stages, the last stages and the down-sampling
	   */
	  unsigned short ind_Z = 0, ind_Y = 0;
	  while ( ind_Y != numeric_limits<decltype(ind_Y)>::max() )
		{
		  top.p_CLK.set<bool>(true);
		  top.step();
		  top.p_CLK.set<bool>(false);
		  top.step();
		  if ( top.p_reg__sync.get<bool>() == true )
			{
			  if ( ind_Z == numeric_limits<decltype(ind_Z)>::max() )
				{
				  if ( ind_Y == numeric_limits<decltype(ind_Y)>::max() )
					{}
				  else if ( ind_Y == ((unsigned long)top.p_nbre__Y__2__0__stages__out.get<short>() + 7 + 3 ) )
					{
					  // cout << "Y converges to " << top.p_Y__Y__2__0.get<decltype(dat::value_type())>();
					  // cout << "\tat the first valid time " << ind_Y << " after Z" << endl;
					  ind_Y = numeric_limits<decltype(ind_Y)>::max();
					}
				  else
					ind_Y += 1;
				}
			  else if ( ind_Z == ((unsigned long)top.p_nbre__Z__2__0__stages__out.get<short>() + 6 ))
				{
				  // cout << "Z converges to " << top.p_Z__Z__2__0.get<decltype(dat::value_type())>();
				  // cout << "\tat the first valid time " << ind_Z << endl;
				  ind_Z = numeric_limits<decltype(ind_Z)>::max();
				}
			  else
				ind_Z += 1;
			}
		}
	  /** TODO replace the values by data collected from thee VHDL
	   *
	   * Today:
	   * 33 = 32 + 1 clock cycles for the frame
	   * 24 = 4 notes * 6 octaves
	   * 2**6 cycles are required for a complete cycle
	   * about 30 to run 30 times
	   */
	  for ( ind = 0; ind < 2000 * 24 * 33 ; ind ++ )
		{
		  if ( ind % ( 50 * 24 * 33 ) == 0 )
			{
			  cout << '.';
			  cout.flush();
			}


		  top.p_CLK.set<bool>(true);
		  top.step();
		  top.p_CLK.set<bool>(false);
		  top.step();

		  if ( top.p_reg__sync.get<bool>() == true )
		  {
			/** Fetch the X, Y, Z
			 *  Add the module of X and Y, which should increase of 16% from the initial value module, to the statistic
			 *  Add the Z value, which should converge to 0 to the statistics
			 *  These data should be the same regardless the frequency
			 *  These data should be the same regardless the down-sampling is active or not.
			 */
			decltype(dat.value_type()) X_Z_2_0 = top.p_X__Z__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Y_Z_2_0 = top.p_Y__Z__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Z_Z_2_0 = top.p_Z__Z__2__0.get<decltype(dat.value_type())>();

			XY_Data<int,32> currentPoint_Z_2_0( X_Z_2_0, Y_Z_2_0);

   			simulData.Z_2_0.check_module_constant +=
			  sqrt( (decltype(dat.module_value_type()))currentPoint_Z_2_0.GetModuleSquared()) ;			

			// cout << X_Z_2_0 << ',' << Y_Z_2_0 << ':';

			simulData.Z_2_0.check_Z_converges += (float)Z_Z_2_0;
			// cout << '\t' << Z_Z_2_0;

			/** Get the octave note couple, as it is independent statistics. The frequencies are different.
			 *  Each angle cosine value is stored in the object for the next occurrence.
			 *  The previous one is retrieved to compute the scalar product, and divided by the module power 2.
			 *  Please note, the modules are checked above, then the two modules are supposed to be equal.
			 *  The result is added to the statistics.
			 *  The strobe is discarded here as it should always be on.
			 */
			unsigned char octave_Z_2_0 = top.p_metadata__Z__2__0__octave.get<unsigned char>();
			unsigned char note_Z_2_0 = top.p_metadata__Z__2__0__note.get<unsigned char>();
			pair< unsigned char, unsigned char > key_ON_Z_2_0 = make_pair( octave_Z_2_0, note_Z_2_0 );

			// cout << (unsigned short)octave_Z_2_0 << ',' << (unsigned short)note_Z_2_0 << " \t";
			if ( simulData.Z_2_0.check_scalar_prod_per_ON_constant.contains(key_ON_Z_2_0) )
			  {
				/** The following code displays the high digits of X and Y of the note 3
				 *    as an array of octaves columns.
				 *  It is intended to debug the test software and/or check the meta data fits the values.
				 *  Uncomment it if needed.
				 */
				/*
				  if ( key_ON_Z_2_0.second == 3 )
				  {
				  cout << (unsigned short)key_ON_Z_2_0.first << ": " << currentPoint_Z_2_0.string_light() << '\t';
				  if ( key_ON_Z_2_0.first == 5 )
				  cout << endl;
				  }
				*/

				// Found, then process the diff, replace the old value and add the diff in the statistics
				pair<XY_Data<int,32>,stats<long double>>&data_info =
				  simulData.Z_2_0.check_scalar_prod_per_ON_constant.find( key_ON_Z_2_0 )->second;
				data_info.second += currentPoint_Z_2_0.GetCosine(data_info.first);
				data_info.first = currentPoint_Z_2_0;
				// cout << 'z';
			  }
			else
			  {
				// Not found, create the records and initialise the statistics
				stats<long double>theNewZStats;
				// Set the value that should be found
				// TODO
				simulData.
				  Z_2_0.
				  check_scalar_prod_per_ON_constant.
				  insert(make_pair(key_ON_Z_2_0,make_pair(currentPoint_Z_2_0,theNewZStats)));
				// cout << 'Z';
			  }

			/** Now bring back the vector to the X axis
			 *  This part depends if the Down-sampling is active or not
			 */

			// cout << (unsigned short)octave_Y_2_0 << ',' << (unsigned short)note_Y_2_0 << " \t";

			/** Fetch the X, Y, Z
			 */
			decltype(dat.value_type()) X_Y_2_0 = top.p_X__Y__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Y_Y_2_0 = top.p_Y__Y__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Z_Y_2_0 = top.p_Z__Y__2__0.get<decltype(dat.value_type())>();

			/** Get the octave note couple, as it is independent statistics. The frequencies are different.
			 *  Each Z value is stored in the object for the next occurrence.
			 *  The previous one is retrieved to compute the difference.
			 *  The result is added to the statistics.
			 *
			 *  In the case the strobe is off, the result is irrelevant.
			 *  It enter into the statistics with the octave and the note set to the maximum value.
			 *  It does not look like clean. However it is a good check to keep them,
			 *    in order to display the number of occurrence.
			 */
			unsigned char octave_Y_2_0 = top.p_metadata__Y__2__0__octave.get<unsigned char>();
			unsigned char note_Y_2_0 = top.p_metadata__Y__2__0__note.get<unsigned char>();
			if ( top.p_metadata__Y__2__0__strobe.get<bool>() == false )
			  {
				// We should find a way to populate cleanly according to the N_octaves and N_notes
				octave_Y_2_0 = numeric_limits<decltype(octave_Y_2_0)>::max();
				note_Y_2_0 = numeric_limits<decltype(note_Y_2_0)>::max();
			  }

			
			/**  Add the X value, which should increase of 32% from the initial value module, to the statistic
			 *  Add the Y value, which should converge to 0 to the statistics
			 *  These data should be the same regardless the frequency
			 */
			if ( octave_Y_2_0 != numeric_limits<decltype(octave_Y_2_0)>::max() ||
				 note_Y_2_0 != numeric_limits<decltype(note_Y_2_0)>::max() ) {
			  simulData.Y_2_0.check_X_converges += (float)X_Y_2_0;
			  // cout << '\t' << X_Y_2_0;
			  simulData.Y_2_0.check_Y_converges += (float)Y_Y_2_0;
			  // cout << '\t' << Y_Y_2_0 << endl;
			}


			// TEMP TEMP Looks like there is a shift between the meta data and the data
			// A branch makes a quick and dirty temporary fix.
			pair< unsigned char, unsigned char > key_ON_Y_2_0 = make_pair( octave_Y_2_0, note_Y_2_0 );
			if ( simulData.Y_2_0.check_spin_per_ON_constant.contains(key_ON_Y_2_0) )
			  {
				// Found, then process the diff, replace the old value and add the diff in the statistics
				pair<int, stats<double>>&data_info =
				  simulData.Y_2_0.check_spin_per_ON_constant.find( key_ON_Y_2_0 )->second;
				if ( octave_Y_2_0 != numeric_limits<decltype(octave_Y_2_0)>::max() ||
					 note_Y_2_0 != numeric_limits<decltype(note_Y_2_0)>::max() ) {
				  data_info.second += (double)((unsigned int)Z_Y_2_0 - (unsigned int)data_info.first);
				  data_info.first = Z_Y_2_0;
				}else
				  {
					data_info.second += 0.0;
					data_info.first = 0;
				  }
				//cout << 'y';
			  }
			else
			  {
				// Not found, create the records and initialise the statistics
				stats<double>theNewYStats;
				theNewYStats.SetNormalize( pow( 2, dat.GetRegSize() ) / 360 );
				// Set the value that should be found
				// TODO
				simulData.
				  Y_2_0.
				  check_spin_per_ON_constant.
				  insert(make_pair(key_ON_Y_2_0,make_pair(Z_Y_2_0,theNewYStats)));
				//cout << 'Y';
			  }
		  } // top.p_reg__sync.get<bool>() == true
		} // Main for loop
	  cout << endl;
	  return simulData;
	});

  auto chrono_end = chrono::high_resolution_clock::now();
  auto chrono_duration = chrono::duration_cast<chrono::milliseconds>(chrono_end-chrono_start);
  cout << "Duration: " << chrono_duration.count() << " mS" << endl;

  //TODO explain the results display
  
  cout << "Checking the Z to 0 first set of stages" << endl;
  cout << "Number             X,Y module delta from grow                       X,Y module absolute" << endl; 
  cout << "of points         min average max standard dev                   min average max standard dev" << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const auto&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << (string)dat.Z_2_0.check_module_constant << "\t\t";
				  cout << dat.Z_2_0.check_module_constant.Display_without_offset_normalize() << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
  cout << "Number                 Z to 0 degrees                                       Z to 0 integer" << endl; 
  cout << "of points         min average max standard dev                        min average max standard dev" << endl;
	for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			  [](const auto&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << (string)dat.Z_2_0.check_Z_converges << "\t\t";
				  cout << dat.Z_2_0.check_Z_converges.Display_without_offset_normalize() << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
			// Now display the octave note specific results
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const auto&dat){

			for_each( execution::seq,
					  dat.Z_2_0.check_scalar_prod_per_ON_constant.begin(),
					  dat.Z_2_0.check_scalar_prod_per_ON_constant.end(),
					  [](const auto&ON_iter){
						cout << "O: " << (unsigned short)ON_iter.first.first <<
						  ", N: " << (unsigned short)ON_iter.first.second << '\t';
						cout << (unsigned int)ON_iter.second.second << '\t';
						cout << (string)ON_iter.second.second << "\t\t";
						cout << ON_iter.second.second.Display_arccos_degrees();
						cout << endl;
					  });
			cout << endl;
			});
  cout << endl;
  cout << "Checking the Y to 0 second set of stages" << endl;
  cout << "Number            X module delta from grow                          X module absolute"<< endl;
  cout << "of points         min average max standard dev                      min average max standard dev " << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const auto&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << (string)dat.Y_2_0.check_X_converges << "\t\t";
				  cout << dat.Y_2_0.check_X_converges.Display_without_offset_normalize() << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
  cout << "Number            Y to 0 integer                                    Y to 0 ratio from X"<< endl;
  cout << "of points         min average max standard dev                      min average max standard dev " << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const auto&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << (string)dat.Y_2_0.check_Y_converges << "\t\t";
				  cout << dat.Y_2_0.check_Y_converges.Display_without_offset_normalize() << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
			// Now display the octave note specific results
  cout << "Number            Z rotation angle degrees                          Z rotation angle integer" << endl; 
  cout << "of points         min average max standard dev                      min average max standard dev" << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const auto&dat){
			for_each( execution::seq,
					  dat.Y_2_0.check_spin_per_ON_constant.begin(),
					  dat.Y_2_0.check_spin_per_ON_constant.end(),
					  [](const auto&ON_iter){
						if ( ON_iter.first.first != numeric_limits<decltype(ON_iter.first.first)>::max() ||
							 ON_iter.first.second != numeric_limits<decltype(ON_iter.first.second)>::max() ) {
						  cout << "O: " << (unsigned short)ON_iter.first.first <<
							", N: " << (unsigned short)ON_iter.first.second << '\t';
						}else
						  cout << "O: /, N: /\t";
						cout << (unsigned int)ON_iter.second.second << '\t';
						cout << (string)ON_iter.second.second << "\t\t";
						cout << ON_iter.second.second.Display_without_offset_normalize() << endl;
					  });
			cout << endl;
			});

  /*
TODO create object and include in the loop above
  set<pair<unsigned char, unsigned char>> check_spin_per_ON_constant_keys;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[&](const auto&dat){
			  check_spin_per_ON_constant_keys.insert( views::keys(ON_iter.first ));
			});
  for_each( check_spin_per_ON_constant_keys.begin(), check_spin_per_ON_constant_keys.end(),
			[](const pair<unsigned char, unsigned char>&the_ON){
			  cout << the_ON.first << " " << the_ON.second << "   ";
			});
  cout << endl;
  */
}
