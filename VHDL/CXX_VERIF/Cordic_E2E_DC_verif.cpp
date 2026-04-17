#include "Cordic_E2E_DC_verif.hxx"

/** @file Cordic_E2E_DC_verif.hxx
 * @brief Run a formal verification
 *
 * There is no more documentation, for future information, see the .hxx file documentation
 */

/** @brief InitialValueData constructor
 *
 * Constructs for reg_size equal to 32
 * Other sizes are not yet supported, see in the .hxx file
 */
InitialValueData::InitialValueData(const int&X_init,const int&Y_init,bool sanity_check):
  X_init(X_init),Y_init(Y_init)
{
  if ( sanity_check )
	{
	  // Check if the numbers are 31 bits
	  if ( ( X_init & 0xc0000000 ) == 0x80000000 || ( X_init & 0xc0000000 ) == 0x40000000 )
		throw length_error("The initial X value is out of range, should be signed 31 bits ");
	  if ( ( Y_init & 0xc0000000 ) == 0x80000000 || ( Y_init & 0xc0000000 ) == 0x40000000 )
		throw length_error("The initial Y value is out of range, should be signed 31 bits ");
	}
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

SimulDataType::SimulDataType(const long double&module_vector,
							 const unsigned short &Z_2_0_stages,const unsigned short&Y_2_0_stages)
{
  unsigned short ind;
  // Compute how much the vector should grow in module.
  double Z_2_0_cumul_cos = 1.0;
  for ( ind = 0; ind < Z_2_0_stages; ind ++ )
	// The inversion is done at the end for performance reasons
	Z_2_0_cumul_cos *= cos( atan( 1.0 / (float) pow( 2, ind + 1))); 
  // The expected module value is the initial module divided by the cosines
  Z_2_0.check_module_constant.SetOffset( module_vector / Z_2_0_cumul_cos );
  // The expected module value is the initial module divided by the cosines
  cout << Z_2_0_stages << '\t' << module_vector << " * " << 1.0 / Z_2_0_cumul_cos << " = " << module_vector / Z_2_0_cumul_cos << '\t';
  // The expected Z value is 0
  Z_2_0.check_Z_converges.SetOffset( 0.0 );


  double Y_2_0_cumul_cos = 1.0;
  for ( ind = 0; ind < Y_2_0_stages; ind ++ )
	Y_2_0_cumul_cos *= cos( atan( 1.0 / (float)pow( 2, ind + 1 )));
  // The expected X value is the initial module divided by the cosines of both Z and Y to 0
  Y_2_0.check_X_converges.SetOffset( module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ));
  cout << ", " << module_vector << " * " << 1.0 / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ) << " = " << module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ) << endl;
  // The expected Y value is 0
  Y_2_0.check_Y_converges.SetOffset( 0.0 );
}


optional<unsigned int> SimulDataType::GetAndCheck_nbre_points()const
{
  unsigned int nbre_points = (unsigned int)Z_2_0.check_module_constant;
  if ( (unsigned int)Z_2_0.check_Z_converges != nbre_points )
	return nullopt;
  if ( (unsigned int)Y_2_0.check_X_converges != nbre_points )
	return nullopt;
  if ( (unsigned int)Y_2_0.check_Y_converges != nbre_points )
	return nullopt;
  return nbre_points;
}


int main()
{
  vector<InitialValueData>theInitialData = { 
	InitialValueData( 0x3fffffff, 0 ),
	InitialValueData( 0, 0x3fffffff ),
	InitialValueData( 0xc0000000, 0 ),
	InitialValueData( 0, 0xc0000000 )
  };
  vector<SimulDataType>theSimulData;

  auto chrono_start = chrono::high_resolution_clock::now();
  transform( // execution::par,
			 theInitialData.begin(), theInitialData.end(),
			 back_inserter(theSimulData),
			 [&](const InitialValueData&dat) {
			   
			   unsigned long long ind,ind2;

	  cxxrtl_design::p_Cordic__E2E__DC__CXX__test top;

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

	  SimulDataType simulData(sqrt((long double)dat.GetModuleSquared()),
							  top.p_nbre__Z__2__0__stages__out.get<unsigned short>(),
							  top.p_nbre__Y__2__0__stages__out.get<unsigned short>());

	  top.p_input__X.set<decltype(dat.value_type())>(dat.Get_X_init_31());
	  top.p_input__Y.set<decltype(dat.value_type())>(dat.Get_Y_init_31());

	  cout << '(' << dat.Get_X_init_32() << ',' << dat.Get_Y_init_32() << ")\t";  

	  cout << "Running until the reset and the input values propagated to the output ..." << endl;
	  cout.flush();
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
				  else if ( ind_Y == ((unsigned long)top.p_nbre__Y__2__0__stages__out.get<short>() + 7 ) )
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

	  for ( ind = 0; ind < 40 * 24 * 33 ; ind ++ )
		{
		  if ( ind % ( 10 * 24 * 33 ) == 0 )
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
			unsigned char octave_Z_2_0 = top.p_metadata__Z__2__0__octave.get<unsigned char>();
			unsigned char note_Z_2_0 = top.p_metadata__Z__2__0__note.get<unsigned char>();
			pair< unsigned char, unsigned char > key_ON_Z_2_0 = make_pair( octave_Z_2_0, note_Z_2_0 );

			// cout << (unsigned short)octave_Z_2_0 << ',' << (unsigned short)note_Z_2_0 << " \t";

			// Spin the vector
			decltype(dat.value_type()) X_Z_2_0 = top.p_X__Z__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Y_Z_2_0 = top.p_Y__Z__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Z_Z_2_0 = top.p_Z__Z__2__0.get<decltype(dat.value_type())>();

			InitialValueData currentPoint_Z_2_0( X_Z_2_0, Y_Z_2_0, false);

			simulData.Z_2_0.check_module_constant +=
			  sqrt( (decltype(dat.module_value_type()))currentPoint_Z_2_0.GetModuleSquared()) ;			

			// cout << X_Z_2_0 << ',' << Y_Z_2_0 << ':';

			simulData.Z_2_0.check_Z_converges += (float)Z_Z_2_0;
			// cout << '\t' << Z_Z_2_0;

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
				pair<InitialValueData,stats<long double>>&data_info =
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

			// Now bring back the vector to the X axis
			unsigned char octave_Y_2_0 = top.p_metadata__Y__2__0__octave.get<unsigned char>();
			unsigned char note_Y_2_0 = top.p_metadata__Y__2__0__note.get<unsigned char>();
			// TEMP TEMP Looks like there is a shift between the meta data and the data
			pair< unsigned char, unsigned char > key_ON_Y_2_0 = make_pair( octave_Y_2_0, note_Y_2_0 );

			// cout << (unsigned short)octave_Y_2_0 << ',' << (unsigned short)note_Y_2_0 << " \t";

			decltype(dat.value_type()) X_Y_2_0 = top.p_X__Y__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Y_Y_2_0 = top.p_Y__Y__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Z_Y_2_0 = top.p_Z__Y__2__0.get<decltype(dat.value_type())>();
			simulData.Y_2_0.check_X_converges += (float)X_Y_2_0;
			// cout << '\t' << X_Y_2_0;
			simulData.Y_2_0.check_Y_converges += (float)Y_Y_2_0;
			// cout << '\t' << Y_Y_2_0 << endl;
			if ( simulData.Y_2_0.check_spin_per_ON_constant.contains(key_ON_Y_2_0) )
			  {
				// Found, then process the diff, replace the old value and add the diff in the statistics
				pair<unsigned int, stats<double>>&data_info =
				  simulData.Y_2_0.check_spin_per_ON_constant.find( key_ON_Y_2_0 )->second;
				data_info.second += (double)(((unsigned int)Z_Y_2_0) - data_info.first);
				data_info.first = (unsigned int)Z_Y_2_0;
				//cout << 'y';
			  }
			else
			  {
				// Not found, create the records and initialise the statistics
				stats<double>theNewYStats;
				// Set the value that should be found
				// TODO
				simulData.
				  Y_2_0.
				  check_spin_per_ON_constant.
				  insert(make_pair(key_ON_Y_2_0,make_pair((unsigned int)Z_Y_2_0,theNewYStats)));
				//cout << 'Y';
			  }
		  }
		}
	  cout << endl;
	  return simulData;
	});

  auto chrono_end = chrono::high_resolution_clock::now();
  auto chrono_duration = chrono::duration_cast<chrono::milliseconds>(chrono_end-chrono_start);
  cout << "Duration: " << chrono_duration.count() << " mS" << endl;
  
  cout << "Checking the Z to 0 first set of stages" << endl;
  cout << "Number       X,Y module to the grown input module                                Z to 0 " << endl; 
  cout << "of points         min average max standard dev                        min average max standard dev " << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](SimulDataType&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << (string)dat.Z_2_0.check_module_constant << '\t';
				  cout << (string)dat.Z_2_0.check_Z_converges << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
			// Now display the octave note specific results
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](SimulDataType&dat){

			for_each( execution::seq,
					  dat.Z_2_0.check_scalar_prod_per_ON_constant.begin(),
					  dat.Z_2_0.check_scalar_prod_per_ON_constant.end(),
					  [](const auto&ON_iter){
						cout << "O: " << (unsigned short)ON_iter.first.first <<
						  ", N: " << (unsigned short)ON_iter.first.second << '\t';
						cout << (unsigned int)ON_iter.second.second << '\t';
						cout << (string)ON_iter.second.second;
						cout << endl;
					  });
			cout << endl;
			});
  cout << endl;
  cout << "Checking the Y to 0 second set of stages" << endl;
  cout << "Number       X to the grown input module                                         Y to 0"<< endl;
  cout << "of points         min average max standard dev                        min average max standard dev " << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const SimulDataType&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << (string)dat.Y_2_0.check_X_converges << '\t';
				  cout << (string)dat.Y_2_0.check_Y_converges << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
			// Now display the octave note specific results
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](const SimulDataType&dat){
			for_each( execution::seq,
					  dat.Y_2_0.check_spin_per_ON_constant.begin(),
					  dat.Y_2_0.check_spin_per_ON_constant.end(),
					  [](const auto&ON_iter){
						cout << "O: " << (unsigned short)ON_iter.first.first <<
						  ", N: " << (unsigned short)ON_iter.first.second << '\t';
						cout << (unsigned int)ON_iter.second.second << '\t';
						cout << (string)ON_iter.second.second;
						cout << endl;
					  });
			cout << endl;
			});
 }
