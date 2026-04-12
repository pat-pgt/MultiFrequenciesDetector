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
 * The offset and the normalization are not initialized.
 * It should be done. Otherwise, the software crashes
 */
template<typename T>stats<T>::stats():
  nbre_points( 0 ),
  the_sum_avg( 0 ),the_sum_stddev( 0 ),the_sum_skew( 0 ),the_sum_kurt( 0 ),
  the_min( numeric_limits<T>::max() ),
  the_max( numeric_limits<T>::min() )
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

  return format("{: E}, {: 1.3E}, {: E}, {:E}",
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
			   
	  unsigned long long ind;

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
				  else if ( ind_Y == ((unsigned long)top.p_nbre__Y__2__0__stages__out.get<short>() + 5 ) )
					{
					  // cout << "Y converges to " << top.p_Y__Y__2__0.get<decltype(dat::value_type())>();
					  // cout << "\tat the first valid time " << ind_Y << " after Z" << endl;
					  ind_Y = numeric_limits<decltype(ind_Y)>::max();
					}
				  else
					ind_Y += 1;
				}
			  else if ( ind_Z == ((unsigned long)top.p_nbre__Z__2__0__stages__out.get<short>() + 5 ))
				{
				  // cout << "Z converges to " << top.p_Z__Z__2__0.get<decltype(dat::value_type())>();
				  // cout << "\tat the first valid time " << ind_Z << endl;
				  ind_Z = numeric_limits<decltype(ind_Z)>::max();
				}
			  else
				ind_Z += 1;
			}
		}

	  for ( ind = 0; ind < 20 * 33 ; ind ++ )
		{
		  top.p_CLK.set<bool>(true);
		  top.step();
		  top.p_CLK.set<bool>(false);
		  top.step();
		  if ( top.p_reg__sync.get<bool>() == true )
		  {
			decltype(dat.value_type()) X_Z_2_0 = top.p_X__Z__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Y_Z_2_0 = top.p_Y__Z__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Z_Z_2_0 = top.p_Z__Z__2__0.get<decltype(dat.value_type())>();
			InitialValueData currentPoint( X_Z_2_0, Y_Z_2_0, false);
			simulData.Z_2_0.check_module_constant +=
			  sqrt( (decltype(dat.module_value_type()))currentPoint.GetModuleSquared()) ;
			// cout << X_Z_2_0 << '\t' << Y_Z_2_0;
			simulData.Z_2_0.check_Z_converges += (float)Z_Z_2_0;
			// cout << '\t' << Z_Z_2_0;

			decltype(dat.value_type()) X_Y_2_0 = top.p_X__Y__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Y_Y_2_0 = top.p_Y__Y__2__0.get<decltype(dat.value_type())>();
			decltype(dat.value_type()) Z_Y_2_0 = top.p_Z__Y__2__0.get<decltype(dat.value_type())>();
			simulData.Y_2_0.check_X_converges += (float)X_Y_2_0;
			// cout << '\t' << X_Y_2_0;
			simulData.Y_2_0.check_Y_converges += (float)Y_Y_2_0;
			// cout << '\t' << Y_Y_2_0 << endl;
		  }
		}
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
				  cout << format("{:06}", *dat.GetAndCheck_nbre_points()) << ",\t";
				  cout << (string)dat.Z_2_0.check_module_constant << '\t';
				  cout << (string)dat.Z_2_0.check_Z_converges << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
  cout << "Checking the Y to 0 second set of stages" << endl;
  cout << "Number       X to the grown input module                                         Y to 0"<< endl;
  cout << "of points         min average max standard dev                        min average max standard dev " << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](SimulDataType&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << format("{:06}", *dat.GetAndCheck_nbre_points()) << ",\t";
				  cout << (string)dat.Y_2_0.check_X_converges << '\t';
				  cout << (string)dat.Y_2_0.check_Y_converges << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
}
