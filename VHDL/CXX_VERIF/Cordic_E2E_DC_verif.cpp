#include "Cordic_E2E_DC_verif.hxx"

/* This code work ONLY for 32 bits reg_size.
 * That means, the input is 31 bits.
 * For other size, it should be moved from int / long long to bit_wise
 */

InitialValueData::InitialValueData(const int&X_init,const int&Y_init):
  X_init(X_init),Y_init(Y_init)
{
  // Check if the numbers are 31 bits
  if ( ( X_init & 0xc0000000 ) == 0x80000000 || ( X_init & 0xc0000000 ) == 0x40000000 )
	throw length_error("The initial X value is out of range, should be signed 31 bits ");
  if ( ( Y_init & 0xc0000000 ) == 0x80000000 || ( Y_init & 0xc0000000 ) == 0x40000000 )
	throw length_error("The initial Y value is out of range, should be signed 31 bits ");
}


template<typename T>stats<T>::stats():
  nbre_points( 0 ),
  the_sum_avg( 0 ),the_sum_stddev( 0 ),the_sum_skew( 0 ),the_sum_kurt( 0 ),
  the_min( numeric_limits<T>::max() ),
  the_max( numeric_limits<T>::min() )
  // Initialisation of the offset and the normalization:
  // not default value, 0 or other, crash if it is not done
{}

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
  T the_stddev =  the_sum_stddev / nbre_points - the_avg * the_avg ;

  // TODO find the formula to get the kurtosis and the skew

  return format("Based on {:06} values, avg={:1.3E}, std dev={:E}",
				nbre_points, the_avg, the_stddev); 
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

	  SimulDataType simulData(sqrt((long double)dat.GetModuleSquared()),
							  top.p_nbre__Z__2__0__stages__out.get<unsigned short>(),
							  top.p_nbre__Y__2__0__stages__out.get<unsigned short>());

	  top.p_input__X.set<int>(dat.Get_X_init_31());
	  top.p_input__Y.set<int>(dat.Get_Y_init_31());

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
					  //					  cout << "Y converges to " << top.p_Y__Y__2__0.get<int>();
					  // cout << "\tat the first valid time " << ind_Y << " after Z" << endl;
					  ind_Y = numeric_limits<decltype(ind_Y)>::max();
					}
				  else
					ind_Y += 1;
				}
			  else if ( ind_Z == ((unsigned long)top.p_nbre__Z__2__0__stages__out.get<short>() + 5 ))
				{
				  // cout << "Z converges to " << top.p_Z__Z__2__0.get<int>();
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
			int X_Z_2_0 = top.p_X__Z__2__0.get<int>();
			int Y_Z_2_0 = top.p_Y__Z__2__0.get<int>();
			int Z_Z_2_0 = top.p_Z__Z__2__0.get<int>();
			simulData.Z_2_0.check_module_constant += sqrt(
														  (long double)(((long long)X_Z_2_0) * ((long long)X_Z_2_0)) +
														  (long double)(((long long)Y_Z_2_0) * ((long long)Y_Z_2_0)));
			// cout << X_Z_2_0 << '\t' << Y_Z_2_0;
			simulData.Z_2_0.check_Z_converges += (float)Z_Z_2_0;
			// cout << '\t' << Z_Z_2_0;

			int X_Y_2_0 = top.p_X__Y__2__0.get<int>();
			int Y_Y_2_0 = top.p_Y__Y__2__0.get<int>();
			int Z_Y_2_0 = top.p_Z__Y__2__0.get<int>();
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
  
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](SimulDataType&dat){
			  cout << (string)dat.Z_2_0.check_module_constant << '\t';
			  cout << (string)dat.Z_2_0.check_Z_converges << '\t';
			  cout << (string)dat.Y_2_0.check_X_converges << '\t';
			  cout << (string)dat.Y_2_0.check_Y_converges << endl;
			});
}
