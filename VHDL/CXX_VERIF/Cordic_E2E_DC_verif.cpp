#include "Cordic_E2E_DC_verif.hxx"


template<typename T>stats<T>::stats():
  nbre_points( 0 ),
  the_sum_avg( 0 ),the_sum_stddev( 0 ),the_sum_skew( 0 ),the_sum_kurt( 0 ),
  the_min( numeric_limits<T>::max() ),
  the_max( numeric_limits<T>::min() )
{}

template<typename T>stats<T>&stats<T>::operator+=(const T&val)
{
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
  T the_stddev = the_sum_stddev / nbre_points - the_avg * the_avg;

  // TODO find the formula to get the kurtosis and the skew

  return format("Based on {:06} values, avg={:1.3E}, std dev={:E}",
				nbre_points, the_avg, the_stddev); 
}

struct SimulDataType
  {
	const int X_init;
	const int Y_init;
	
	SimulDataType() = delete;
	// TODO throw exception if more than 31 bits
	SimulDataType(const int&X_init,const int&Y_init);

	const float GetInitAngle()const;

	struct {
	  stats<double>check_module_constant;
	  stats<float>check_Z_converges;
	}  Z_2_0;
	struct {
	  stats<float>check_X_converges;
	  stats<float>check_Y_converges;
	}  Y_2_0;
};
SimulDataType::SimulDataType(const int&X_init,const int&Y_init):
  X_init(X_init),Y_init(Y_init)
{}


int main()
{
  array<SimulDataType,4>theSimulData = { 
	SimulDataType( 0x3fffffff, 0 ),
	SimulDataType( 0, 0x3fffffff ),
	SimulDataType( 0x40000000, 0 ),
	SimulDataType( 0, 0x40000000 )
  };


  auto chrono_start = chrono::high_resolution_clock::now();
  for_each( execution::par,
			 theSimulData.begin(), theSimulData.end(),
			 [&](SimulDataType&dat){

	  unsigned long long ind;

	  cxxrtl_design::p_Cordic__E2E__DC__CXX__test top;

	  top.p_input__X.set<int>(dat.X_init);
	  top.p_input__Y.set<int>(dat.Y_init);
	  cout << dat.X_init << '\t' << dat.Y_init << '\t';  

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
					  //					  cout << "Y connverges to " << top.p_Y__Y__2__0.get<int>();
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
			dat.Z_2_0.check_module_constant += (double)(((long long)X_Z_2_0) * ((long long)X_Z_2_0))
			  + (double)(((long long)Y_Z_2_0) * ((long long)Y_Z_2_0));
			cout << X_Z_2_0 << '\t' << Y_Z_2_0;
			dat.Z_2_0.check_Z_converges += (float)Z_Z_2_0;
			cout << '\t' << Z_Z_2_0;

			int X_Y_2_0 = top.p_X__Y__2__0.get<int>();
			int Y_Y_2_0 = top.p_Y__Y__2__0.get<int>();
			int Z_Y_2_0 = top.p_Z__Y__2__0.get<int>();
			dat.Y_2_0.check_X_converges += (float)X_Y_2_0;
			cout << '\t' << X_Y_2_0;
			dat.Y_2_0.check_Y_converges += (float)Y_Y_2_0;
			cout << '\t' << Y_Y_2_0 << endl;
		  }
		}
	});

  auto chrono_end = chrono::high_resolution_clock::now();
  auto chrono_duration = chrono::duration_cast<chrono::milliseconds>(chrono_end-chrono_start);
  cout << "Duration: " << chrono_duration.count() << " mS" << endl;
  
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](SimulDataType&dat){
			  cout << (string)dat.Z_2_0.check_module_constant << '\t';
			  cout << (string)dat.Z_2_0.check_Z_converges << '\t';
			  cout << (string)dat.Y_2_0.check_Y_converges << '\t';
			  cout << (string)dat.Y_2_0.check_Y_converges << endl;
			});
}
