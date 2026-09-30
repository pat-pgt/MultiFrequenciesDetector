#include "Cordic_E2E_lightfilter_verif.hxx"

#ifdef Cordic_E2E_lightfilter_CXX_noDS
#error "This is a non-sense. The light filter assumes the is at least the first down-sampling level"
#elif Cordic_E2E_lightfilter_CXX_DS1
#include <Cordic_E2E_lightfilter_CXX_DS1.cpp>
#elif Cordic_E2E_lightfilter_CXX_DS2
#include <Cordic_E2E_lightfilter_CXX_DS2.cpp>
#else
#error "Not all down-sampling rates VHDL CXX tests are compiled"
#endif


#include "Utils_CXX_verif.cpp"


#include <getopt.h>


template <typename stats_type, typename stats_long_type,typename cxx_reg_type, unsigned short reg_size>
void SimulDataType<stats_type,stats_long_type,cxx_reg_type,reg_size>::
Init(const stats_long_type&module_vector,
			  const unsigned short &Z_2_0_stages,const unsigned short&Y_2_0_stages)
{
  unsigned short ind;
  // Compute how much the vector should grow in module.
  stats_long_type Z_2_0_cumul_cos = 1.0;
  for ( ind = 0; ind < Z_2_0_stages; ind ++ )
	// The inversion is done at the end for performance reasons
	Z_2_0_cumul_cos *= cos( atan( 1.0 / (float) pow( 2, ind + 1))); 
  // The expected module value is the initial module divided by the cosines
  cout << Z_2_0_stages << '\t' << module_vector << " * " << 1.0 / Z_2_0_cumul_cos << " = " << module_vector / Z_2_0_cumul_cos << '\t';


  stats_long_type Y_2_0_cumul_cos = 1.0;
  for ( ind = 0; ind < Y_2_0_stages; ind ++ )
	Y_2_0_cumul_cos *= cos( atan( 1.0 / (float)pow( 2, ind + 1 )));
  cout << ", " << module_vector << " * " << 1.0 / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ) << " = " << module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ) << endl;
  // The expected Y value is 0, no offset, but normalize to compare to X at the end
  Y_2_0.check_Y_converges.SetNormalize( module_vector / ( Z_2_0_cumul_cos * Y_2_0_cumul_cos ));
  // Diff Z value, no offset
}


template <typename stats_type, typename stats_long_type,typename cxx_reg_type, unsigned short reg_size>
optional<unsigned int> SimulDataType<stats_type,stats_long_type,cxx_reg_type,reg_size>::
GetAndCheck_nbre_points()const
{
  return (unsigned int)Y_2_0.check_Y_converges;
}


void PrintHelp()
{

}

extern char*optarg;

int main(int argc,char*argv[])
{
  int opt;
  bool has_hv = false;

  /** Full cycles number
   * 
   * This should be small for a fast simulation
   * It should be large for good simulations
   * A pretty good value is the one in which the lowest note of the lowest octave
   *   is going to make a full spin
   * The value should be at least 2 to run the differences.
   */
  unsigned short half_cycles_number = 20;
  unsigned short nbre_initial_vextors = 1;
  bool input_DC = false;

  while((opt = getopt( argc,argv,"cl:n:hv"))!=EOF)
	switch( opt)
	  {
	  case 'c':
		input_DC = true;
		break;
	  case 'n':
		nbre_initial_vextors = atoi(optarg);
		break;
	  case 'l':
		half_cycles_number = atoi(optarg);
		break;
	  case 'h':
		PrintHelp();
		has_hv = true;
		break;
	  case 'v':
		cout << "Cordic_E2E_lightfilter_verif  v1.0RC1" << endl;
		has_hv = true;
		break;
	  }

  /* Only to get a runable veriosn, to be changed later
   *
   * The initial data is only one vector
   * the simulation runs for multiple cuttoff frequencies
   */
  Value_Data<int,32>dat = Value_Data<int,32>( 0x3fffffff );
  vector<unsigned char>freq_octave_list = { 2, 4 };

  if ( has_hv )
	return -1;
  if ( half_cycles_number < 20 )
	{
	  // There is a need to spin the clock at lot
	  //   as the filters need to converge to their values.
	  // There is a warm up before collecting the results, see below.
	  cout << "-l The number of half cycles should not be lower then 20" << endl;
	  return -2;
	}
  /** In some cases, the same result is expected for different initial values.
   *  A statistic (of the statistic) displays one time. TODO TODO
   * However, the detail can be displayed
   */
  const bool display_details = true;

  cout << "--------------------------------------------------------------------------------------------" << endl;
  cout << "All the results issued here, are with a minimum explanations to reduce the number of lines." << endl;
  cout << "The documentation is inside the code, especially after this paragraph." << endl;
  cout << "--------------------------------------------------------------------------------------------" << endl;
  cout << "The tests results are displayed as: for each test, for each initial data, do it" << endl;
  cout << "If something is wrong in a test, it is irrelevant to go future." << endl; 
  cout << "--------------------------------------------------------------------------------------------" << endl;

  vector<SimulDataType<double,long double,int,32>>theSimulData;
  for( auto ind_ID : freq_octave_list )
	theSimulData.push_back(SimulDataType<double,long double,int,32>());


  auto chrono_start = chrono::high_resolution_clock::now();
  /** Run all the test of all the initial values and the Down-sampling
   *    and place them in structures.
   *  Indeed a future version is going to run in parallel on multiple CPU machines
   */
  transform( execution::par,
			 freq_octave_list.begin(), freq_octave_list.end(),
			 theSimulData.begin(),
			 [&](const unsigned char&freq_octave) {

			   unsigned long long strobe_stable_1=0, strobe_stable_0=0;

#ifdef Cordic_E2E_lightfilter_CXX_noDS
#error "This is a non-sense. The light filter assumes the is at least the first down-sampling level"
#elif Cordic_E2E_lightfilter_CXX_DS1
			   Cordic_E2E_lightfilter_1::p_Cordic__E2E__lightfilter__CXX__test top;
			   const unsigned char with_downsampling = 1;
#elif Cordic_E2E_lightfilter_CXX_DS2
			   Cordic_E2E_lightfilter_2::p_Cordic__E2E__lightfilter__CXX__test top;
			   const unsigned char with_downsampling = 2;
#else
#error "Not all down-sampling rates VHDL CXX tests are compiled"
#endif

			   /** Spin the clock in order to run the reset
				*/
			   cout << "Resetting the circuits ..." << endl;
			   cout.flush();
			   top.p_RST.set<bool>(true);
			   unsigned short ind_reset;
			   for (ind_reset = 0; ind_reset < 50; ind_reset++ )
				 {
				   top.p_CLK.set<bool>(true);
				   top.step();
				   top.p_CLK.set<bool>(false);
				   top.step();
				 }
			   top.p_RST.set<bool>(false);

			   if ( dat.GetRegSize() != top.p_reg__size__4__verif.get<unsigned char>() )
				 throw length_error("The C++ and the VHDL registers size mismatch");

			   SimulDataType<double,long double,int,32> simulData;
			   simulData.Init(sqrt((long double)dat.GetValueSquared()),
							  top.p_nbre__Z__2__0__stages__out.get<unsigned short>(),
							  top.p_nbre__Y__2__0__stages__out.get<unsigned short>());

			   /** Since many input angles has been tested in the end to end DC test
				*  We consider, here, only the X=input, Y=0 or the X=0, Y=input
				*  TODO TODO
				*/
			   bool input_Value_isNeg = false;
			   auto set_input_values=[&top,&input_Value_isNeg,&dat](){
				 top.p_the__input.set<decltype(dat.value_type())>
				    (dat.Get_Positive_Negative_Value(input_Value_isNeg));
				 //				 cout << '>' << dat.Get_Positive_Negative_Value(input_Value_isNeg) << " ";
				 //				 cout.flush();
				 top.p_input__x__not__y.set<bool>(true);
			   };

			   set_input_values();

			   cout << '(' << (int)dat << ")\t";  

			   cout << "Running until the reset and the input values propagated to the output ..." << endl;
			   cout.flush();
			   /** Run until the input reaches the output.
				   It is mostly a number of frames (reg_sync) equal to
				   the sum of the number of Z to 0 stages plus the number of Y to 0 stages
				   plus 6, plus 7 and plus 3 to count the first stages, the last stages and the down-sampling
			   */
			   /** This may be removed.
				   The code below is intended to wait until the filter are stabilized.
				   Then this one is redundant.
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
			   /* Despite the end to end DC test, which should test a couple of full Z spins of the lowest frequency,
				*   this should consider the time to converge to the destination.
				* In this test, the input signals are a square of the lowest note at the octave under test.
				*/


			   simulData.octave_of_input = freq_octave;
			   /* To avoid a verification of the frequency (done elsewhere), a first "dry-run"
				*   similar to the end to end DC test checks how many cycles are required to get
				*   an half turn of the Z output
				*   X and Y start with the same 0 initial value as they are not.
				*   As soon as the simetry is reached, the module exits with the lenth of the half periode
				*/
			   long input_half_periode_according_X;
			   long input_half_periode_according_Y;
			   unsigned long HP_counter = 0;
			   cout << "Spin the clock to find the half periode of the octave " << (unsigned short)freq_octave << " note 0" << endl;
			   enum XY_per_state { wait4start, wait4first1, isstarted, isdone };
			   XY_per_state X_per_state;
			   XY_per_state Y_per_state;
			   bool is_Y_started = false;
			   unsigned short freq_detec_max = 3;
			   do {
				 X_per_state = XY_per_state::wait4start;
				 Y_per_state = XY_per_state::wait4start;
				 input_half_periode_according_X = 0;
				 input_half_periode_according_Y = 0;
			   do {
				  top.p_CLK.set<bool>(true);
				  top.step();
				  top.p_CLK.set<bool>(false);
				  top.step();

				  if ( top.p_reg__sync.get<bool>() == true )
					{
					  HP_counter += 1;
					  if ( top.p_metadata__prefilter__1__note.get<unsigned char>() == 0  )
						   //						   top.p_metadata__Y__2__0__strobe.get<bool>() == true )
						if ( top.p_metadata__prefilter__1__octave.get<unsigned char>() == freq_octave )
						  {
							decltype(dat.value_type()) perdetec_X = top.p_X__prefilter__1.get<decltype(dat.value_type())>();
							bitset<32>bsX(perdetec_X);
							// cout << setfill('0') << setw(8) << hex << perdetec_X << ':' ;
							// cout << setfill('0') << setw(8) << hex << perdetec_Y << "  " ;
							// cout.flush();
							if ( bsX.test( 32 - 1 ) )
							  {
								// A 1 is received, set to started
								if ( X_per_state == XY_per_state::wait4first1 )
								  {
									X_per_state = XY_per_state::isstarted;
									input_half_periode_according_X = HP_counter;
								  }
							  } else
							  {
								// The counting was started, it ends here owing a return to 0
								if ( X_per_state == XY_per_state::isstarted )
								  {
									input_half_periode_according_X = HP_counter - input_half_periode_according_X;
									X_per_state = XY_per_state::isdone;
								  }
								// We can now wait for a one
								if ( X_per_state == XY_per_state::wait4start )
								  X_per_state = XY_per_state::wait4first1;
							  }
							decltype(dat.value_type()) perdetec_Y = top.p_Y__prefilter__1.get<decltype(dat.value_type())>();
							bitset<32>bsY(perdetec_Y);
							// cout << setfill('0') << setw(8) << hex << perdetec_X << ':' ;
							// cout << setfill('0') << setw(8) << hex << perdetec_Y << "  " ;
							// cout.flush();
							// No comments hee. For more details see the comments of the X, above.
							if ( bsY.test( 32 - 1 ) )
							  {
								if ( Y_per_state == XY_per_state::wait4first1 )
								  {
									Y_per_state = XY_per_state::isstarted;
									input_half_periode_according_Y = HP_counter;
								  }
							  } else
							  {
								if ( Y_per_state == XY_per_state::isstarted )
								  {
									input_half_periode_according_Y = HP_counter - input_half_periode_according_Y;
									Y_per_state = XY_per_state::isdone;
								  }
								if ( Y_per_state == XY_per_state::wait4start )
								  Y_per_state = XY_per_state::wait4first1;
							  }
						  }
					}
			   }while( false ); // while ( ( X_per_state != XY_per_state::isdone ) || ( Y_per_state != XY_per_state::isdone ) );
			   cout << "Frequency found at octave " << dec << (unsigned short)freq_octave;
			   cout << ", requires " << input_half_periode_according_X;
			   cout << "  " << input_half_periode_according_Y;
			   cout << " reg_sync cycles for a half periode " << endl;
			   if ( freq_detec_max > 0 )
				 freq_detec_max -= 1;
			   // Both values are displayed to verify the test.
			   // Some unbalance can occur. It is due to the arithmetics rounding errors
			   } while ( ( abs( input_half_periode_according_X - input_half_periode_according_Y ) >
						   input_half_periode_according_X / 200 )  && freq_detec_max > 0 );
			   unsigned long input_half_periode = ( input_half_periode_according_X + input_half_periode_according_Y ) / 2;


			   // According with the end to end DC test, octave 4 note 0 takes 128 reg_sync to spin one full turn.
			   // Then the helf is 64. Since there are 4 notes and 6 octaves, the result is:
			   input_half_periode = 24576 / pow( 2, freq_octave );
			   cout << "To speed up the simulation, set to " << input_half_periode << endl;

			   unsigned char note_max(1);
			   unsigned char octave_max(1);
			   unsigned long full_cycle_loop;
			   unsigned short ind_half_cycles;
			   unsigned long extra_cycles_downsampling;
			   /** TODO compute the value
				*
				* 
				*/
			   for ( ind_half_cycles = 0; ind_half_cycles < half_cycles_number ; ind_half_cycles ++ )
				 {
				   // For now the input changes at each full cycle
				   if( ind_half_cycles % 2 == 0 )
					 {
					   input_Value_isNeg =  false;
					   cout << '-';
					 } else {
					 if ( input_DC )
					   input_Value_isNeg = false;
					 else
					   input_Value_isNeg = true;
					 cout << '_';
				   }

				   full_cycle_loop = 0;
				   do {
 					 dat = Value_Data<int,32>( (signed long)( 0x3fffffff * sin(full_cycle_loop*2*numbers::pi/(input_half_periode*2))) );
					 //					 dat = Value_Data<int,32>( 0x3fffffff );

					 cout.flush();
					 set_input_values();


					 top.p_CLK.set<bool>(true);
					 top.step();
					 top.p_CLK.set<bool>(false);
					 top.step();

					 if ( top.p_strobe__stable.get<bool>() )
					   strobe_stable_1 +=1;
					 else
					   strobe_stable_1 +=0;

					 if ( top.p_reg__sync.get<bool>() == true )
					   {
						 full_cycle_loop += 1;

						 // Start a litle bit later to stabilise the filters
						 if ( ind_half_cycles > (half_cycles_number / 2) )
						   {
							 /** Fetch Z to confirm it works
							  */
							 decltype(dat.value_type()) Z_pref_1 = top.p_Z__prefilter__1.get<decltype(dat.value_type())>();

							 simulData.prefilter_1.confirm_Z_2_0 += (float)Z_pref_1;

							 // cout << '\t' << Z_pref_1;

							 decltype(dat.value_type()) X_pref_1 = top.p_X__prefilter__1.get<decltype(dat.value_type())>();
							 decltype(dat.value_type()) Y_pref_1 = top.p_Y__prefilter__1.get<decltype(dat.value_type())>();

							 XY_Data<int,32> currentPoint_pref_1( X_pref_1, Y_pref_1);
							 /** Get the octave note couple, as it is independent statistics. The frequencies are different.
							  *  Please note, the modules are checked above, then the two modules are supposed to be equal.
							  *  The result is added to the statistics.
							  *  The strobe is discarded here as it should always be on.
							  */
							 unsigned char octave_pref_1 = top.p_metadata__prefilter__1__octave.get<unsigned char>();
							 unsigned char note_pref_1 = top.p_metadata__prefilter__1__note.get<unsigned char>();
							 pair< unsigned char, unsigned char > key_ON_pref_1 = make_pair( octave_pref_1, note_pref_1 );
							 // Update, if so, the maximums numbers of octave or notes
							 // It is done only once here, has some value can be lost but none can appear later
							 if ( octave_pref_1 > octave_max )
							   octave_max = octave_pref_1;
							 if ( note_pref_1 > note_max )
							   note_max = note_pref_1;

							 // cout << (unsigned short)octave_pref_1 << ',' << (unsigned short)note_pref_1 << " \t";
							 if ( simulData.prefilter_1.check_module_per_ON.contains(key_ON_pref_1) )
							   {
								 /** The following code displays the high digits of X and Y of the note 3
								  *    as an array of octaves columns.
								  *  It is intended to debug the test software and/or check the meta data fits the values.
								  *  Uncomment it if needed.
								  */
								 /*
								   if ( key_ON_pref_1.second == 3 )
								   {
								   cout << (unsigned short)key_ON_pref_1.first << ": " << currentPoint_pref_1.string_light() << '\t';
								   if ( key_ON_pref_1.first == 5 )
								   cout << endl;
								   }
								 */

								 // Found, then process the diff, replace the old value and add the diff in the statistics
								 pair<XY_Data<int,32>,stats<long double>>&data_info =
								   simulData.prefilter_1.check_module_per_ON.find( key_ON_pref_1 )->second;
								 data_info.second += sqrt( (decltype(dat.module_value_type()))
														   currentPoint_pref_1.GetModuleSquared());
								 data_info.first = currentPoint_pref_1;
								 // cout << 'z';
							   }
							 else
							   {
								 // Not found, create the records and initialize the statistics
								 stats<long double>theNewPrefStats;
								 // Set the value that should be found
								 theNewPrefStats += sqrt( (decltype(dat.module_value_type()))
														  currentPoint_pref_1.GetModuleSquared());
								 simulData.
								   prefilter_1.
								   check_module_per_ON.
								   insert(make_pair(key_ON_pref_1,make_pair(currentPoint_pref_1,theNewPrefStats)));
								 // cout << 'Z';
							   }
	
						 
							 /** TODO second part */
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
							   simulData.Y_2_0.check_Y_converges += (float)Y_Y_2_0;
							   // cout << '\t' << Y_Y_2_0;
							 }


							 // TEMP TEMP Looks like there is a shift between the meta data and the data
							 // A branch makes a quick and dirty temporary fix.
							 pair< unsigned char, unsigned char > key_ON_Y_2_0 = make_pair( octave_Y_2_0, note_Y_2_0 );
							 if ( simulData.Y_2_0.check_X_converges_per_ON.contains(key_ON_Y_2_0) )
							   {
								 stats<double>&data_info = simulData.Y_2_0.check_X_converges_per_ON.find(key_ON_Y_2_0)->second;
								 if ( octave_Y_2_0 != numeric_limits<decltype(octave_Y_2_0)>::max() ||
									  note_Y_2_0 != numeric_limits<decltype(note_Y_2_0)>::max() ) {
								   data_info += (float)X_Y_2_0;
								 }else
								   data_info += 0.0;
								 //cout << 'y';
							   }
							 else
							   {
								 // Not found, create the records and initialize the statistics
								 // Set the value that should be found
								 // TODO
								 stats<double>new_stats;
								 new_stats += (double)X_Y_2_0;
								 simulData.
								   Y_2_0.
								   check_X_converges_per_ON.
								   insert(make_pair(key_ON_Y_2_0,new_stats));
								 //cout << 'Y';
							   }
						   } // ind_half_cycles > half_cycles_number / 2
					   } // top.p_reg__sync.get<bool>() == true
					 /* count for full cycles
					  *
					  */
					 // It looks like bad to recalculate at each iteration (including the max notes and octaves),
					 // The circuit and the statistics are so complex and resource consuming,
					 //   this is neglectable.

				   } while( full_cycle_loop < input_half_periode);
				 } // Main for loop

			   cout << endl;

			   cout << "Strobe stable was " << strobe_stable_1 << " times '1', and " << strobe_stable_0 << " times '0'" << endl; 

			   return simulData;

			 });

  auto chrono_end = chrono::high_resolution_clock::now();
  auto chrono_duration = chrono::duration_cast<chrono::milliseconds>(chrono_end-chrono_start);
  cout << "Duration: " << chrono_duration.count() << " mS" << endl;

  /********************************************************************************************/
  /*                          Now display all the structures                                  */
  /********************************************************************************************/
  /** @brief What should be care about.
   *
   * The test is an "infinite impulse response".
   * Each step of the DUT depends on what happened before.
   * There is a dilemma between pushing the simulation and using more configurations.\n
   * In case of a doubt of a result, one can try to push more simulation.\n
   * Since the link between the mathematics and the implementations is not yet done,
   *   the shifts of the filter is an "arbitrary" value.
   * The day this link is done, a way to forge is going to be kept.
   * By this way, a higher frequency can be used to not cut everything.
   * 1) All the first part simulation should be done, at least, using the barrel shifter and the direct access
   *   memory module.
   * The result should be the same (for the same parameters).\n
   * 2) All the test should be done with multiple number of prefilter stages.
   * For every frequency, the attenuation ratio should be power of the number of stages.
   * 3) The DC input sends the sinusoid in the filters.
   * Since the filter cut-off frequencies as low as the frequency, the output should be slightly constant.\n
   * 4) The sinusoidal input should produce a peek at the corresponding octave note,
   *   and produce a decay on both sides.\n\n
   *
   * This is an early version.\n
   * All the parameters should go into a class rather than to recompile
   *   and rather than to have a single octave number. 
   * A new test angle generator or an injector should be written
   *   to send some frequencies in random order and to send 0 for some others.
   * The goal is to avoid to miss some bugs with correlated data.\n
   * It give a pretty good idea of the resources for the project if the main filter can be estimate and added.
   */

  cout << "Checking the Z to 0 and the module after the first set of stages and the pre-filter" << endl;
  cout << "Number                 Z to 0 degrees                                       Z to 0 integer" << endl; 
  cout << "of points         max-min average standard dev                        max-min average standard dev" << endl;
	for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			  [](auto&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << dat.prefilter_1.confirm_Z_2_0.Basic_display() << "\t\t";
				  cout << dat.prefilter_1.confirm_Z_2_0.Display_without_offset_normalize() << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
	cout << endl;
	// Now display the octave note specific results
	// The number of samples are always minus 1 as they are differences
	cout << "Module of X and Y after the prefilter" << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](auto&dat){

			for_each( execution::seq,
					  dat.prefilter_1.check_module_per_ON.begin(),
					  dat.prefilter_1.check_module_per_ON.end(),
					  [&dat](auto&ON_iter){
						if ( ON_iter.first.first == dat.octave_of_input && ON_iter.first.second == 0 )
						  cout << "* ";
						else
						  cout << "  ";
						cout << "O: " << (unsigned short)ON_iter.first.first <<
						  ", N: " << (unsigned short)ON_iter.first.second << '\t';
						cout << (unsigned int)ON_iter.second.second << '\t';
						cout << ON_iter.second.second.Basic_display() << "\t\t";
						cout << ON_iter.second.second.Display_arccos_degrees() << "\t\t";
						cout << ON_iter.second.second.Display_arccos_Nth_turns();
						cout << endl;
					  });
			cout << endl;
			});


  cout << endl;
  cout << "-------------------------------------------------------------------------------------------------" << endl;
  cout << "Checking the Y to 0 second set of stages" << endl;
  cout << "Number            Y to 0 integer                                    Y to 0 ratio from X"<< endl;
  cout << "of points         max-min average standard dev                      max-min average standard dev " << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](auto&dat){
			  if ( dat.GetAndCheck_nbre_points() )
				{
				  cout << "  " << *dat.GetAndCheck_nbre_points() << ",  ";
				  cout << dat.Y_2_0.check_Y_converges.Display_without_offset_normalize() << "\t\t";
				  cout << dat.Y_2_0.check_Y_converges.Basic_display() << endl;
				}
			  else
				cout << "Problem: the number of points is not the same for all the tests" << endl;
			});
  cout << endl;
  // Now display the octave note specific results
  cout << "Number            X value after the filter                              integer" << endl; 
  cout << "of points         max-min average standard dev                      max-min average standard dev" << endl;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[](auto&dat){
			for_each( execution::seq,
					  dat.Y_2_0.check_X_converges_per_ON.begin(),
					  dat.Y_2_0.check_X_converges_per_ON.end(),
					  [&dat](auto&ON_iter){
						if ( ON_iter.first.first != numeric_limits<decltype(ON_iter.first.first)>::max() ||
							 ON_iter.first.second != numeric_limits<decltype(ON_iter.first.second)>::max() ) {
						  if ( ON_iter.first.first == dat.octave_of_input && ON_iter.first.second == 0 )
							cout << "* ";
						  else
							cout << "  ";
						  cout << "O: " << (unsigned short)ON_iter.first.first;
						  cout << ", N: " << (unsigned short)ON_iter.first.second << '\t';
						  cout << (unsigned int)ON_iter.second << '\t';
						  cout << ON_iter.second.Basic_display() << "\t\t";
						  cout << ON_iter.second.Display_without_offset_normalize();
						}else{
						  cout << "  O: /, N: /\t";
						  cout << (unsigned int)ON_iter.second;
						}
						cout << endl;
					  });
			cout << endl;
			});

  /*
TODO create object and include in the loop above
  set<pair<unsigned char, unsigned char>> check_spin_per_ON_constant_keys;
  for_each( execution::seq,
			theSimulData.begin(), theSimulData.end(),
			[&](auto&dat){
			  check_spin_per_ON_constant_keys.insert( views::keys(ON_iter.first ));
			});
  for_each( check_spin_per_ON_constant_keys.begin(), check_spin_per_ON_constant_keys.end(),
			[](const pair<unsigned char, unsigned char>&the_ON){
			  cout << the_ON.first << " " << the_ON.second << "   ";
			});
  cout << endl;
  */
  return 0;
}
