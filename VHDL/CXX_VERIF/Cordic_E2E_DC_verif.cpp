#include <Cordic_E2E_DC_CXX.cpp>

#include <iostream>
#include <limits>
using namespace std;


int main()
{
  cxxrtl_design::p_Cordic__E2E__DC__CXX__test top;

  // Set the data

  int val_1 = 0x20000000;
  int val_2 = 0;
  top.p_input__X.set<int>(val_1);
  top.p_input__Y.set<int>(val_2);

  // Reset the circuit

  top.p_RST.set<bool>(true);
  unsigned long ind;
  for (ind = 0; ind < 50; ind++ )
	{
	  top.p_CLK.set<bool>(true);
	  top.step();
	  top.p_CLK.set<bool>(false);
	  top.step();
	}
  top.p_RST.set<bool>(false);

  unsigned short ind_Z, ind_Y;
  for (ind = 0; ind < 50000; ind++ )
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
				  cout << "Y connverges to " << top.p_Y__Y__2__0.get<int>();
				  cout << "\tat the first valid time " << ind_Y << endl;
				  ind_Y = numeric_limits<decltype(ind_Y)>::max();
				}
			  else
				ind_Y += 1;
			}
		  else if ( ind_Z == ((unsigned long)top.p_nbre__Z__2__0__stages__out.get<short>() + 5 ))
			{
			  cout << "Z converges to " << top.p_Z__Z__2__0.get<int>();
			  cout << "\tat the first valid time " << ind_Z << endl;
			  ind_Z = numeric_limits<decltype(ind_Z)>::max();
			}
		  else
			ind_Z += 1;
		}
	}
  

}
