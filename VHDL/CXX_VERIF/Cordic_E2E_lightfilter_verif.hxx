#include "Utils_CXX_verif.hxx"

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


