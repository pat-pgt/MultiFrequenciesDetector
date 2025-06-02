
library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.MultiFreqDetect_package.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Frequencies.all;

--! @brief Pre-filter 
--!
--! This computes the prefilter
--!   to remove high frequencies with a minimum of resources.\n
--! There are first order infinite impulse response filters
--!   using 1/2**N as a coefficient.\n
--! Since the final filtering is very low (against the sampling rate),
--!   a down-sampling follows.\n
--! That reduces the final filtering resources because
--! * the low pass frequency is closer than the sampling rate,
--!   that reduces the requested number of bits of the operands.
--! * there are less multiplication and additions.\n
--! ***********************************************|||**
--! *               WARNING                            *
--! * Some additional assertions have to be written    *
--! * To validate the shifts "detection" and execution *
--! ****************************************************
package PreFilter_package is
--! In most of the ASIC generators, the RAM data size is what one wants.
--! For the FPGA the standard libraries are, in general, 1, 2, 4, 8, 16. 
  constant ram_data_size : positive := 16;

--! @brief Data structure for the 2**N division in the IIR pre-filter
--!
--! Since the arithmetic operations are done on an arithm_size bloc at a time,
--!   and since a counter may be inadequate (propagation too long),
--!   the detailed type may change during the design.
  type shifts_IIR_data is record
    the_shifts : std_logic_vector(4 downto 0);
  end record shifts_IIR_data;

--! @brief Pre-filter meta-data to coeff compute
--!
--! This entity computes:
--! * The delay of the metadata to through out.
--!     The IIR filter has a latency of 3 registers (+1)
--! * The number of shifts needed.
--! It is a separate one as it is required only once for the sine and the cosine.
  component Prefilter_metadata_and_shifts_compute is
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      --
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      shifts_calc   : out shifts_IIR_data);
  end component Prefilter_metadata_and_shifts_compute;


--! @brief Pre-filter IIR compute
--!
--! This a a dummy component to include some documentation
--!   used both by the 3 computation components.\n
--! They should be bundle by pair to compute the sine and the cosine.\n
--! The computation should be something like
  --! output( N + 1 ) = 0.1 .input + 0.9 . output( N )
  --! To avoid multiplications, the formula is transformed to:*
  --! output( N + 1 ) = output( N ) + 0.1 . ( input - output( N ) )
  --! Since it is a prefilter, only the inverted power of 2
  --! are used as coefficients.
  --!
  --! The computation is in 3 steps:
  --! * input - state variable is computed
  --!   the note and octave are computed to get a shifts number
  --! * this signal is shifted
  --! * The shifted signal is added to the state variable
  component Prefilter_IIR_stage_dummy is
    port (
      CLK              : in  std_logic;
      state_var_in     : in  reg_type
      );
  end component Prefilter_IIR_stage_dummy;



--! @brief Pre-filter IIR compute the diff
--!
--! This component computes the diff beween
--!   the input and the state variable.\n
--! Even with data lost by the shift,
--!   It is slightly better and
--!   and wouldn't consume too much resources
--!   to perform the calculation on the full precision.
--! For more information about the calculation,
--!   see Prefilter_IIR_stage_dummy.
  component Prefilter_IIR_stage_diff is
    port (
      CLK              : in  std_logic;
      RST              : in  std_logic;
      reg_sync         : in  std_logic;
      state_var_in     : in  reg_type;
      data_input_in    : in  reg_type;
      data_out         : out reg_type
      );
  end component Prefilter_IIR_stage_diff;
--! @brief Pre-filter IIR compute the shifts
--!
--! This component computes the shift
--!   to divide by 2**N.\n
--! It may need, in the future a configure statement
--!   to switch different architectures
--!   for different groups of configuration.
--!   See in the entity documentation.\n
--! For more information about the calculation,
--!   see Prefilter_IIR_stage_dummy.
  component Prefilter_IIR_stage_shift is
    port (
      CLK              : in  std_logic;
      RST              : in  std_logic;
      reg_sync         : in  std_logic;
      shifts_calc      : in  shifts_IIR_data;
      data_in          : in  reg_type;
      data_out         : out reg_type
      );
  end component Prefilter_IIR_stage_shift;
--! @brief Pre-filter IIR compute the add
--!
--! This component computes the final addition.\n
--! The result is the output and the new state variable
--!   to be stored into the RAM.\n
--! For more information about the calculation,
--!   see Prefilter_IIR_stage_dummy.
  component Prefilter_IIR_stage_add is
    port (
      CLK                : in  std_logic;
      RST                : in  std_logic;
      reg_sync           : in  std_logic;
      -- This signal has to be delayed
      -- the latency of the diff and the shift
      state_var_in       : in  reg_type;
      data_in            : in  reg_type;
      state_var_data_out : out reg_type
      );
  end component Prefilter_IIR_stage_add;


--! @brief Pre-filter state variable storage
--!
--! Stores in a RAM based barrel shifter two state variables
--!
  component Prefilter_Storage is
    generic (
      --! ram_locations_size : positive := N_notes * N_octaves - 3;
      --! N_notes * N_octaves is the number of IIR filters involved\n
      --! multiplied by 2 because there is the sine and the cosine to handle\n
      --! multiplied by ram_bloc_size because reg_size is lower than ram_data_size,
      --! then there are multiple reads and writes.\n
      --! minus 3 because 3 values are in the pipe-lined process of the pre-filter.
      ram_locations_size : positive := 29
      );
    port (
      CLK        : in  std_logic;
      RST        : in  std_logic;
      reg_sync   : in  std_logic;
      SV_sin_in  : in  reg_type;
      SV_cos_in  : in  reg_type;
      --! Output sine register.\n
      --! To keep a standard inter-modules interface, thees reg_type registers
      --! are shifted by arithm size between the reg_sync (active)
      SV_sin_out : out reg_type;
      --! Output cosine register.\n
      --! To keep a standard inter-modules interface, thees reg_type registers
      --! are shifted by arithm size between the reg_sync (active)
      SV_cos_out : out reg_type
      );
  end component Prefilter_Storage;
  --! @brief Prefilter delay line
  --!
  --! The Prefilter subtraction, and, in some case, the prefilter shift
  --! Introduces a latency.
  --! This component takes the state value, delays it, and provide it for the addition
  component Prefilter_Delay is
    generic(
      latency : positive );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z
      );
  end component Prefilter_Delay;
--! @brief Prefilter stage
--!
--! This is a pair of sine and cosine calculation
--! with their associated memory storage and
--! delay for the meta-data.
  component Prefilter_stage is
    generic (
      the_stage_offset : real := 1.0;
      debug_level : integer range 0 to 2 := 0
      );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z
      );
  end component Prefilter_stage;
--! @brief Prefilter bundle
--!
--! This is the bundle for the whose wants more stages
  component Prefilter_bundle is
    generic (
      --! Defines the number of stages and their offsets ratios
      stages_offsets : prefilter_stages_offset_list;
      debug_level : integer range 0 to 2 := 0);
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z
      );
  end component Prefilter_bundle;


--! @brief
--!
--! This function returns the ratio between the low-pass (pre)filter
--!     and the frequency to detect.
--! If the progression would have been linear,
--!   the low-pass would have been the half of the step.
--! The function returns the ratio, considering the number of notes per octave
--! Since the progression is log, the same ratio can be multiplied
--! to each frequency to get the low-pass 
--! Proofs:
--!   Let's say F is one of the frequencies and N is the number of notes per octave.
--!   Let's say the crossover is at X after F.
--!   Let's say the crossover is at Y before the next frequency at R.F.
--!   X+Y=(R-1).F and Y=R.X . Then X=(R-1)/(R+1).
--!     If the number of notes par octave is 1, X=1/3.F, Y=2/3.F
--!     If the number of notes per octave is 12, X=0.02887.F, Y=0.03059.F
  constant Prefilter_lowpass_on_freq_ratio : real := (2.0 ** (1.0 / real(N_notes)) - 1.0) /
                                                     (2.0 ** (1.0 / real(N_notes)) + 1.0);

--! @brief computes the first order coefficient as a power of 2
--!
--! The iteration of a first order IIR is
--!   Xnext = coeff . Xprev + ( 1 - coeff ). input .\n
--! The goal of the pre-filter is to have simple coefficients
--!   to avoid a multiplication. the inverse of power 2 has been chosen.
--!   Then only shifts are needed.\n
--! Since the meta data contains the octave number, the shifts can be deduced
--!   by a subtraction from an offset by the octave number.\n
--! However, that gives the data for the last note of the octave (the worst case).
--!   For the lowest notes, the frequency can be lowered
--!   before passing to the lower octave.
--!   See the other function Meta_data_2_prefilter_coeff_note.\n
--! This function computes this offset and the ratio between
--!   the cut-off frequency and the desired frequency (see README.modules).
  function Meta_data_2_prefilter_coeff_shifts(constant cutoff_ratio : real) return integer;

--! @brief computes the correction of the first order coefficient
--!
--! The iteration of a first order IIR is
--!   Xnext = coeff . Xprev + ( 1 - coeff ). input .\n
--! Since the coefficients are a power of 2,
--!   calculated on highest note of the octave,
--!   some lower notes could have been filtered at a lower (/2) frequency.\n
--! Since the notes are in progression
--!   from a frequency F included to a frequency 2.F excluded,
--!   This function only gives a threshold to add 1 to the shift.
--!
  function Meta_data_2_prefilter_coeff_note(constant cutoff_ratio : real) return notes_N_range;

--! @brief Private function for Meta_data_2_prefilter_coeff_offset and Meta_data_2_prefilter_coeff_note
--!
--! These 2 functions are, in fact, the integer and the decimal part.
--! This function computes the real value.
  function Meta_data_2_prefilter_coeff_real(constant cutoff_ratio : real) return real;


  constant CLK_cycles_per_sample : positive := ((reg_size / arithm_size + 1) * N_notes * N_octaves);


--  function Prefilter_cnv_metadata_2_shifts(signal metadata_in : meta_data_t; constant ratiofrom_cuttoff : real)
--    return std_logic_vector;

end package PreFilter_package;

package body PreFilter_package is

  function Meta_data_2_prefilter_coeff_shifts(constant cutoff_ratio : real) return integer is
    constant coeff_required : real    := Meta_data_2_prefilter_coeff_real(cutoff_ratio);
    variable coeff_curr     : real    := 1.0;
    variable coeff_shifts   : natural := 0;
  begin
    -- Better to stay at a higher than a lower frequency
    main_loop : while (2.0 * coeff_required) < coeff_curr loop
      coeff_curr   := coeff_curr / 2.0;
      coeff_shifts := coeff_shifts + 1;
    end loop main_loop;

    return coeff_shifts;
  end function Meta_data_2_prefilter_coeff_shifts;

  function Meta_data_2_prefilter_coeff_note(constant cutoff_ratio : real) return notes_N_range is
    constant coeff_required  : real    := Meta_data_2_prefilter_coeff_real(cutoff_ratio);
    variable coeff_by_shifts : real    := 1.0 / 2.0 ** Meta_data_2_prefilter_coeff_shifts(cutoff_ratio);
    constant note_threshold  : natural := 0;
    variable the_return      : notes_N_range;
  begin
    if N_notes < 2 then
      return N_notes;
    end if;
    -- Due to the coeff_shifts function, the required is always lower
    --   than the one taken by shifts.
    -- We are going to divide by the space between notes until it passes under
    main_loop : for ind in N_notes - 1 downto 1 loop
      coeff_by_shifts := coeff_by_shifts / (2.0 ** (real(1) / real(N_notes)));
      main_loop_exit : exit main_loop when coeff_by_shifts < coeff_required;
      the_return      := the_return - 1;
    end loop main_loop;
    return the_return;
  end function Meta_data_2_prefilter_coeff_note;

  function Meta_data_2_prefilter_coeff_real(constant cutoff_ratio : real) return real is
    -- The relative space between notes,
    -- taken at the highest note of the octave, is
    constant cutoff_rel_bandwidth : real := 2.0 - 2.0 ** (real(N_notes - 1) / real(N_notes));
    -- The lowest filtering frequency
    -- is half of the bandwidth for A00 ( 440 / 32 )
    constant cutoff_bandwidth_A00 : real := cutoff_ratio * cutoff_rel_bandwidth * 440.0 / (32.0 * 2.0);
  begin
    assert cutoff_ratio >= 1.0
      report "Cut-off ratio " & real'image(cutoff_ratio) & " lower than 1 is a non sense"
      severity warning;
    assert cutoff_ratio <= 3.0
                           report "Cut-off ratio " & real'image(cutoff_ratio) & " bigger than 3 is a non sense"
                           severity warning;
    
    return 1.0 - exp(- 2.0 * 3.1415926 * cutoff_bandwidth_A00 * real(CLK_cycles_per_sample)/ CLK_freq);
  end function Meta_data_2_prefilter_coeff_real;
  
end package body PreFilter_package;
