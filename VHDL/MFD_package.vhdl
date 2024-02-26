--! @brief General definitions of the frequencies detection
--!
--! This file is regulary updated
--! It come from a demonstrator that was used to qulify from end to end
--! One can know which part is up to date while checking the associte vhdl file
--! With new versions of VHDL, the constants can go into package generic

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.Meta_data_package.all;

package MultiFreqDetect_package is
  --! Number of bits that are added or subtracted at a time
  --! Please note, not all values are supported by components
  --! such the filtering
  constant arithm_size      : integer range 1 to 24   := 1;
  --! @brief Number of bits of the vectors in the cordic algo
  --!
  --! The angle internal calculation uses twice\n
  --! reg_size should be a multiple of arithm_size
  --! and not equal to it
  constant reg_size         : integer range 16 to 255 := 32;
  --! @brief Exponent size
  --!
  --! The output is sent as mantissa and exponent,
  --! in order to speed up amplitude comparaisons
  constant exponent_size    : integer range 1 to 8    := 5;
  --! @brief Mantissa size
  --!
  --! The output is sent as mantissa and exponent,
  --! in order to speed up amplitude comparaisons
  constant detect_mant_size : integer range 3 to 16   := 12;

  --! @brief Type of the cordic X, Y and Z
  --!
  --! For the Cordic algorithm, X and Y should be the same type.
  --! It is relevant to also keep Z in the same scale as X and Y.\n
  --! Since we are using serial calculation, all the 3 have the same length.
  subtype reg_type is std_logic_vector(reg_size - 1 downto 0);
  type cordic_stages_num_list is array(integer range<>) of positive;
  --! Data link between the stages
  --!
  --! All the values are passed in serial mode bit by bits
  --! or small groups by small groups
  --! Metadata and quadrant are passed in parallel mode.
  type reg_sin_cos_z is record
    --! LSB comes first.
    --! For this version, the size is 1 as arithm_size is 1
    the_sin : reg_type;
    --! LSB comes first.
    --! For this version, the size is 1 as arithm_size is 1
    the_cos : reg_type;
    --! LSB comes first.
    --! For this version, the size is 1 as arithm_size is 1
    angle_z : reg_type;
  end record reg_sin_cos_z;
  --! Data link between the stages
  --!
  --! All the values are passed in serial mode.
  --! Metadata and quadrant are passed in parallel mode.
  type reg_sin_cos_toggle is record
    --! to be removed
    RUN       : std_logic;
    --! to be removed
    reg_sync  : std_logic;
    --! LSB comes first.
    the_sin   : std_logic;
    --! LSB comes first.
    the_cos   : std_logic;
    --! LSB comes first.
    --! 3 high bits are always 0 (or 1 if Z is negative),
    --!   or Z might be equal to 0010..0
    angle_z   : std_logic;
    meta_data : meta_data_t;
    --! In which PI/4 zone is Z.\n
    --! In the Cordic before the filtering,
    --!   only X and Y are used, Z should converge to 0.
    --! If Z is between PI/4 and PI/2 modulo PI/2, X and Y are toggled.\n
    --! In the Cordic after the filtering,
    --!   only X and Z are used, Y should converge to 0.
    --! If X and Y has been toggled in the first stage,
    --!   the symmetry against PI/4 is computed.
    --! If X, Y or both have been converted to positive,
    --!   Z is updated accordingly.
    quadrant  : std_logic_vector(2 downto 0);
  end record reg_sin_cos_toggle;


  --! @brief Common ports
  --!
  --! This is a dummy component, never instantiated.
  --! It is designed to document the signal
  --!   used by most of the components.
  --! It is also designed to run some asserts.
  component MultiCordic_Dummy_Doc_and_Asserts is
    generic (
      --! Unless otherwise, it is in debug mode is true.
      debug_mode : boolean
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST : in std_logic;
      --! @brief Run this sequence
      --!
      --! The interleaves make some components idle at some frames.
      --! The component runs if '1'. 
      RUN : in std_logic
      );
  end component MultiCordic_Dummy_Doc_and_Asserts;
  --! @brief Computes angles
  --!
  --! Computes the angle of the next value.\n
  --! It adds the current angle to the constant
  --!  according to the frequency.\n
  --! This module is the general sequencer as well.\n
  --! * provides the sync bit for the serial calculation
  --! * provides octaves and notes
  --! \return The angle, the metadata and the sync bit
  component AngleGene is
    generic (
      debug_mode : boolean := true
      );
    port (
      CLK       : in  std_logic;
      RST       : in  std_logic;
      --! A full cycle of all the requested notes of all the requested octaves
      --! completed
      full_sync : out std_logic;
      --! Starts a new computation of a given note and octave
      --! Note, it is not in phase with the meta data, but the shift is constant
      reg_sync  : out std_logic;
      --! Angle out, updated with reg_sync
      --! Only the half high bit are available.
      --! The others are for the computation precision.
      angle_z   : out reg_type;
      --! angle out, updated with reg_sync
      meta_data : out meta_data_t
      );
  end component AngleGene;
  --! @brief Cordic Z to 0 input stage 
  --!
  --! Runs the 3 first Cordic iterations.
  --! That reduces the latency and reduces the cumulative 1/cos(h) products.
  --! The result is only amplified by 16% rather than 60%
  --! by placing directly the vector in the relevant PI/4 quadrant.
  --! Z is updated accordingly.\n
  --! For the normal mode, the input value is entered to the X input,
  --!   Y is set to 0 and Z is set to the angle.
  --! Y can be non-null for test purposes.\n
  component Cordic_FirstStage_2_to_0 is
    generic (
      debug_mode : boolean := false
      );
    port (
      CLK              : in  std_logic;
      RST              : in  std_logic;
      RUN              : in  std_logic;
      reg_sync         : in  std_logic;
      -- unsigned min = 0, max = 2.PI - epsilon
      angle_z          : in  reg_type;
      meta_data        : in  meta_data_t;
      -- signed 2's complement
      input_c, input_s : in  std_logic_vector;
      scz              : out reg_sin_cos_z
      );
  end component Cordic_FirstStage_2_to_0;
  --! @brief Cordic input stage
  --!
  --! Runs the 3 first Cordic iterations of the detection bloc, after the filters.
  --! The X and Y values would have been placed accordingly to X and Y,
  --!   and the angle would have been set to 0.\n
  --! The X and Y are placed into the relevant PI/2 steps quadrant.
  --! Then X, Y or both are negated.
  --! A PI/2 multiple is set to the angle.
  --! The 2 high bits of the quadrant signal are set.\n
  --! In this version, there is no comparison to toggle X and Y
  --!   if the angle is greater then PI/4.\n
  --! This is mostly to avoid X and Y to grow to much,
  --!  as only low arc-tg are computed.
  component MultiCordic_Detect_FS is
    generic (
      debug_mode : boolean := false
      );
    port (
      CLK         : in  std_logic;
      RST         : in  std_logic;
      input_scsa  : in  reg_sin_cos_toggle;
      output_scsa : out reg_sin_cos_toggle
      );
  end component MultiCordic_Detect_FS;
  --! @brief Cordic intermediary stage
  --!
  --! Runs one iteration of the Cordic alghorithm.
  --! It is used for all the frequencies.\n
  --! It can run in the P->R or R->P mode.
  --! This component should be instantiated as many times as iterations.
  --! The output of the previous one is connected to the input of the next one.\n
  --! Its register latency is 1 full register to receive and 1 to transmit
  --! See details in the implementation
  component MultiCordic_IntermStage is
    generic (
      posit_interm         : natural;
      --! @brief Cordic mode
      --!
      --! True: angle converges to 0 as the vector is turned accordingly
      --! False: vector converges to (x,0) as the angle is updated accordingly 
      vector_from_angle    : boolean;
      --! Debug mode: angle is the real angle or a simple value taken from the position
      debug_mode_angle     : boolean := false;
      --! Debug mode: send the input to the output as it
      debug_mode_donothing : boolean := false
      );
    port (
      CLK         : in  std_logic;
      RST         : in  std_logic;
      input_scsa  : in  reg_sin_cos_toggle;
      output_scsa : out reg_sin_cos_toggle
      );
  end component MultiCordic_IntermStage;
  --! @brief Cordic last stage
  --!
  --! It is not, strictly speaking a Cordic stage.
  --! It is used for tests while converting serial to parallel data.
  --! The data is left as fixed values.
  --! For test purposes, it can be used everywhere in the chain.\n
  --! While using the filter, only one frequency is selected.
  --! That make the result friendly viewable.
  component MultiCordic_LastStage is
    generic (
      --! True: selects one frequency.
      --! False: all the frequencies passes through.
      use_metadata_filter : boolean := true;
      --! If use_metadata_filter is true, which frequency.
      metadata_filter     : meta_data_t);
    port (
      CLK        : in  std_logic;
      RST        : in  std_logic;
      input_scsa : in  reg_sin_cos_toggle;
      output_sin : out reg_type;
      output_cos : out reg_type
      );
  end component MultiCordic_LastStage;

  --! @brief Cordic last detection stage
  --!
  --! It converts serial data into parallel data.\n
  --! It can be used for test purposes with a conversion into float values.
  --! It can be used as the final stage to process easily the amplitude
  --!   and the phase.\n
  --! It detects the exponent. The exponent is common
  --!   for the sine and the cosine. It is based on the greater value.\n
  --! While using the filter, only one frequency is selected.
  --! That make the result friendly viewable.
  --! Its latency is 1 full register to receive, the output is parallel
  component MultiCordic_Detect_LS is
    generic (
      --! True: selects one frequency.
      --! False: all the frequencies passes through.
      use_metadata_filter : boolean := true;
      --! If use_metadata_filter is true, which frequency.
      metadata_filter     : meta_data_t);
    port (
      CLK             : in  std_logic;
      RST             : in  std_logic;
      input_scsa      : in  reg_sin_cos_toggle;
      --! Used for debug as it always should converge to 0.
      output_sin_s    : out std_logic;
      output_sin_m    : out std_logic_vector(detect_mant_size - 1 downto 0);
      --! Amplitude of the signal
      output_cos_s    : out std_logic;
      output_cos_m    : out std_logic_vector(detect_mant_size - 1 downto 0);
      output_exponent : out std_logic_vector(exponent_size - 1 downto 0);
      --! Phase of the signal
      output_angle    : out reg_type;
      --! Meta-data octave
      output_octave   : out std_logic_vector(2 downto 0);
      --! Meta-data note
      output_note     : out std_logic_vector(3 downto 0)
      );
  end component MultiCordic_Detect_LS;


  component MultiCordic_Prefilter is
    port (
      CLK         : in  std_logic;
      RST         : in  std_logic;
      input_scsa  : in  reg_sin_cos_toggle;
      output_scsa : out reg_sin_cos_toggle
      );
  end component MultiCordic_Prefilter;



  component Multicordic_bundle is
    generic(
      interm_size : positive := 5
      );
    port (
      CLK                            : in  std_logic;
      RST                            : in  std_logic;
      input                          : in  reg_type;
      output_sin_ser, output_cos_ser : out std_logic;
      output_gen_sync                : out std_logic;
      output_sin_s                   : out std_logic;
      output_sin_m                   : out std_logic_vector(detect_mant_size - 1 downto 0);
      output_cos_s                   : out std_logic;
      output_cos_m                   : out std_logic_vector(detect_mant_size - 1 downto 0);
      output_exponent                : out std_logic_vector(exponent_size - 1 downto 0);
      output_angle                   : out reg_type;
      output_octave                  : out std_logic_vector(2 downto 0);
      output_note                    : out std_logic_vector(3 downto 0));
  end component Multicordic_bundle;

  type angle_list_per_note_t is array(0 to N_notes - 1) of std_logic_vector(2*reg_size - 1 downto 0);


  function angle_constants_populate_reg(constant debug_mode : boolean) return angle_list_per_note_t;

  function arctg_2_angle_reg(constant shift_bits : natural; constant debug_mode : boolean) return reg_type;

end package MultiFreqDetect_package;

package body MultiFreqDetect_package is

  function angle_constants_populate_reg(constant debug_mode : boolean) return angle_list_per_note_t is
    variable temp_r     : angle_list_per_note_real;
    variable temp_i     : integer;
    variable the_return : angle_list_per_note_t;
  begin
    temp_r := angle_constants_populate_real(debug_mode);
    for ind in the_return'range loop
      temp_i := integer(floor(temp_r(ind) * real(2 ** reg_size )));
      the_return(ind)(the_return(ind)'high downto the_return(ind)'high - 3 + 1 ) :=
        ( others => '0' );
      the_return(ind)(the_return(ind)'high - 2 downto the_return(ind)'high - 2 - reg_size + 1 ) :=
       std_logic_vector(to_unsigned(temp_i, reg_size));
      the_return(ind)(the_return(ind)'high - 2 - reg_size downto the_return(ind)'low ) := ( others => '0' );
    end loop;
    return the_return;
  end function angle_constants_populate_reg;

  function arctg_2_angle_reg(constant shift_bits : natural; constant debug_mode : boolean) return reg_type is
    variable temp_r     : real;
    variable temp_i     : integer;
    variable the_return : reg_type;
  begin
    temp_r     := arctg_2_angle_real(shift_bits);
    temp_i     := integer(round(temp_r * real(2 ** reg_type'length)));
    the_return := std_logic_vector(to_unsigned(temp_i, reg_type'length));

    assert not debug_mode report "Angle " & real'image(temp_r) & " =int=> " &
      natural'image(temp_i) & " pour " & integer'image(shift_bits)
      severity note;

    return the_return;
  end function arctg_2_angle_reg;


  
end package body MultiFreqDetect_package;
