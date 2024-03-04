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
