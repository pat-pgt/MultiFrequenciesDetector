--! @brief
--!
--! This package defines all the utilities to link
--!   the circuit frequencies to the frequencies related to the notes.\n
--! It does not contain any user definitions.

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.Meta_data_package.all,
  work.InterModule_formats.all,
  work.Frequencies.all;

package Input_modules is

  --! @brief Populates the array of notes delta Z
  --!
  --! Computes the constant to be added to the angle, at each full cycle,
  --! in order to run the angle generator.\n
  --! Since the octaves are computed by multiple multiplications by 2
  --! the array contains only values for one octave.\n
  --! In the debug mode, the master clock frequency is not considered,
  --! the lowest octave is going to make a full spin ASAP.\n
  --! The NOT debug mode considers the master clock frequency.\n
  --! TODO Consider the lowest octave (and the number of octaves),
  --! rather than octaves from 00 to N_octaves - 1.
  function angle_constants_populate_real(constant debug_mode : boolean) return angle_list_per_note_real;

  --! @brief computes the Z angle for a given arctg as 1/2**N
  --!
  --! For each cordic algorithm stage, the vector (X,Y) is spun
  --! CW or CCW by an angle which its tangent is 1/2**N.\n
  --! This function computes the angle to add or subtract to the angle Z.
  function arctg_2_angle_real(constant shift_bits : natural) return real;

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

  
end package Input_modules;

package body Input_modules is

  function angle_constants_populate_real(constant debug_mode : boolean) return angle_list_per_note_real is
    variable temp : angle_list_per_note_real;
  begin
    if debug_mode = false then
      assert false report "Sorry not yet implemented" severity failure;
      --! The value to add, at each note change, is:
      --! * the frequency of the lowest note of the first possible octave
      --! * multiplied by 2 power the starting octave minus -1.
      --! * multiplied by the number of CLK cycles between notes
      --! * multiplied by 2 power the number of bits of the angle
      --! * divided by the CLK frequency

    else
      for ind_note in temp'range loop
        temp( ind_note ) := 2.0 ** ( real( ind_note )/ 12.0 ) / 4.0;
      end loop;
    end if;
    return temp;
  end function angle_constants_populate_real;

  function arctg_2_angle_real(constant shift_bits : natural) return real is
    variable the_atg_r : real;
  begin
    the_atg_r := arctan(1.0, real(2 ** shift_bits));
    -- normalize the angle to: min = 0, max = 2.pi - epsilon
    the_atg_r := the_atg_r / (2.0 * math_pi);
    -- "convert" into bin
    assert shift_bits < 32 report "Interm stage uses a big shift of " &
      integer'image(shift_bits)
      severity warning;
    return the_atg_r;
  end function arctg_2_angle_real;

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
  
end package body Input_modules;
