--! @brief Meta data definitions and utilities
--!
library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all;

package Meta_data_package is
  --! @brief Number of notes per octave
  --!
  --! If 1, the A is used. If 2, the A and the D# are used.
  --! If 3, the A, C#, F are used. If 4, the A, C, D# and F# are used
  --! If 5, 7, 8, 9, 10 or 11, a strange result is expected but it does not crach
  --! If 12 all the notes are used\n
  --! This is nice for testing or checking an FPGA size
  constant N_notes          : integer range 1 to 12   := 12;
  --! @brief Number of octaves.
  --!
  --! It should be at least two, due to restrictions in the angle generator
  --!   as the angle "spining" is done using vectors of double size
  constant N_octaves        : integer range 2 to 8    := 7;
  --! @brief starting octave.
  --!
  --! Lowest A note to handle.
  --! The 440Hz is the A4, the 220Hz is the A3 etc... The A00 is quoted as -1.
  constant starting_octave  : integer range -1 to 4   := 4;
  -- Is going to be removed
  constant global_freq      : real                    := 4.0e3;
  --! @brief Master clock
  --!
  --! Used by the angle generator and by the filters
  constant CLK_freq         : real                    := 25.0e6;
  --! @brief Met data of the frequencies
  --!
  --! This data follows each value.\n
  --! Since the computations run is sequence, it is not that relevant.
  --! However, it is useful for debug.
  --! It can be used to verify values are in sync in an improved version.
  type meta_data_t is record
    --! First one 0x0 => octave -1, last one 0x7 => octave 6.
    octave : std_logic_vector(2 downto 0);
    --! First one 0 => C/do, last one 0xb => B/si. \n
    --! Catch teste with all notes and all octaves 0xff
    note   : std_logic_vector(3 downto 0);
  end record meta_data_t;
  --! Set from integers
  function octave_note_to_meta_data(constant octave : integer range 0 to N_octaves - 1;
                                    constant note   : integer range 0 to N_notes - 1) return meta_data_t;
  function octave_all_notes_to_meta_data(constant octave : integer range 0 to N_octaves - 1) return meta_data_t;
  --! Override the comparison operator
  function "=" (lmd, rmd                                 : meta_data_t) return boolean;
  --! Convert meta-data into note name or value
  --!
  --! If the number of notes, N, per octave is 1, 2, 3, 6 or 12,
  --! the return string is, for instance, C#4 
  --! otherwise, the text is base octave multiplied by sqrt(N)powerM
  function meta_data_image(md                            : meta_data_t) return string;
  --! List of meta datas
  --! This is mostly used for testing as a list of octave notes couple
  type meta_data_list_t is array(integer range<>) of meta_data_t;
  --! List of stage to monitor
  --! This is mostly used for testing as a list of N and N+1 couples stages

  type angle_list_per_note_real is array(0 to N_notes - 1) of real;


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

  function arctg_2_angle_real(constant shift_bits : natural) return real;


end package Meta_data_package;



package body Meta_data_package is
  function octave_note_to_meta_data(constant octave : integer range 0 to N_octaves - 1;
                                    constant note   : integer range 0 to N_notes - 1) return meta_data_t is
    variable the_return : meta_data_t;
  begin
    the_return.octave := std_logic_vector(to_unsigned(octave, the_return.octave'length));
    the_return.note   := std_logic_vector(to_unsigned(note, the_return.note'length));
    return the_return;
  end function octave_note_to_meta_data;
  
  function octave_all_notes_to_meta_data(constant octave : integer range 0 to N_octaves - 1) return meta_data_t is
    variable the_return : meta_data_t;
  begin
    the_return.octave := std_logic_vector(to_unsigned(octave, the_return.octave'length));
    the_return.note   := (others => '1');
    return the_return;
  end function octave_all_notes_to_meta_data;

  function "=" (lmd, rmd : meta_data_t) return boolean is
  begin
    if lmd.note = std_logic_vector(to_signed(-1, lmd.note'length)) or
      rmd.note = std_logic_vector(to_signed(-1, rmd.note'length)) then
      return true;
    else
      return (lmd.octave = rmd.octave) and (lmd.note = rmd.note);
    end if;
  end function "=";

  function meta_data_image(md : meta_data_t) return string is
    type octaves_list_t is array(0 to 7) of string(1 to 2);
    constant octaves_list : octaves_list_t := (0 => "00", 1=>"0 ", 2=>"1 ", 3=>"2 ", 4=>"3 ", 5=>"4 ", 6=>"5 ", 7=>"6 ");
    type notes_list_t is array (0 to 11) of string(1 to 2);
    constant notes_list : notes_list_t := (0  => "A ", 1=>"A#", 2=>"B ", 3=>"C ", 4=>"C#", 5=>"D ",
                                            6 =>"D#", 7=>"E ", 8=>"F ", 9=>"F#", 10=>"G ", 11=>"G#");
  begin
    if md.note = std_logic_vector( to_signed( -1, md.note'length )) then
      return "all ";
    elsif N_notes = 1 or N_notes = 2 or N_notes = 4 or N_notes = 6 or N_notes = 12 then
      return notes_list((to_integer(unsigned(md.note)) * N_notes) / 12) & octaves_list(to_integer(unsigned(md.octave)));
    else
      report "Sorry not yet implemented" severity failure;
    end if;
  end function meta_data_image;

  function angle_constants_populate_real(constant debug_mode : boolean) return angle_list_per_note_real is
    variable temp : angle_list_per_note_real;
  begin
    if debug_mode = false then
      assert false report "Sorry not yet implemented" severity failure;
      --! The value to add, at each note change, is:
      --! * the frequency of the lowest note of the first possible octave
      --! * multiplied by 2 power the starting octave minus -1.
      --! * multiplied by the number of CLK cycles between notes
      --! * multiplied by 2 power the number iof bits of the angle
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
    -- normalise the angle to: min = 0, max = 2.pi - epsilon
    the_atg_r := the_atg_r / (2.0 * math_pi);
    -- "convert" into bin
    assert shift_bits < 32 report "Interm stage uses a big shift of " &
      integer'image(shift_bits)
      severity warning;
    return the_atg_r;
  end function arctg_2_angle_real;

end package body Meta_data_package;
