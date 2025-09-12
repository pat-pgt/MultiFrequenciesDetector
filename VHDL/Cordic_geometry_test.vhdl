library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.reg_size,
  work.Input_modules.all;


entity Cordic_geometry_test is
end entity Cordic_geometry_test;

architecture arch of Cordic_geometry_test is
  signal shift_counter : natural := 0;
begin
assert false report "Shifts expected radians   (expec. degrees)     actual radians     (actual degrees)"
  severity note;
  
main_proc : process is
  variable expected_value : real;
  variable actual_value : real;
  begin
    main_count : if shift_counter < reg_size - 3 then
      actual_value := 2.0 * math_pi *
                      real(to_integer(unsigned(arctg_2_angle_reg( shift_counter )))) /
                      real( 2 ** reg_size );
      expected_value := arctan( 1.0 / 2.0 ** shift_counter );
      shift_counter <= shift_counter + 1;
      assert false report integer'image( shift_counter ) &
        ",  " & real'image( expected_value ) & " (" &
        real'image( 180.0 * expected_value / math_pi ) & ")" &
        ",  " & real'image( actual_value ) & " (" &
        real'image( 180.0 * actual_value / math_pi ) & ")"
        severity note;
      wait for 1 nS;
    else
      assert false
        report "Verification of the geometry is over, see the results for details"
        severity note;
      wait;
    end if main_count;
  end process main_proc;

end architecture arch;
