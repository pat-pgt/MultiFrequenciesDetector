library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all,
  work.Cordic_package.Cordic_IntermStage;

--! @brief Test one cordic X, Y and Z stage
--!
--! Test one Cordic elementary calculation,
--!   for Z to 0 or Y to 0.\n
--!
--! ASSUME the division ratio is greater than 2 (shift 1)\n
--! ASSUME X and Y are
--!   * lower or equal than 01011..11 (signed)
--!   * greater or equal than 10100..01 (signed).
--! ASSUME Z is always lower than 0100..00 (unsigned),
--!   or always upper than 1100..00.\n
--! ASSUME the low bits of the shifted operand are lost
--!   using cut not round.
--!   The number of lost bits is equal to the shifts value.\n
--! ASSUME the Y or Z that trigger the add or subtract
--!   should be valid only during the reg_sync.\n
--! ASSERT X, Y and Z are correct.
--! ASSERT there is no cross talk between
--!   the registers before and after.\n
--! ACCURACY exact integer values are asserted.\n
--! LENGTH can be run as a fast test
--!   or a long and more complete test.\n
--!
--! It generates many relevant test cases
--!   and compare against the theory.\n
--! Since the addition or subtraction is serial,
--!   the classics 12 vectors schema can not be used.
--! It assumes there are no interactions between the bits,
--!   inside the vectors.
--! For X, a counter is connected to the high bits.
--! Another counter is connected to the low bits.
--! The last one is a 1 bit counter,
--!   connected to all the remaining mid bits.
--! This is instantiated a second time for Y.
--! This is instantiated a third time for Z
--!   with the high -1 bit always equal to the high bit.\n
--! The crosstalk test and the trigger valid window
--!   (see above) is done while sending 'X' into
--!   the standard logic, when nothing relevant is expected.\n


entity Cordic_Interm_stages_test is
  generic (
    test_vector_length_high : positive := 2;
    --     test_vector_length_mid is always 1
    test_vector_length_low  : positive := 2;
    Z_not_Y_to_0            : boolean;
    shifts_calc_from        : integer range 1 to reg_size - 2
    shifts_calc_from        : integer range 1 to reg_size - 2
    );

end entity Cordic_Interm_stages_test;


--! The strategy is to generate test patterns
--!   and to send them with error patterns between every one.

architecture arch of Cordic_Interm_stages_test is
  signal CLK        : bit      := '0';
  --! Number of runs
  constant run_nbre : positive := reg_size / arithm_size;
  signal run_val    : integer  := 0;
  signal reg_sync   : std_logic;
  signal odd_even   : bit;

  --! Global counter to check all the combinations
  signal global_counter : unsigned(4 downto 0) := (others => '0');
  --! An and/or reduce to check if there are some X error in a run result
  signal check_X_local  : std_logic;
  --! An and/or compilation of the check_X_local of all the run
  --! to report at the end
  signal check_X_global : std_logic            := '0';
begin

  main_proc : process

  begin


  end process main_proc;



end architecture arch;
