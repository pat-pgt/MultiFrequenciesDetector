library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.MultiFreqDetect_package.all;


package Cordic_package is

--! @brief Cordic Z to 0 first stage
--!
--! Perform a pre-processing and format the data
--! in order to run the stages.
--! X is provided as a signed data, using a vector
--! of a length between 4 bits and the register size minus 2.
--! The data is left justified to left and the low bits
--! are computed to get a rail to rail input.\n
--! Y is computed in the same way than X.
--! It is used only for tests while providing a PI/2 shifted
--! input.\n
--! Z is provided as an unsigned vector 0 to 2.PI - epsilon.
--! Z is converted into a signed vector PI to -PI.
--! In fact PI/2 to PI and -PI to -PI/2 are never raised.\n
--! Rules applies in this order
--! to spin the vector while subtracting the angle from Z
--! PI to 2.PI: the high bit of Z is cleared, X=-X and Y=-Y\n 
--! PI/2 to PI: the high - 1 bit of Z is cleared, X=-Y and Y=X
--! For the PI/4 to PI/2 zone,
--! the vector has spun by PI/2, the (remaining) angle
--! is taken from the symmetry against PI/2 and negated
--! Then the Z is transmitted while filling up the 3 high bits
--! using the high - 2 bit of the input Z.
  component Cordic_FirstStage_Z_to_0 is
    generic (
      debug_mode : boolean := false
      );
    port (
      CLK              : in  std_logic;
      RST              : in  std_logic;
      -- Comming from the angle generator,
      -- the flag tells the angle has just changed
      -- For the intermediary stages,
      -- the flag tells the data has stopped in order
      -- to update some signals such as CCW_or_CW
      reg_sync_in      : in  std_logic;
      reg_sync_out : out std_logic;
      -- unsigned min = 0, max = 2.PI - epsilon
      -- come a serial chunks of arithm_size bit
      angle_z          : in  reg_type;
      meta_data_in     : in  meta_data_t;
      meta_data_out    : out meta_data_t;
      --! signed input vector
      --! The size should be at least 4 to keep the trigonometric properties\n
      --! The size should be not more than the reg_size size minus 1,
      --!   in order to divide by 2 the input
      --!   as Cordic slightly increases the gain (see the Python simulation)
      input_x, input_y :     std_logic_vector;
      --! Output of X, Y and Z.
      scz              : out reg_sin_cos_z
      );
  end component Cordic_FirstStage_Z_to_0;
--! @brief Cordic intermediary stages
--!
--! This computes one cordic vector spin with its angle (Z) update\n
--! It is common for both\n
--! * To multiply a value by an angle vector, the angle should converged to 0\n
--! * To convert rectangular coordinates to polar, The Y should convergent to 0.
--! For this case, it is required the angle is already in the -PI/2 +PI/2 zone
  component Cordic_IntermStage is
    generic (
      debug_mode   : boolean := false;
      Z_not_Y_to_0 : boolean;
      shifts_calc  : integer range 1 to reg_size - 2
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
  end component Cordic_IntermStage;

--! @brief Cordic Z to 0 last stage
--!
--! Performs some sanity checks on Z.
--! No process is needed on X and Y as
--! the prefilter uses the serial data.\n
--! The checks on Z are for the tests.
--! They can, howaver, be synthetised
--! as some tests can be done using
--! FPGA (with specific input and
--! output code).
  component Cordic_LastStage_Z_to_0 is
    port (
      CLK              : in  std_logic;
      RST              : in  std_logic;
      reg_sync         : in  std_logic;
      scz_in           : in  reg_sin_cos_z;
      Z_error_exponent : out std_logic_vector(5 downto 0)
      );
  end component Cordic_LastStage_Z_to_0;

  component Cordic_Bundle_Z_to_0 is
    generic (
      debug_mode  : boolean               := false;
      stages_nbre : integer range 1 to 25 := 5
      );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      X_out         : out std_logic_vector(arithm_size - 1 downto 0);
      Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
      Z_expon_out   : out std_logic_vector(5 downto 0));
  end component Cordic_Bundle_Z_to_0;


end package Cordic_package;
