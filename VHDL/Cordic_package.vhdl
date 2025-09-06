library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
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
--! Z is provided as an unsigned vector 0 to 2.PI - epsilon.\n
--! The output is data in the format of the stages links.
--! The first vector spin to be executed
--! is the arc-tan 0.5 that means 26.56 degrees.\n
--! The latency of one stage has to be considered.
  component Cordic_FirstStage_Z_to_0 is
    port (
      CLK              : in  std_logic;
      RST              : in  std_logic;
      -- Coming from the angle generator,
      -- the flag tells the angle has just changed
      -- For the intermediary stages,
      -- the flag tells the data has stopped in order
      -- to update some signals such as CCW_or_CW
      reg_sync_in      : in  std_logic;
      reg_sync_out     : out std_logic;
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
      scz_out          : out reg_sin_cos_z
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

--! @brief Cordic last stage for test
--!
--! This converts the serial format of X, Y and Z
--!   to parrallel and check.\n
--! This gets the exponent of the value that
--!   should converge to 0.
  component Cordic_LastStage_4_test is
    generic (
      Z_not_Y_2_0    : boolean
      );
    port (
      CLK            : in  std_logic;
      RST            : in  std_logic;
      reg_sync       : in  std_logic;
      scz_in         : in  reg_sin_cos_z;
      X_out          : out reg_type;
      Y_out          : out reg_type;
      Z_out          : out reg_type;
      error_exponent : out std_logic_vector(5 downto 0)
      );
  end component Cordic_LastStage_4_test;

--! @brief Cordic last stage for processing
--!
--! This converts into parallel with mantissa and exponent
--!   
  component Cordic_LastStage_4_processing is
    generic (
      Z_not_Y_2_0    : boolean
      );
    port (
      CLK        : in  std_logic;
      RST        : in  std_logic;
      reg_sync   : in  std_logic;
      scz_in     : in  reg_sin_cos_z;
      X_out      : out reg_type;
      X_exponent : out std_logic_vector(5 downto 0);
      Z_out      : out reg_type;
      Z_exponent : out std_logic_vector(5 downto 0)
      );
  end component Cordic_LastStage_4_processing;

  component Cordic_Bundle_Z_to_0 is
    generic (
      stages_nbre         : integer range 1 to 25 := 20;
      metadata_catch_list : meta_data_list_t;
      stages_catch_list   : cordic_stages_num_list
      );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      full_sync     : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z;
      X_out         : out reg_type;
      Y_out         : out reg_type;
      Z_out         : out reg_type;
      Z_expon_out   : out std_logic_vector(5 downto 0);
      report_in     : in  std_logic;
      report_out    : out std_logic);
  end component Cordic_Bundle_Z_to_0;

--! @brief Cordic Y to 0 first stage
--!
--! Perform a pre-processing and format the data
--! in order to run the stages.
--! X, Y and Z are provided in the stages links format.\n
--! The latency of two stages has to be considered.
  component Cordic_FirstStage_Y_to_0 is
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      -- For the first and the intermediary stages,
      -- the flag tells the data has stopped in order
      -- to update some signals such as CCW_or_CW
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      --! Input of X, Y and Z.
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z
      );
  end component Cordic_FirstStage_Y_to_0;

  component Cordic_Bundle_Y_to_0 is
    generic (
      stages_nbre         : integer range 1 to 25 := 20;
      metadata_catch_list : meta_data_list_t;
      stages_catch_list   : cordic_stages_num_list
      );
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      full_sync     : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      X_out         : out reg_type;
      Y_out         : out reg_type;
      Z_out         : out reg_type;
      Y_expon_out   : out std_logic_vector(5 downto 0);
      report_in     : in  std_logic;
      report_out    : out std_logic);
  end component Cordic_Bundle_Y_to_0;

  --! @brief Monitor one cordic stage
  --!
  --! This component is called for the stage num_stage
  --! According with a list of meta-data.\n
  --! This component catches the data before and after.\n
  --! The determinant is computed with X and Y, its absolute value is kept.
  --! The difference of Z is computed, its absolute value is kept.\n
  --! One can check the details with a wave viewer to investigate bugs.
  --! The component produces a report of the minimum and maximum deviation at the end.\n
  --! During the synthesis, the component is not called,
  --! then its related entity should be missing.
  component Cordic_Interm_monitor is
    generic (
      Z_not_Y_to_0 : boolean;
      stage_num : positive;
      metadata_catch_list : meta_data_list_t
      );
    port (
      CLK             : in std_logic;
      RST             : in std_logic;
      reg_sync        : in std_logic;
      full_sync       : in  std_logic;
      report_in       : in std_logic;
      report_out      : out std_logic;
      meta_data_after : in meta_data_t;
      scz_before      : in reg_sin_cos_z;
      scz_after       : in reg_sin_cos_z);
  end component Cordic_Interm_monitor;

end package Cordic_package;

