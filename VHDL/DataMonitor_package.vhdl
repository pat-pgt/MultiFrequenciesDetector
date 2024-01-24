--! @brief Data filter for simulation purposes
--!
--! Since the calculation is serial and pipe lined,
--! this code is intended to extract the data
--! that match one or more meta-data
--! 

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.reg_type,
  work.MultiFreqDetect_package.reg_sin_cos_z,
  work.MultiFreqDetect_package.meta_data_t,
  work.MultiFreqDetect_package.meta_data_list_t;

package DataMonitor_package is

  component DataMonitorZ is
    generic (
      filter_md : meta_data_t
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK         : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST         : in std_logic;
      full_sync   : in std_logic;
      reg_sync    : in std_logic;
      angle_z     : in reg_type;
      meta_data   : in meta_data_t;
      --! Start reporting.
      --! This is not "thread safe" then the report should fit on one line
      send_report : in std_logic
      );
  end component DataMonitorZ;

  component DataMonitorXYZ is
    generic (
      filter_md            : meta_data_t;
      amplitude_correction : real := 1.0
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK         : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST         : in std_logic;
      full_sync   : in std_logic;
      reg_sync    : in std_logic;
      scz         : in reg_sin_cos_z;
      meta_data   : in meta_data_t;
      --! Start reporting.
      --! This is not "thread safe" then the report should fit on one line
      send_report : in std_logic
      );
  end component DataMonitorXYZ;

  component DataMonitorMultiZ is
    generic (
      meta_data_list : meta_data_list_t
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK         : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST         : in std_logic;
      full_sync   : in std_logic;
      reg_sync    : in std_logic;
      angle_z     : in reg_type;
      meta_data   : in meta_data_t;
      --! Start reporting.
      --! This is not "thread safe" then the report should fit on one line
      send_report : in std_logic
      );
  end component DataMonitorMultiZ;
  component DataMonitorMultiXYZ is
    generic (
      meta_data_list       : meta_data_list_t;
      amplitude_correction : real := 1.0
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK         : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST         : in std_logic;
      full_sync   : in std_logic;
      reg_sync    : in std_logic;
      scz         : in reg_sin_cos_z;
      meta_data   : in meta_data_t;
      --! Start reporting.
      --! This is not "thread safe" then the report should fit on one line
      send_report : in std_logic
      );
  end component DataMonitorMultiXYZ;
end package DataMonitor_package;
