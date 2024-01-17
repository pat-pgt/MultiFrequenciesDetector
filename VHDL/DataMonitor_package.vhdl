--! @brief Data filter for simulation purposes
--!
--! Since the calculation is serial and pipe lined,
--! this code is intended to extract the data
--! that match one or more metadata
--! 

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.reg_type,
  work.MultiFreqDetect_package.meta_data_t,
  work.MultiFreqDetect_package.meta_data_list_t;

package DataMonitor_package is

  component DataMonitor is
    generic (
      filter_md : meta_data_t
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK       : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST       : in std_logic;
      reg_sync  : in std_logic;
      angle_z   : in reg_type;
      meta_data : in meta_data_t
      );
  end component DataMonitor;

  component DataMonitorMulti is
    generic (
      meta_data_list : meta_data_list_t
      );
    port (
      --! Master clock. It is active on rising edges.
      CLK       : in std_logic;
      --! Reset. It is active when '1', it is in normal run if '0'.
      RST       : in std_logic;
      reg_sync  : in std_logic;
      angle_z   : in reg_type;
      meta_data : in meta_data_t
      );
  end component DataMonitorMulti;
end package DataMonitor_package;
