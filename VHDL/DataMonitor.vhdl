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
  work.MultiFreqDetect_package.meta_data_t;

entity DataMonitor is
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
end entity DataMonitor;

architecture rtl of DataMonitor is
  signal curr_angle_z : reg_type;
signal angle_z_diff : reg_type;
begin
  main_proc : process (CLK) is
    variable padding_1bit : signed( 0 downto 0 );
    variable diff_temp : signed( reg_type'length downto 0 );
  begin
    CLK_IF : if rising_edge(CLK) then
      if reg_sync = '1' and
        meta_data.note = filter_md.note and meta_data.octave = filter_md.octave then
        curr_angle_z <= angle_z;
        padding_1bit(0) := '0';
        diff_temp := ( padding_1bit & signed( angle_z )) - ( padding_1bit & signed( curr_angle_z ));
        angle_z_diff <= std_logic_vector( diff_temp( diff_temp'high - 1 downto diff_temp'low ));
      end if;
    end if CLK_IF;
  end process main_proc;
end architecture rtl;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.reg_type,
  work.MultiFreqDetect_package.meta_data_t,
  work.MultiFreqDetect_package.meta_data_list_t,
  work.DataMonitor_package.DataMonitor;

entity DataMonitorMulti is
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
end entity DataMonitorMulti;

architecture rtl of DataMonitorMulti is
begin

  filters_set : for ind in meta_data_list'range generate
    filter_elem : DataMonitor generic map (
      filter_md => meta_data_list(ind)
      )
      port map (
        CLK       => CLK,
        RST       => RST,
        reg_sync  => reg_sync,
        angle_z   => angle_z,
        meta_data => meta_data
        );
  end generate filters_set;
  
end architecture rtl;
