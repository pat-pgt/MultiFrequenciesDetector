--! @brief Data filter for simulation purposes
--!
--! Since the calculation is serial and pipe lined,
--! this code is intended to extract the data
--! that match one or more meta-data
--! 

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.starting_octave,
  work.MultiFreqDetect_package.reg_type,
  work.MultiFreqDetect_package.meta_data_t;

entity DataMonitorZ is
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
    send_report : in std_logic
    );
end entity DataMonitorZ;

architecture rtl of DataMonitorZ is
  signal curr_angle_z                       : reg_type;
  signal angle_z_diff                       : reg_type;
  signal angle_z_diff_min, angle_z_diff_max : reg_type;
  signal start_count                        : std_logic_vector(1 downto 0);
begin
  main_proc : process (CLK) is
    variable padding_1bit    : signed(0 downto 0);
    variable diff_temp_plus1 : signed(reg_type'length downto 0);
    variable diff_temp       : signed(reg_type'range);
  begin
    CLK_IF : if rising_edge(CLK) then
      if reg_sync = '1' and
        meta_data.note = filter_md.note and meta_data.octave = filter_md.octave then
        curr_angle_z    <= angle_z;
        padding_1bit(0) := '0';
        diff_temp_plus1 := (padding_1bit & signed(angle_z)) - (padding_1bit & signed(curr_angle_z));
        diff_temp       := diff_temp_plus1(diff_temp_plus1'high - 1 downto diff_temp_plus1'low);
        angle_z_diff    <= std_logic_vector(diff_temp);
      end if;
      if RST = '0' then
        if start_count = std_logic_vector(to_signed(-1, start_count'length)) then
          if unsigned(diff_temp) > unsigned(angle_z_diff_max) then
            angle_z_diff_max <= std_logic_vector(diff_temp);
          end if;
          if unsigned(diff_temp) < unsigned(angle_z_diff_min) then
            angle_z_diff_min <= std_logic_vector(diff_temp);
          end if;
        end if;
        if start_count /= std_logic_vector(to_signed(-1, start_count'length)) and full_sync = '1' then
          
          
          start_count <= std_logic_vector(unsigned(start_count) + 1);
        end if;
      else
        angle_z_diff_min <= (others => '1');
        angle_z_diff_max <= (others => '0');
        start_count      <= (others => '0');
      end if;
    end if CLK_IF;
  end process main_proc;
  report_proc : process(send_report)
    variable note_txt   : string(1 to 2);
    variable octave_txt : string(1 to 2);
  begin
    if rising_edge(send_report) then
      if (to_integer(unsigned(filter_md.octave)) + starting_octave) = -1 then
        octave_txt := "00";
      else
        octave_txt := integer'image(to_integer(unsigned(filter_md.octave)) + starting_octave) & " ";
      end if;
      -- TODO check the 2 high bits which should be both 1 or both 0 
      report "O/N " & octave_txt & "/" &
        integer'image(to_integer(unsigned(filter_md.note))) & ": min " &
        integer'image(to_integer(unsigned(angle_z_diff_min))) & ": max " &
        integer'image(to_integer(unsigned(angle_z_diff_max)))
        severity note;
    end if;
  end process report_proc;
end architecture rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.MultiFreqDetect_package.reg_type,
  work.MultiFreqDetect_package.reg_sin_cos_z,
  work.MultiFreqDetect_package.meta_data_t;

entity DataMonitorXYZ is
  generic (
    filter_md            : meta_data_t;
    amplitude_correction : real := 1.0
    );
  port (
    --! Master clock. It is active on rising edges.
    CLK       : in std_logic;
    --! Reset. It is active when '1', it is in normal run if '0'.
    RST       : in std_logic;
    full_sync : in std_logic;
    reg_sync  : in std_logic;
    scz       : in reg_sin_cos_z;
    meta_data : in meta_data_t
    );
end entity DataMonitorXYZ;

architecture rtl of DataMonitorXYZ is
  signal hold_real_x, hold_real_y : real;
  signal hold_amplitude           : real;
  signal hold_angle               : real;
begin
  main_proc : process (CLK) is
    variable hold_real_x_v, hold_real_y_v : real;
  begin
    CLK_IF : if rising_edge(CLK) then
      if reg_sync = '1' and
        meta_data.note = filter_md.note and meta_data.octave = filter_md.octave then
        hold_real_x_v  := real(to_integer(signed(scz.the_cos)));
        hold_real_y_v  := real(to_integer(signed(scz.the_sin)));
        hold_real_x    <= hold_real_x_v;
        hold_real_y    <= hold_real_y_v;
        hold_amplitude <= sqrt(hold_real_x_v **2 + hold_real_y_v**2) * amplitude_correction;
        if hold_real_y_v /= 0.0 then
          hold_angle <= arctan(hold_real_x_v / hold_real_y_v);
        else
          hold_angle <= 90.0;
        end if;
      -- TODO handle Z
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
  work.DataMonitor_package.DataMonitorZ;

entity DataMonitorMultiZ is
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
    send_report : in std_logic
    );
end entity DataMonitorMultiZ;

architecture rtl of DataMonitorMultiZ is
begin

  filters_set : for ind in meta_data_list'range generate
    filter_elem : DataMonitorZ generic map (
      filter_md => meta_data_list(ind)
      )
      port map (
        CLK         => CLK,
        RST         => RST,
        full_sync   => full_sync,
        reg_sync    => reg_sync,
        angle_z     => angle_z,
        meta_data   => meta_data,
        send_report => send_report
        );
  end generate filters_set;
  
end architecture rtl;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.reg_type,
  work.MultiFreqDetect_package.meta_data_t,
  work.MultiFreqDetect_package.meta_data_list_t,
  work.MultiFreqDetect_package.reg_sin_cos_z,
  work.DataMonitor_package.DataMonitorXYZ;

entity DataMonitorMultiXYZ is
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
    send_report : in std_logic
    );
end entity DataMonitorMultiXYZ;

architecture rtl of DataMonitorMultiXYZ is
begin

  filters_set : for ind in meta_data_list'range generate
    filter_elem : DataMonitorXYZ generic map (
      filter_md            => meta_data_list(ind),
      amplitude_correction => amplitude_correction
      )
      port map (
        CLK         => CLK,
        RST         => RST,
        full_sync   => full_sync,
        reg_sync    => reg_sync,
        scz         => scz,
        meta_data   => meta_data,
        send_report => send_report
        );
  end generate filters_set;
  
end architecture rtl;
