library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.all,
  work.DataMonitor_package.DataMonitorMultiZ;


entity AngleGene_test is
end entity AngleGene_test;

architecture rtl of AngleGene_test is
  signal CLK          : std_logic                     := '0';
  signal main_counter : std_logic_vector(4 downto 0) := (others => '0');
  signal RST          : std_logic_vector(5 downto 0)  := (others => '1');

  signal reg_sync       : std_logic;
  signal angle_z        : reg_type;
  signal full_sync : std_logic;
  signal meta_data      : meta_data_t;
  signal send_report : std_logic := '0';
  -- For the whose uses the WAV format rather the GHD format
  signal display_octave : std_logic_vector(meta_data.octave'range);
  signal display_note   : std_logic_vector(meta_data.note'range);
  function octave_note_to_meta_data(constant octave : integer range 0 to 7;
                                    constant note   : integer range 0 to 11) return meta_data_t is
    variable the_return : meta_data_t;
  begin
    the_return.octave := std_logic_vector(to_unsigned(octave, the_return.octave'length));
    the_return.note   := std_logic_vector(to_unsigned(note, the_return.note'length));
    return the_return;
  end function octave_note_to_meta_data;
  constant monitor_list : meta_data_list_t :=
    (
      0 => octave_note_to_meta_data(octave => 4, note => 0),
      1 => octave_note_to_meta_data(octave => 2, note => 0),
      2 => octave_note_to_meta_data(octave => 0, note => 0),
      3 => octave_note_to_meta_data(octave => 0, note => 1),
      4 => octave_note_to_meta_data(octave => 0, note => 11)
      );
begin
  display_note   <= meta_data.note;
  display_octave <= meta_data.octave;

  main_proc : process is

  begin
    IF_COUNT : if main_counter /= std_logic_vector(to_signed(-2, main_counter'length)) and
                 main_counter /= std_logic_vector(to_signed(-1, main_counter'length)) then
      IF_CLK : if CLK = '1' then
        if full_sync = '1' then
          main_counter                     <= std_logic_vector(unsigned(main_counter) + 1);
          report "Progress (" &
            integer'image(to_integer(unsigned(main_counter))) &
            "/" &
            integer'image(2 ** main_counter'length - 3) &
            ")"
            severity note;
        end if;
        RST(RST'high - 1 downto RST'low) <= RST(RST'high downto RST'low + 1);
        RST(RST'high)                    <= '0';
      end if IF_CLK;
      CLK <= not CLK;
      wait for 10 ns;
    elsif main_counter = std_logic_vector(to_signed(-2, main_counter'length)) then
      main_counter <= std_logic_vector(to_signed(-1, main_counter'length));
      report "SImulation is over" severity note;
      report "------------------" severity note;
      send_report <= '1';
      wait for 1 nS;
    else
      wait;                             -- for ever
    end if IF_COUNT;
  end process main_proc;

  AngleGene_instanc : AngleGene port map
    (
      CLK       => CLK,
      RST       => RST(0),
      full_sync => full_sync,
      reg_sync  => reg_sync,
      angle_z   => angle_z,
      meta_data => meta_data
      );

  DataMonitorMulti_instanc : DataMonitorMultiZ generic map
    (
      meta_data_list => monitor_list
      )
    port map (
      CLK       => CLK,
      RST       => RST(0),
      full_sync => full_sync,
      reg_sync  => reg_sync,
      angle_z   => angle_z,
      meta_data => meta_data,
      send_report => send_report
      );
end architecture rtl;
