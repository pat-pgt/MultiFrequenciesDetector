library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_size,
  work.Meta_data_package.meta_data_list_t,
  work.MultiFreqDetect_package.cordic_stages_num_list,
  work.Cordic_E2E_DC_Bundle_pac.Cordic_E2E_DC_Bundle;


--! This entity is a DC test without the filtering
--!
--! There are 4 test modules.
--! The input of each one is a DC value as (dc,0),(0,dc),(-dc,0),(0,-dc).\n
--! The output of the first set should produce:
--! * X and a Y which sqrt(X2+Y2) is the DC value grown by about 16.44% (see below).
--! * Z close to 0. For fast reading, the 2's exponent is displayed.\n
--! The final output should produce:
--! * X which is the DC value grown by about 35.59%
--! * Y close to 0. For fast reading, the 2's exponent is displayed.\n
--! * Z which is the angle provided by the angle_gene many CLK cycles ago,
--!     shifted by 0, 90, 180 or 240 degrees.\n
--! Since the Cordic spins by CCW or CW, not CCW, 0 or CW,
--!   the 1/cosine(h) is not processed on each stage, but at the end.
--! Then the values grow a little bit.
--! The test uses the largest possible values to check there is no overflow.\n
--!
--! (TODO) The test checks the Z spin matches the meta-data information.
--!

entity Cordic_bundle_test_Z_to_0_Y_to_0 is
  generic (
    input_hi            : std_logic_vector(reg_size - 2 downto 0) := "0111111111111111111111111111111";
    input_low           : std_logic_vector(reg_size - 2 downto 0) := (others => '0');
    input_minhi         : std_logic_vector(reg_size - 2 downto 0) := "1000000000000000000000000000000";
    nbre_Z_2_0_stages   : integer range 4 to reg_size             := 18;
    nbre_Y_2_0_stages   : integer range 4 to reg_size             := 23;
    metadata_catch_list : meta_data_list_t(15 to 14);      --  := (
--      11                                                            => octave_note_to_meta_data(octave => 0, note => 0),
--      12                                                            => octave_note_to_meta_data(octave => 3, note => 2),
--      13                                                            => octave_note_to_meta_data(octave => 6, note => 4),
--      14                                                            => octave_all_notes_to_meta_data(octave => 4)
--      );
    stages_catch_list   : cordic_stages_num_list(13 to 7)  -- := (1, 2, 6, 10, 17)
    );
end entity Cordic_bundle_test_Z_to_0_Y_to_0;


architecture rtl of Cordic_bundle_test_Z_to_0_Y_to_0 is
  signal CLK                   : std_logic                             := '0';
  signal RST                   : std_logic_vector(2*reg_size downto 0) := (others => '1');
  signal full_sync             : std_logic;
  signal reg_sync              : std_logic;
  signal RST_monitor_Z_2_0     : natural                               := nbre_Z_2_0_stages + 5;
  signal RST_monitor_Y_2_0     : natural                               := nbre_Y_2_0_stages + 5;
  signal main_counter          : unsigned(4 downto 0)                  := (others => '0');
  -- Use the copy paste to allow to view using the VCD format
  -- This project does not yet supports the GHW
  signal X_out_Z_2_0_000       : reg_type;
  signal Y_out_Z_2_0_000       : reg_type;
  signal Z_out_Z_2_0_000       : reg_type;
  signal Z_Z_2_0_expon_out_000 : std_logic_vector(5 downto 0);
  signal X_out_Y_2_0_000       : reg_type;
  signal Y_out_Y_2_0_000       : reg_type;
  signal Z_out_Y_2_0_000       : reg_type;
  signal Y_Y_2_0_expon_out_000 : std_logic_vector(5 downto 0);
  signal X_out_Z_2_0_090       : reg_type;
  signal Y_out_Z_2_0_090       : reg_type;
  signal Z_out_Z_2_0_090       : reg_type;
  signal Z_Z_2_0_expon_out_090 : std_logic_vector(5 downto 0);
  signal X_out_Y_2_0_090       : reg_type;
  signal Y_out_Y_2_0_090       : reg_type;
  signal Z_out_Y_2_0_090       : reg_type;
  signal Y_Y_2_0_expon_out_090 : std_logic_vector(5 downto 0);
  signal X_out_Z_2_0_180       : reg_type;
  signal Y_out_Z_2_0_180       : reg_type;
  signal Z_out_Z_2_0_180       : reg_type;
  signal Z_Z_2_0_expon_out_180 : std_logic_vector(5 downto 0);
  signal X_out_Y_2_0_180       : reg_type;
  signal Y_out_Y_2_0_180       : reg_type;
  signal Z_out_Y_2_0_180       : reg_type;
  signal Y_Y_2_0_expon_out_180 : std_logic_vector(5 downto 0);
  signal X_out_Z_2_0_240       : reg_type;
  signal Y_out_Z_2_0_240       : reg_type;
  signal Z_out_Z_2_0_240       : reg_type;
  signal Z_Z_2_0_expon_out_240 : std_logic_vector(5 downto 0);
  signal X_out_Y_2_0_240       : reg_type;
  signal Y_out_Y_2_0_240       : reg_type;
  signal Z_out_Y_2_0_240       : reg_type;
  signal Y_Y_2_0_expon_out_240 : std_logic_vector(5 downto 0);
  
  constant input_hi2_plus_low2 : real := (
    real(to_integer(signed(input_hi)))**2+real(to_integer(signed(input_low)))**2);
  constant input_minhi2_plus_low2 : real := (
    real(to_integer(signed(input_minhi)))**2+real(to_integer(signed(input_low)))**2);
  signal out_Z_2_0_X2_plus_Y2_000 : real;
  signal out_Y_2_0_X2_000         : real;
  signal out_Z_2_0_X2_plus_Y2_090 : real;
  signal out_Y_2_0_X2_090         : real;
  signal out_Z_2_0_X2_plus_Y2_180 : real;
  signal out_Y_2_0_X2_180         : real;
  signal out_Z_2_0_X2_plus_Y2_240 : real;
  signal out_Y_2_0_X2_240         : real;

  signal report_cordic_bundle : std_logic := '0';

begin
  
  main_proc : process
  begin
    COUNT_IF : if main_counter /= unsigned(to_signed(-2, main_counter'length)) and
                 main_counter /= unsigned(to_signed(-1, main_counter'length)) then
      CLK_IF : if CLK = '0' then
        if full_sync = '1' then
          main_counter <= main_counter + 1;
          if main_counter(main_counter'low + 2 downto main_counter'low) = "111" then
            report "Progress " &
              integer'image(to_integer(main_counter(main_counter'high downto main_counter'low + 3))) &
              "/" & integer'image(2** (main_counter'length - 3) - 2)
              severity note;
          end if;
        end if;
        RST(RST'high-1 downto RST'low) <= RST(RST'high downto RST'low+1);
        RST(RST'high)                  <= '0';

      end if CLK_IF;
      CLK <= not CLK;
      wait for 10 ns;
    elsif main_counter = unsigned(to_signed(-2, main_counter'length)) then
      report"Simulation is over, report coming soon" severity note;

      report "Cordic stages individual reports are:" severity note;
      report "The explanations are in the Cordic_interm_monitor" severity note;
      report_cordic_bundle <= '1';
      main_counter         <= unsigned(to_signed(-1, main_counter'length));
      wait for 1 ns;
    else
      wait;
    end if COUNT_IF;
  end process main_proc;

  RST_monitor_proc : process(reg_sync)
  begin
    CLK_IF : if rising_edge(reg_sync) then
      if RST_monitor_Z_2_0 > 0 then
        RST_monitor_Z_2_0 <= RST_monitor_Z_2_0 - 1;
      elsif RST_monitor_Y_2_0 > 0 then
        RST_monitor_Y_2_0 <= RST_monitor_Y_2_0 - 1;
      end if;
    end if CLK_IF;
  end process RST_monitor_proc;

  module_Z_2_0 : process(all)
  begin
    if RST_monitor_Z_2_0 = 0 then
      out_Z_2_0_X2_plus_Y2_000 <= sqrt((
        real(to_integer(signed(X_out_Z_2_0_000)))**2+real(to_integer(signed(Y_out_Z_2_0_000)))**2) /
                                  input_hi2_plus_low2);
      out_Z_2_0_X2_plus_Y2_090 <= sqrt((
        real(to_integer(signed(X_out_Z_2_0_090)))**2+real(to_integer(signed(Y_out_Z_2_0_090)))**2) /
                                 input_hi2_plus_low2);
      out_Z_2_0_X2_plus_Y2_180 <= sqrt((
        real(to_integer(signed(X_out_Z_2_0_180)))**2+real(to_integer(signed(Y_out_Z_2_0_180)))**2) /
                                 input_hi2_plus_low2);
      out_Z_2_0_X2_plus_Y2_240 <= sqrt((
        real(to_integer(signed(X_out_Z_2_0_240)))**2+real(to_integer(signed(Y_out_Z_2_0_240)))**2) /
                                 input_hi2_plus_low2);
    end if;
  end process module_Z_2_0;

  module_Y_2_0 : process(all)
  begin
    if RST_monitor_Y_2_0 = 0 then
      out_Y_2_0_X2_000 <= sqrt(real(to_integer(signed(X_out_Y_2_0_000)))**2 / input_hi2_plus_low2);
      out_Y_2_0_X2_090 <= sqrt(real(to_integer(signed(X_out_Y_2_0_090)))**2 / input_hi2_plus_low2);
      out_Y_2_0_X2_180 <= sqrt(real(to_integer(signed(X_out_Y_2_0_180)))**2 / input_minhi2_plus_low2);
      out_Y_2_0_X2_240 <= sqrt(real(to_integer(signed(X_out_Y_2_0_240)))**2 / input_minhi2_plus_low2);
    end if;
  end process module_Y_2_0;

  Cordic_E2E_DC_Bundle_instanc_000 : Cordic_E2E_DC_Bundle
    generic map(
      metadata_catch_list => metadata_catch_list,
      nbre_Z_2_0_stages   => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages   => nbre_Y_2_0_stages,
      stages_catch_list   => stages_catch_list
      )
    port map(
      CLK                    => CLK,
      RST                    => RST(RST'low),
      input_x                => input_hi,
      input_y                => input_low,
      reg_sync               => reg_sync,
      full_sync              => full_sync,
      X_out_Z_2_0            => X_out_Z_2_0_000,
      Y_out_Z_2_0            => Y_out_Z_2_0_000,
      Z_out_Z_2_0            => Z_out_Z_2_0_000,
      Z_Z_2_0_expon_out      => Z_Z_2_0_expon_out_000,
      X_out_Y_2_0            => X_out_Y_2_0_000,
      Y_out_Y_2_0            => Y_out_Y_2_0_000,
      Z_out_Y_2_0            => Z_out_Y_2_0_000,
      Y_Y_2_0_expon_out      => Y_Y_2_0_expon_out_000,
      report_cordic_bundle_1 => report_cordic_bundle
      );

  Cordic_E2E_DC_Bundle_instanc_090 : Cordic_E2E_DC_Bundle
    generic map(
      metadata_catch_list => metadata_catch_list,
      nbre_Z_2_0_stages   => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages   => nbre_Y_2_0_stages,
      stages_catch_list   => stages_catch_list
      )
    port map(
      CLK                    => CLK,
      RST                    => RST(RST'low),
      input_x                => input_low,
      input_y                => input_hi,
      reg_sync               => open,
      full_sync              => open,
      X_out_Z_2_0            => X_out_Z_2_0_090,
      Y_out_Z_2_0            => Y_out_Z_2_0_090,
      Z_out_Z_2_0            => Z_out_Z_2_0_090,
      Z_Z_2_0_expon_out      => Z_Z_2_0_expon_out_090,
      X_out_Y_2_0            => X_out_Y_2_0_090,
      Y_out_Y_2_0            => Y_out_Y_2_0_090,
      Z_out_Y_2_0            => Z_out_Y_2_0_090,
      Y_Y_2_0_expon_out      => Y_Y_2_0_expon_out_090,
      report_cordic_bundle_1 => report_cordic_bundle
      );

  Cordic_E2E_DC_Bundle_instanc_180 : Cordic_E2E_DC_Bundle
    generic map(
      metadata_catch_list => metadata_catch_list,
      nbre_Z_2_0_stages   => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages   => nbre_Y_2_0_stages,
      stages_catch_list   => stages_catch_list
      )
    port map(
      CLK                    => CLK,
      RST                    => RST(RST'low),
      input_x                => input_minhi,
      input_y                => input_low,
      reg_sync               => open,
      full_sync              => open,
      X_out_Z_2_0            => X_out_Z_2_0_180,
      Y_out_Z_2_0            => Y_out_Z_2_0_180,
      Z_out_Z_2_0            => Z_out_Z_2_0_180,
      Z_Z_2_0_expon_out      => Z_Z_2_0_expon_out_180,
      X_out_Y_2_0            => X_out_Y_2_0_180,
      Y_out_Y_2_0            => Y_out_Y_2_0_180,
      Z_out_Y_2_0            => Z_out_Y_2_0_180,
      Y_Y_2_0_expon_out      => Y_Y_2_0_expon_out_180,
      report_cordic_bundle_1 => report_cordic_bundle
      );

  Cordic_E2E_DC_Bundle_instanc_240 : Cordic_E2E_DC_Bundle
    generic map(
      metadata_catch_list => metadata_catch_list,
      nbre_Z_2_0_stages   => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages   => nbre_Y_2_0_stages,
      stages_catch_list   => stages_catch_list
      )
    port map(
      CLK                    => CLK,
      RST                    => RST(RST'low),
      input_x                => input_low,
      input_y                => input_minhi,
      reg_sync               => open,
      full_sync              => open,
      X_out_Z_2_0            => X_out_Z_2_0_240,
      Y_out_Z_2_0            => Y_out_Z_2_0_240,
      Z_out_Z_2_0            => Z_out_Z_2_0_240,
      Z_Z_2_0_expon_out      => Z_Z_2_0_expon_out_240,
      X_out_Y_2_0            => X_out_Y_2_0_240,
      Y_out_Y_2_0            => Y_out_Y_2_0_240,
      Z_out_Y_2_0            => Z_out_Y_2_0_240,
      Y_Y_2_0_expon_out      => Y_Y_2_0_expon_out_240,
      report_cordic_bundle_1 => report_cordic_bundle
      );


end architecture rtl;
