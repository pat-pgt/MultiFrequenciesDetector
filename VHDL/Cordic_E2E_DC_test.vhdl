library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_size,
  work.meta_data_package.N_octaves,
  work.meta_data_package.N_notes,
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
    with_auto_result    : boolean                                 := true;
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
  procedure results_real_stats( variable in_A : in real;
                                variable in_B : in real;
                                variable in_C : in real;
                                variable in_D : in real;
                                variable is_started : in boolean;
                                signal the_min : inout real; signal the_max : inout real ) is
    variable the_min_v, the_max_v : real;
  begin
    if is_started then
      the_min_v := the_min;
      if in_A < the_min_v then
        the_min_v := in_A;
      end if;
      if in_B < the_min_v then
        the_min_v := in_B;
      end if;
      if in_C < the_min_v then
        the_min_v := in_C;
      end if;
      if in_D < the_min_v then
        the_min_v := in_D;
      end if;
      the_min <= the_min_v;

      the_max_v := the_max;
      if in_A > the_max_v then
        the_max_v := in_A;
      end if;
      if in_B > the_max_v then
        the_max_v := in_B;
      end if;
      if in_C > the_max_v then
        the_max_v := in_C;
      end if;
      if in_D > the_max_v then
        the_max_v := in_D;
      end if;
      the_max <= the_max_v;
    end if;
  end procedure results_real_stats;
  signal CLK                   : std_logic                             := '0';
  signal RST                   : std_logic_vector(2*reg_size downto 0) := (others => '1');
  signal full_sync             : std_logic;
  signal reg_sync              : std_logic;
  signal RST_monitor_Z_2_0     : natural                               := nbre_Z_2_0_stages + 5;
  signal RST_monitor_Y_2_0     : natural                               := nbre_Y_2_0_stages + 5;
  signal main_counter          : unsigned(3 downto 0)                  := (others => '0');
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
  
  constant input_hi2_plus_low2    : real := (
    real(to_integer(signed(input_hi)))**2+real(to_integer(signed(input_low)))**2);
  constant input_mod_hi_low       : real := sqrt(input_hi2_plus_low2);
  constant input_minhi2_plus_low2 : real := (
    real(to_integer(signed(input_minhi)))**2+real(to_integer(signed(input_low)))**2);
  constant input_mod_minhi_low    : real := sqrt(input_minhi2_plus_low2);
  signal out_Z_2_0_MOD_X_Y_000_NO : real;
  signal out_Y_2_0_X2_000         : real;
  signal out_Z_2_0_MOD_X_Y_090_NO : real;
  signal out_Y_2_0_X2_090         : real;
  signal out_Z_2_0_MOD_X_Y_180_NO : real;
  signal out_Y_2_0_X2_180         : real;
  signal out_Z_2_0_MOD_X_Y_240_NO : real;
  signal out_Y_2_0_X2_240         : real;
  signal out_Z_2_0_mod_min        : real := real'high;
  signal out_Z_2_0_mod_max        : real := real'low;
  signal out_Y_2_0_mod_min        : real := real'high;
  signal out_Y_2_0_mod_max        : real := real'low;
  
  signal report_cordic_bundle     : std_logic := '0';

  signal auto_result_count        : natural := 0;

  constant angle_rotate_90        : unsigned( reg_size - 1 downto 0) := ( reg_size - 1 => '0', reg_size - 2 => '1', others=> '0' );
  constant angle_rotate_90_r      : real := real( to_integer( angle_rotate_90 ));
  signal diff_angle_0_90          : unsigned( reg_size - 1 downto 0);
  signal diff_angle_90_180        : unsigned( reg_size - 1 downto 0);
  signal diff_angle_180_240       : unsigned( reg_size - 1 downto 0);
  signal diff_angle_min           : real := real'high;
  signal diff_angle_max           : real := real'low;
--  signal last_angle               : signed( N_octaves * N_notes - 1 downto 0 );
begin
  
  main_proc : process
  begin
    COUNT_IF : if main_counter /= unsigned(to_signed(-2, main_counter'length)) and
                 main_counter /= unsigned(to_signed(-1, main_counter'length)) then
      CLK_IF : if CLK = '0' then
        if full_sync = '1' then
          main_counter <= main_counter + 1;
          if main_counter(main_counter'low + 1 downto main_counter'low) = "11" then
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
      report "End to end DC test, simulation is over" severity note;
      assert with_auto_result report"Detailed report is not selected, see some results with a wave viewer." severity note;
      assert with_auto_result report"To activate it set the generic with_auto_report to true." severity note;
      if with_auto_result then
        report "At the end of the Z to 0, the modules should grow by 16.44% for all the outputs at all time" severity note;
        report "The min is: " & real'image(out_Z_2_0_mod_min) & ", the max is: " & real'image(out_Z_2_0_mod_max) severity note;
        report "At the end of the Y to 0, the modules should grow by 35.59% for all the outputs at all time" severity note;
        report "The min is: " & real'image(out_Y_2_0_mod_min) & ", the max is: " & real'image(out_Y_2_0_mod_max) severity note;
        report "At the end of the Y to 0, the angles should increase by $4... between the 4 modules" severity note;
        report "  the computed ratios should be close to 1.00" severity note;
        report "The min is: " & to_string( diff_angle_min ) &
          ", the max is: " & to_string(diff_angle_max )
          severity note;

      end if;
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

  -- It is better to set the sensitivity to the output, rather than to the CLK
  --   as spurious changes can be seen
  module_Z_2_0 : process(all)
    variable SQRT_X2_Y2_000_normalized, SQRT_X2_Y2_090_normalized, SQRT_X2_Y2_180_normalized, SQRT_X2_Y2_240_normalized : real;
    variable is_started : boolean;
  begin
    if RST_monitor_Z_2_0 = 0 then
      SQRT_X2_Y2_000_normalized := sqrt( real(to_integer(signed(X_out_Z_2_0_000)))**2+real(to_integer(signed(Y_out_Z_2_0_000)))**2 ) /
                                   input_mod_hi_low;
      out_Z_2_0_MOD_X_Y_000_NO <= SQRT_X2_Y2_000_normalized;
      SQRT_X2_Y2_090_normalized := sqrt( real(to_integer(signed(X_out_Z_2_0_090)))**2+real(to_integer(signed(Y_out_Z_2_0_090)))**2 ) /
                                   input_mod_hi_low;
      out_Z_2_0_MOD_X_Y_090_NO <= SQRT_X2_Y2_090_normalized;
      SQRT_X2_Y2_180_normalized := sqrt( real(to_integer(signed(X_out_Z_2_0_180)))**2+real(to_integer(signed(Y_out_Z_2_0_180)))**2 ) /
                                   input_mod_minhi_low;
      out_Z_2_0_MOD_X_Y_180_NO <= SQRT_X2_Y2_180_normalized;
      SQRT_X2_Y2_240_normalized := sqrt( real(to_integer(signed(X_out_Z_2_0_240)))**2+real(to_integer(signed(Y_out_Z_2_0_240)))**2 ) /
                                   input_mod_minhi_low;
      out_Z_2_0_MOD_X_Y_240_NO <= SQRT_X2_Y2_240_normalized;
      if with_auto_result then
        is_started := true;
        results_real_stats( SQRT_X2_Y2_000_normalized, SQRT_X2_Y2_090_normalized, SQRT_X2_Y2_180_normalized, SQRT_X2_Y2_240_normalized,
                            is_started,
                            out_Z_2_0_mod_min, out_Z_2_0_mod_max );
      end if;
    end if;
  end process module_Z_2_0;

  -- It is better to set the sensitivity to the output, rather than to the CLK
  --   as spurious changes can be seen
  module_Y_2_0 : process(X_out_Y_2_0_000,X_out_Y_2_0_090,X_out_Y_2_0_180,X_out_Y_2_0_240,
                         Z_out_Y_2_0_000,Z_out_Y_2_0_090,Z_out_Y_2_0_180,Z_out_Y_2_0_240)
    variable X_000_normalized, X_090_normalized, X_180_normalized, X_240_normalized : real;
    variable diff_angle_0_90_v, diff_angle_90_180_v, diff_angle_180_240_v, diff_angle_240_0_v : unsigned( reg_size - 1 downto 0 );
    variable diff_angle_0_90_r, diff_angle_90_180_r, diff_angle_180_240_r, diff_angle_240_0_r : real;
    variable is_started : boolean;
  begin
    if RST_monitor_Y_2_0 = 0 then
      X_000_normalized := real(to_integer(signed(X_out_Y_2_0_000))) / input_mod_hi_low;
      out_Y_2_0_X2_000 <= X_000_normalized;
      X_090_normalized := real(to_integer(signed(X_out_Y_2_0_090))) / input_mod_hi_low;
      out_Y_2_0_X2_090 <= X_090_normalized;
      X_180_normalized := real(to_integer(signed(X_out_Y_2_0_180))) / input_mod_hi_low;
      out_Y_2_0_X2_180 <= X_180_normalized;
      X_240_normalized := real(to_integer(signed(X_out_Y_2_0_240))) / input_mod_hi_low;
      out_Y_2_0_X2_240 <= X_240_normalized;
      
      diff_angle_0_90_v  := unsigned(Z_out_Y_2_0_090) - unsigned(Z_out_Y_2_0_000);
      diff_angle_90_180_v  := unsigned(Z_out_Y_2_0_180) - unsigned(Z_out_Y_2_0_090);
      diff_angle_180_240_v := unsigned(Z_out_Y_2_0_240) - unsigned(Z_out_Y_2_0_180);
      diff_angle_0_90 <= diff_angle_0_90_v;
      diff_angle_90_180 <= diff_angle_90_180_v;
      diff_angle_180_240 <= diff_angle_180_240_v;
      AUTO_RESULT : if with_auto_result then
        is_started := true;
        results_real_stats( X_000_normalized, X_090_normalized, X_180_normalized, X_240_normalized,
                            is_started,
                            out_Y_2_0_mod_min, out_Y_2_0_mod_max );
        
          -- The diff angle is very very small.
          -- Then all the (angle) verification is done on one of the four.        
        diff_angle_240_0_v := unsigned(Z_out_Y_2_0_000) - unsigned(Z_out_Y_2_0_240);
        diff_angle_0_90_r := real( to_integer( diff_angle_0_90_v )) / angle_rotate_90_r;
        diff_angle_90_180_r := real( to_integer( diff_angle_90_180_v )) / angle_rotate_90_r;
        diff_angle_180_240_r := real( to_integer( diff_angle_180_240_v )) / angle_rotate_90_r;
        diff_angle_240_0_r := real( to_integer( diff_angle_240_0_v )) / angle_rotate_90_r;

        results_real_stats( diff_angle_0_90_r, diff_angle_90_180_r, diff_angle_180_240_r, diff_angle_240_0_r,
                            is_started,
                            diff_angle_min, diff_angle_max );

        AUTO_RESULT_GE_1 : if auto_result_count > 0 then
        auto_result_count <= auto_result_count + 1;
          
        end if AUTO_RESULT_GE_1;
      end if AUTO_RESULT;
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
      report_cordic_bundle_1 => report_cordic_bundle,
      meta_data_out          => open
      );


end architecture rtl;
