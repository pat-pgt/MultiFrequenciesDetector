library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.MultiFreqDetect_package.all,
  work.Input_modules.all,
  work.Cordic_package.all;


--! This entity is an end to end test without the filtering
--! The input is a DC signal (X and Y)
--! The final output should produce approximately (see below) the DC value,
--!   as the square root of X2 plus Y2,
--!   and an angle (Z) should continuously spinning according with the frequency
--! Since the Cordic spins by +1 or -1, not +1, 0 or -1,
--!   the 1/cosine(h) is not processed on each stage, but at the end.
--! This test does not include the global Z to 0 constant multiplied by the global Y to 0.
--!
--! (TODO) The test should be done with different DC values on X and Y.
--!
--! The values are generics with the actual default values
--!   in order to run a large automated test.
--!
--! (TODO) The test checks the Z spin matches the meta-data information.
--!

entity Cordic_bundle_test_Z_to_0_Y_to_0 is
  generic (
    input_x             : std_logic_vector(30 downto 0)  := "0001000000000000000000000000000";
    input_y             : std_logic_vector(30 downto 0)  := (others => '0');
    metadata_catch_list : meta_data_list_t(11 to 14)     := (
      11                                                            => octave_note_to_meta_data(octave => 0, note => 0),
      12                                                            => octave_note_to_meta_data(octave => 3, note => 2),
      13                                                            => octave_note_to_meta_data(octave => 6, note => 4),
      14                                                            => octave_all_notes_to_meta_data(octave => 4)
      );
    stages_catch_list   : cordic_stages_num_list(3 to 7) := (1, 2, 6, 10, 17)
    );
end entity Cordic_bundle_test_Z_to_0_Y_to_0;


architecture rtl of Cordic_bundle_test_Z_to_0_Y_to_0 is
  signal CLK                                     : std_logic                     := '0';
  signal RST                                     : std_logic_vector(10 downto 0) := (others => '1');
  signal main_counter                            : unsigned(3 downto 0)          := (others => '0');
  signal reg_sync_ag, reg_sync_interm, full_sync : std_logic;
  signal angle_z                                 : reg_type;
  signal meta_data_1                             : meta_data_t;
  signal meta_data_2                             : meta_data_t;
  signal meta_data_3                             : meta_data_t;
  signal meta_data_4                             : meta_data_t;
  signal meta_data_5                             : meta_data_t;
  signal meta_data_6                             : meta_data_t;
  signal scz_1, scz_2, scz_3                     : reg_sin_cos_z;
  signal X_out_Z_2_0                             : reg_type;
  signal Y_out_Z_2_0                             : reg_type;
  signal Z_out_Z_2_0                             : reg_type;
  signal Z_Z_2_0_expon_out                       : std_logic_vector(5 downto 0);
  signal X_out_Y_2_0                             : reg_type;
  signal Y_out_Y_2_0                             : reg_type;
  signal Z_out_Y_2_0                             : reg_type;
  signal Y_Y_2_0_expon_out                       : std_logic_vector(5 downto 0);
  constant input_X2_plus_Y2                      : real := (
      real(to_integer(signed(input_y)))**2+real(to_integer(signed(input_x)))**2);
  signal out_Z_2_0_X2_plus_Y2                    : real;                  
  signal out_Y_2_0_X2                            : real;
  
  signal report_cordic_bundle_1, report_cordic_bundle_2 : std_logic := '0';
  
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
      report_cordic_bundle_1 <= '1';
      main_counter           <= unsigned(to_signed(-1, main_counter'length));
      wait for 1 ns;
    else
      wait;
    end if COUNT_IF;
  end process main_proc;

  module_X_Y_Z_2_0 : process( X_out_Z_2_0, Y_out_Z_2_0, X_out_Y_2_0 )
    begin
      out_Z_2_0_X2_plus_Y2 <= (
        real(to_integer(signed(X_out_Z_2_0)))**2+real(to_integer(signed(Y_out_Z_2_0)))**2) /
                            input_X2_plus_Y2;
      out_Y_2_0_X2 <= real(to_integer(signed( X_out_Y_2_0 )))**2 / input_X2_plus_Y2;
    end process module_X_Y_Z_2_0;
  
  angle_gene_instanc : AngleGene
    generic map
    (
      debug_mode => false
      )
    port map (
      CLK       => CLK,
      RST       => RST(RST'low),
      reg_sync  => reg_sync_ag,
      full_sync => full_sync,
      angle_z   => angle_z,
      meta_data => meta_data_1
      );

  cordic_first_stage_Z_2_0_instanc : Cordic_FirstStage_Z_to_0
    port map (
      CLK           => CLK,
      RST           => RST(RST'low),
      reg_sync_in   => reg_sync_ag,
      reg_sync_out  => reg_sync_interm,
      angle_z       => angle_z,
      meta_data_in  => meta_data_1,
      meta_data_out => meta_data_2,
      input_x       => input_x,
      input_y       => input_y,
      scz           => scz_1);

  cordic_bundle_Z_2_0_instanc : Cordic_Bundle_Z_to_0 generic map (
    stages_nbre         => 15,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list
    )
    port map (
      CLK           => CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync_interm,
      full_sync     => full_sync,
      meta_data_in  => meta_data_2,
      meta_data_out => meta_data_3,
      scz_in        => scz_1,
      scz_out       => scz_2,
      X_out         => X_out_Z_2_0,
      Y_out         => Y_out_Z_2_0,
      Z_out         => Z_out_Z_2_0,
      Z_expon_out   => Z_Z_2_0_expon_out,
      report_in     => report_cordic_bundle_1,
      report_out    => report_cordic_bundle_2);


  -- Prefilter and filter
  meta_data_4 <= meta_data_3;

  cordic_first_stage_Y_2_0_instanc : Cordic_FirstStage_Y_to_0
    port map (
      CLK           => CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_4,
      meta_data_out => meta_data_5,
      scz_in        => scz_2,
      scz_out       => scz_3);

  cordic_bundle_Y_2_0_instanc : Cordic_Bundle_Y_to_0 generic map (
    stages_nbre         => 25,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list
    )
    port map (
      CLK           => CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync_interm,
      full_sync     => full_sync,
      meta_data_in  => meta_data_5,
      meta_data_out => meta_data_6,
      scz_in        => scz_3,
      X_out         => X_out_Y_2_0,
      Y_out         => Y_out_Y_2_0,
      Z_out         => Z_out_Y_2_0,
      Y_expon_out   => Y_Y_2_0_expon_out,
      report_in     => report_cordic_bundle_2);


end architecture rtl;
