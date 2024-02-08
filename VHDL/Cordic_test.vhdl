library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.all,
  work.Cordic_package.all;

entity Cordic_bundle_test_Z_to_0 is
end entity Cordic_bundle_test_Z_to_0;

architecture rtl of Cordic_bundle_test_Z_to_0 is
  signal CLK                                   : std_logic                     := '0';
  signal RST                                   : std_logic_vector(10 downto 0)  := (others => '1');
  signal main_counter                          : unsigned(3 downto 0) := (others => '0');
  signal reg_sync_ag, reg_sync_interm, full_sync                   : std_logic;
  signal input_x                               : std_logic_vector(30 downto 0) := "0001000000000000000000000000000";
  signal input_y                               : std_logic_vector(30 downto 0) := (others => '0');
  signal angle_z                               : reg_type;
  signal meta_data_1, meta_data_2, meta_data_3 : meta_data_t;
  signal scz_1                                 : reg_sin_cos_z;
  signal X_out                                 : std_logic_vector(arithm_size - 1 downto 0);
  signal Y_out                                 : std_logic_vector(arithm_size - 1 downto 0);
  signal Z_expon_out                           : std_logic_vector(5 downto 0);
  signal X2_plus_Y2 : std_logic_vector( 31 downto 0 );
begin
  X2_plus_Y2 <= std_logic_vector( to_unsigned(
    to_integer( signed( input_y( input_y'high downto input_y'high - 14) ))**2+
    to_integer( signed( input_x( input_x'high downto input_x'high - 14) ))**2,
    X2_plus_Y2'length ));

  
  main_proc : process
  begin
    if main_counter /= unsigned( to_signed(-1, main_counter'length)) then
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
    else
      wait;
    end if;
  end process main_proc;

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

  cordic_first_stage_instanc : Cordic_FirstStage_Z_to_0
    port map (
      CLK           => CLK,
      RST           => RST(RST'low),
      reg_sync_in      => reg_sync_ag,
      reg_sync_out      => reg_sync_interm,
      angle_z       => angle_z,
      meta_data_in  => meta_data_1,
      meta_data_out => meta_data_2,
      input_x       => input_x,
      input_y       => input_y,
      scz           => scz_1);

  cordic_bundle_instanc : Cordic_Bundle_Z_to_0 generic map (
    debug_mode  => false,
    stages_nbre => 12
    )
    port map (
      CLK           => CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_2,
      meta_data_out => meta_data_3,
      scz_in        => scz_1,
      X_out         => X_out,
      Y_out         => Y_out,
      Z_expon_out   => Z_expon_out);


end architecture rtl;
