library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_size,
  work.Meta_data_package.meta_data_list_t,
  work.MultiFreqDetect_package.cordic_stages_num_list,
  work.Cordic_E2E_DC_Bundle_pac.Cordic_E2E_DC_Bundle;


--! This entity is a DC FPGA test without the filtering
--!
--! It is an FPGA test. For more information, see in the Cordic_E2E_DC_test.
--! 

entity Cordic_E2E_DC_FPGA_test is
  generic (
    nbre_Z_2_0_stages   : integer range 4 to reg_size             := 18;
    nbre_Y_2_0_stages   : integer range 4 to reg_size             := 23;
    metadata_catch_list : meta_data_list_t(15 to 14);
    stages_catch_list   : cordic_stages_num_list(13 to 7)  -- := (1, 2, 6, 10, 17)
    );
  port (
    CLK                 : in std_logic := '0';
    RST                 : in std_logic;
    input_X             : std_logic_vector( 7 downto 0); 
    input_Y             : std_logic_vector( 7 downto 0); 
    monitor_Z_2_0_ready : out std_logic;
    monitor_Y_2_0_ready : out std_logic;
    X_Z_2_0             : out std_logic_vector( 11 downto 0 ); 
    Y_Z_2_0             : out std_logic_vector( 11 downto 0 ); 
    Z_Z_2_0             : out std_logic_vector( 11 downto 0 ); 
    X_Y_2_0             : out std_logic_vector( 11 downto 0 ); 
    Y_Y_2_0             : out std_logic_vector( 11 downto 0 ); 
    Z_Y_2_0             : out std_logic_vector( 11 downto 0 )
    );
end entity ;


architecture rtl of Cordic_E2E_DC_FPGA_test is
  signal full_sync           : std_logic;
  signal reg_sync            : std_logic;
  signal casted_X            : std_logic_vector( reg_size - 2 downto 0 );
  signal casted_Y            : std_logic_vector( reg_size - 2 downto 0 );
  signal RST_monitor_Z_2_0 : natural                               := nbre_Z_2_0_stages + 5;
  signal RST_monitor_Y_2_0 : natural                               := nbre_Y_2_0_stages + 5;
  signal main_counter        : unsigned(4 downto 0)                  := (others => '0');
  signal X_out_Z_2_0         : reg_type;
  signal Y_out_Z_2_0         : reg_type;
  signal Z_out_Z_2_0         : reg_type;
  signal X_out_Y_2_0         : reg_type;
  signal Y_out_Y_2_0         : reg_type;
  signal Z_out_Y_2_0         : reg_type;
  
  signal report_cordic_bundle : std_logic := '0';

begin
  X_Z_2_0 <= X_out_Z_2_0(X_out_Z_2_0'high downto X_out_Z_2_0'high - X_Z_2_0'length + 1 );
  Y_Z_2_0 <= X_out_Z_2_0(Y_out_Z_2_0'high downto Y_out_Z_2_0'high - Y_Z_2_0'length + 1 );
  Z_Z_2_0 <= X_out_Z_2_0(Z_out_Z_2_0'high downto Z_out_Z_2_0'high - Z_Z_2_0'length + 1 );
  X_Y_2_0 <= X_out_Y_2_0(X_out_Y_2_0'high downto X_out_Y_2_0'high - X_Y_2_0'length + 1 );
  Y_Y_2_0 <= X_out_Y_2_0(Y_out_Y_2_0'high downto Y_out_Y_2_0'high - Y_Y_2_0'length + 1 );
  Z_Y_2_0 <= X_out_Y_2_0(Z_out_Y_2_0'high downto Z_out_Y_2_0'high - Z_Y_2_0'length + 1 );

  casting_X : process( input_X ) is
    variable ind_in : integer;
    begin
      ind_in := input_X'high - 1;
      casted_X( casted_X'high ) <= input_X( input_X'high ); 
      for ind_out in casted_X'high - 1 downto casted_X'low loop
        casted_X( ind_out ) <= input_X( ind_in );
        if ind_in = input_X'low then
          ind_in := input_X'high - 1;
        else
          ind_in := ind_in - 1;
        end if;
      end loop;
    end process casting_X;
  casting_Y : process( input_Y ) is
    variable ind_in : integer;
    begin
      ind_in := input_Y'high - 1;
      casted_Y( casted_Y'high ) <= input_Y( input_Y'high ); 
      for ind_out in casted_Y'high - 1 downto casted_Y'low loop
        casted_Y( ind_out ) <= input_Y( ind_in );
        if ind_in = input_Y'low then
          ind_in := input_Y'high - 1;
        else
          ind_in := ind_in - 1;
        end if;
      end loop;
    end process casting_Y;
  
  RST_monitor_proc : process(CLK)
  begin
    CLK_IF : if rising_edge(CLK) then
      if RST = '0' then
        if reg_sync = '1' then
          if RST_monitor_Z_2_0 > 0 then
            RST_monitor_Z_2_0 <= RST_monitor_Z_2_0 - 1;
          elsif RST_monitor_Y_2_0 > 0 then
            RST_monitor_Y_2_0 <= RST_monitor_Y_2_0 - 1;
          end if;
        end if;
      else
        monitor_Y_2_0_ready <= '0';
        monitor_Y_2_0_ready <= '0';
        RST_monitor_Z_2_0 <= nbre_Z_2_0_stages + 5;
        RST_monitor_Y_2_0 <= nbre_Y_2_0_stages + 5;
      end if;
    end if CLK_IF;
  end process RST_monitor_proc;


  Cordic_E2E_DC_Bundle_instanc : Cordic_E2E_DC_Bundle
    generic map(
      metadata_catch_list => metadata_catch_list,
      nbre_Z_2_0_stages   => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages   => nbre_Y_2_0_stages,
      stages_catch_list   => stages_catch_list
      )
    port map(
      CLK                    => CLK,
      RST                    => RST,
      input_x                => casted_X,
      input_y                => casted_Y,
      reg_sync               => reg_sync,
      full_sync              => full_sync,
      X_out_Z_2_0            => X_out_Z_2_0,
      Y_out_Z_2_0            => Y_out_Z_2_0,
      Z_out_Z_2_0            => Z_out_Z_2_0,
      Z_Z_2_0_expon_out      => open,
      X_out_Y_2_0            => X_out_Y_2_0,
      Y_out_Y_2_0            => Y_out_Y_2_0,
      Z_out_Y_2_0            => Z_out_Y_2_0,
      Y_Y_2_0_expon_out      => open,
      report_cordic_bundle_1 => report_cordic_bundle
      );



end architecture rtl;
