library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_size,
  work.InterModule_formats.reg_sin_cos_z,
  work.Meta_data_package.N_notes,
  work.Meta_data_package.N_octaves,
  work.Meta_data_package.meta_data_t,
  work.Meta_data_package.meta_data_list_t,
  work.MultiFreqDetect_package.cordic_stages_num_list,
  work.Cordic_E2E_DC_Bundle_pac.Cordic_E2E_DC_Bundle;


--! This entity is a DC FPGA test without the filtering
--!
--! It is an FPGA test. For more information, see in the Cordic_E2E_DC_test.
--! 

entity Cordic_E2E_DC_CXX_test is
  generic (
    with_downsampling   : natural := 1;
    nbre_Z_2_0_stages   : integer range 4 to reg_size := 18;
    nbre_Y_2_0_stages   : integer range 4 to reg_size := 18;
    metadata_catch_list : meta_data_list_t(15 to 14);
    stages_catch_list   : cordic_stages_num_list(13 to 7)  -- := (1, 2, 6, 10, 17)
    );
  port (
    CLK                   : in  std_logic                   := '0';
    RST                   : in  std_logic;
    full_sync             : out std_logic;
    reg_sync              : out std_logic;
    input_X               : in  std_logic_vector( reg_size - 1 - 1 downto 0 );
    input_Y               : in  std_logic_vector( reg_size - 1 - 1 downto 0 );
    X_Z_2_0               : out reg_type;
    Y_Z_2_0               : out reg_type;
    Z_Z_2_0               : out reg_type;
    X_Y_2_0               : out reg_type;
    Y_Y_2_0               : out reg_type;
    Z_Y_2_0               : out reg_type;
    nbre_Z_2_0_stages_out : out integer range 4 to reg_size;
    nbre_Y_2_0_stages_out : out integer range 4 to reg_size;
    reg_size_4_verif      : out integer range 16 to 255;
    metadata_Z_2_0_note   : out std_logic_vector(StateNumbers_2_BitsNumbers(N_notes) - 1 downto 0);
    metadata_Z_2_0_octave : out std_logic_vector(StateNumbers_2_BitsNumbers(N_octaves) - 1  downto 0);
    metadata_Y_2_0_note   : out std_logic_vector(StateNumbers_2_BitsNumbers(N_notes) - 1 downto 0);
    metadata_Y_2_0_octave : out std_logic_vector(StateNumbers_2_BitsNumbers(N_octaves) - 1  downto 0);
    metadata_Y_2_0_strobe : out std_logic
    );
end entity Cordic_E2E_DC_CXX_test;


architecture arch of Cordic_E2E_DC_CXX_test is
  signal RST_monitor_Z_2_0    : natural := nbre_Z_2_0_stages + 5;
  signal RST_monitor_Y_2_0    : natural := nbre_Y_2_0_stages + 5;
  signal meta_data_Z_2_0_out  : meta_data_t;
  signal meta_data_Y_2_0_out  : meta_data_t;

  signal report_cordic_bundle : std_logic := '0';
  signal SCZ_out_Y_2_0        : reg_sin_cos_z;
begin
    nbre_Z_2_0_stages_out <= nbre_Z_2_0_stages;
    nbre_Y_2_0_stages_out <= nbre_Y_2_0_stages;

    reg_size_4_verif      <= reg_size;

    --! Since the C++ software may not know the reg_sin_cos_z type
    --!   we split into multiples signals.
    --! Since it is supposed to be read by a verification software,
    --!   we expose directly the shift registers,
    --!   it is supposed to read during the reg_sync.
    --! 
    metadata_Z_2_0_note   <= meta_data_Z_2_0_out.note;
    metadata_Z_2_0_octave <= meta_data_Z_2_0_out.octave;
    metadata_Y_2_0_note   <= meta_data_Y_2_0_out.note;
    metadata_Y_2_0_octave <= meta_data_Y_2_0_out.octave;
    metadata_Y_2_0_strobe <= meta_data_Y_2_0_out.strobe;

    X_Y_2_0 <= SCZ_out_Y_2_0.the_sin;
    Y_Y_2_0 <= SCZ_out_Y_2_0.the_cos;
    Z_Y_2_0 <= SCZ_out_Y_2_0.angle_z;

  Cordic_E2E_DC_Bundle_instanc : Cordic_E2E_DC_Bundle
    generic map(
      --! 0         : no down-sampling
      --! 1 or more : down-sampling without or with N extra rate
      with_downsampling,
      metadata_catch_list => metadata_catch_list,
      nbre_Z_2_0_stages   => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages   => nbre_Y_2_0_stages,
      stages_catch_list   => stages_catch_list
      )
    port map(
      CLK                    => CLK,
      RST                    => RST,
      input_x                => input_X,
      input_y                => input_Y,
      reg_sync               => reg_sync,
      full_sync              => full_sync,
      X_out_Z_2_0            => X_Z_2_0,
      Y_out_Z_2_0            => Y_Z_2_0,
      Z_out_Z_2_0            => Z_Z_2_0,
      Z_Z_2_0_expon_out      => open,
      SCZ_out_Y_2_0          => SCZ_out_Y_2_0,
      report_cordic_bundle_1 => report_cordic_bundle,
      meta_data_Z_2_0_out    => meta_data_Z_2_0_out,
      meta_data_Y_2_0_out    => meta_data_Y_2_0_out
      );


end architecture arch;
