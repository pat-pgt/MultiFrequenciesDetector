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
  work.Cordic_E2E_lightfilter_Bundle_pac.Cordic_E2E_lightfilter_Bundle;


--! This entity is a DC FPGA test without the filtering
--!
--! It is an FPGA test. For more information, see in the Cordic_E2E_DC_test.
--! 

entity Cordic_E2E_lightfilter_CXX_test is
  generic (
    with_downsampling : natural                     := 1;
    nbre_Z_2_0_stages : integer range 4 to reg_size := 24;
    nbre_Y_2_0_stages : integer range 4 to reg_size := 24;
    extra_shifts      : integer range 0 to 7        := 0
    );
  port (
    CLK                         : in  std_logic := '0';
    RST                         : in  std_logic;
    full_sync                   : out std_logic;
    reg_sync                    : out std_logic;
    the_input                   : in  std_logic_vector(reg_size - 1 - 1 downto 0);
    input_x_not_y               : in  std_logic;
    X_prefilter_1               : out reg_type;
    Y_prefilter_1               : out reg_type;
    Z_prefilter_1               : out reg_type;
    X_Y_2_0                     : out reg_type;
    Y_Y_2_0                     : out reg_type;
    Z_Y_2_0                     : out reg_type;
    nbre_Z_2_0_stages_out       : out integer range 4 to reg_size;
    nbre_Y_2_0_stages_out       : out integer range 4 to reg_size;
    reg_size_4_verif            : out integer range 16 to 255;
    metadata_prefilter_1_note   : out std_logic_vector(StateNumbers_2_BitsNumbers(N_notes) - 1 downto 0);
    metadata_prefilter_1_octave : out std_logic_vector(StateNumbers_2_BitsNumbers(N_octaves) - 1 downto 0);
    metadata_Y_2_0_note         : out std_logic_vector(StateNumbers_2_BitsNumbers(N_notes) - 1 downto 0);
    metadata_Y_2_0_octave       : out std_logic_vector(StateNumbers_2_BitsNumbers(N_octaves) - 1 downto 0);
    metadata_Y_2_0_strobe       : out std_logic;
    strobe_stable               : out std_logic
    );
end entity Cordic_E2E_lightfilter_CXX_test;


architecture arch of Cordic_E2E_lightfilter_CXX_test is
  signal RST_monitor_Z_2_0         : natural := nbre_Z_2_0_stages + 5;
  signal RST_monitor_Y_2_0         : natural := nbre_Y_2_0_stages + 5;
  signal meta_data_prefilter_1_out : meta_data_t;
  signal meta_data_Y_2_0_out       : meta_data_t;

  signal report_cordic_bundle : std_logic := '0';
  signal SCZ_prefilter_1_out  : reg_sin_cos_z;
  signal SCZ_out_Y_2_0        : reg_sin_cos_z;
begin
  nbre_Z_2_0_stages_out <= nbre_Z_2_0_stages;
  nbre_Y_2_0_stages_out <= nbre_Y_2_0_stages;

  reg_size_4_verif <= reg_size;

  --! Since the C++ software may not know the reg_sin_cos_z type
  --!   we split into multiples signals.
  --! Since it is supposed to be read by a verification software,
  --!   we expose directly the shift registers,
  --!   it is supposed to read during the reg_sync.
  --! 
  metadata_prefilter_1_note   <= meta_data_prefilter_1_out.note;
  metadata_prefilter_1_octave <= meta_data_prefilter_1_out.octave;
  metadata_Y_2_0_note         <= meta_data_Y_2_0_out.note;
  metadata_Y_2_0_octave       <= meta_data_Y_2_0_out.octave;
  metadata_Y_2_0_strobe       <= meta_data_Y_2_0_out.strobe;

  X_Y_2_0 <= SCZ_out_Y_2_0.the_cos;
  Y_Y_2_0 <= SCZ_out_Y_2_0.the_sin;
  Z_Y_2_0 <= SCZ_out_Y_2_0.angle_z;

  X_prefilter_1 <= SCZ_prefilter_1_out.the_cos;
  Y_prefilter_1 <= SCZ_prefilter_1_out.the_sin;
  Z_prefilter_1 <= SCZ_prefilter_1_out.angle_z;

  Cordic_E2E_lightfilter_Bundle_instanc : Cordic_E2E_lightfilter_Bundle
    generic map(
      --! 0         : no down-sampling
      --! 1 or more : down-sampling without or with N extra rate
      with_downsampling,
      nbre_Z_2_0_stages => nbre_Z_2_0_stages,
      nbre_Y_2_0_stages => nbre_Y_2_0_stages,
      extra_shifts      => extra_shifts
      )
    port map(
      CLK                    ,
      RST                    ,
      the_input              ,
      input_x_not_y          ,
      reg_sync               ,
      full_sync              => full_sync,
      SCZ_pref_1_out         => SCZ_prefilter_1_out,
      SCZ_out_Y_2_0          => SCZ_out_Y_2_0,
      report_cordic_bundle_1 => report_cordic_bundle,
      meta_data_pref_1_out   => meta_data_prefilter_1_out,
      meta_data_Y_2_0_out    => meta_data_Y_2_0_out,
      strobe_stable          => strobe_stable
      );


end architecture arch;


configuration Cordic_E2E_lightfilter_CXX_test_dummy of Cordic_E2E_lightfilter_CXX_test is
  for arch
      for Cordic_E2E_lightfilter_Bundle_instanc : Cordic_E2E_lightfilter_Bundle
        use entity work.Cordic_E2E_lightfilter_Bundle;
        for arch
          for all : work.Prefilter_package.Prefilter_bundle
            use entity work.Prefilter_bundle;
            for arch
              for Prefilter_generate
                for bundle_elem : work.Prefilter_package.Prefilter_stage
                  use entity work.Prefilter_stage;                            
                  for arch
                    for selected_storage : work.Prefilter_package.Prefilter_RAM_Storage
                      use entity work.Prefilter_Dummy_storage(arch);
                    end for;
                  end for;
                end for;
              end for;
            end for;
          end for;
        end for;
      end for;
  end for;
end configuration Cordic_E2E_lightfilter_CXX_test_dummy;
