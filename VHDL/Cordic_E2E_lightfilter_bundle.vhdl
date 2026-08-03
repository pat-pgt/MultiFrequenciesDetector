library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,

  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_size,
  work.InterModule_formats.reg_sin_cos_z,

  work.Meta_data_package.meta_data_t,
  work.Meta_data_package.meta_data_list_t,

  work.MultiFreqDetect_package.cordic_stages_num_list;

package Cordic_E2E_lightfilter_Bundle_pac is
--! This entity is an end to end bundle without the filtering.
--! It is intended to test, using a software or implemented into a FGPA.
  component Cordic_E2E_lightfilter_Bundle is
    generic (
      -- there is no down-sampling
      -- 1         : down-sampling without extra rate
      -- 2 or more : down-sampling with N extra rate
      with_downsampling : natural              := 1;
      nbre_Z_2_0_stages : natural;
      nbre_Y_2_0_stages : natural;
      extra_shifts      : integer range 0 to 7 := 0
      );
    port (
      CLK                    : in  std_logic;
      RST                    : in  std_logic;
      the_input              : in  std_logic_vector(reg_size - 1 - 1 downto 0);
      input_x_not_y          : in  std_logic;
      reg_sync               : out std_logic;
      full_sync              : out std_logic;
      SCZ_pref_1_out         : out reg_sin_cos_z;
      SCZ_pref_2_out         : out reg_sin_cos_z;
      SCZ_out_Y_2_0          : out reg_sin_cos_z;
      report_cordic_bundle_1 : in  std_logic;
      meta_data_pref_1_out   : out meta_data_t;
      meta_data_pref_2_out   : out meta_data_t;
      meta_data_Y_2_0_out    : out meta_data_t;
      strobe_stable          : out std_logic
      );
  end component Cordic_E2E_lightfilter_Bundle;

end package Cordic_E2E_lightfilter_Bundle_pac;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.MultiFreqDetect_package.cordic_stages_num_list,
  work.MultiFreqDetect_package.prefilter_stages_offset_list,
  work.Input_modules.all,
  work.Cordic_package.all,
  work.Downsampling_package.Downsampling_bundle,
  work.Prefilter_package.Prefilter_bundle;

--! This entity is an end to end bundle without the filtering.
--! It is intended to test, using a software or implemented into a FGPA.
entity Cordic_E2E_lightfilter_Bundle is
  generic (
    --! 0         : no down-sampling
    --! 1 or more : down-sampling without or with N extra rate
    with_downsampling : natural              := 0;
    nbre_Z_2_0_stages : natural;
    nbre_Y_2_0_stages : natural;
    extra_shifts      : integer range 0 to 7 := 0
    );
  port (
    CLK                    : in  std_logic;
    RST                    : in  std_logic;
    the_input              : in  std_logic_vector(reg_size - 1 - 1 downto 0);
    input_x_not_y          : in  std_logic;
    reg_sync               : out std_logic;
    full_sync              : out std_logic;
    SCZ_pref_1_out         : out reg_sin_cos_z;
    SCZ_pref_2_out         : out reg_sin_cos_z;
    SCZ_out_Y_2_0          : out reg_sin_cos_z;
    report_cordic_bundle_1 : in  std_logic;
    meta_data_pref_1_out   : out meta_data_t;
    meta_data_pref_2_out   : out meta_data_t;
    meta_data_Y_2_0_out    : out meta_data_t;
    strobe_stable          : out std_logic
    );
end entity Cordic_E2E_lightfilter_Bundle;


architecture arch of Cordic_E2E_lightfilter_Bundle is
  signal angle_z                : reg_type;
  signal input_x                : std_logic_vector(reg_size - 2 downto 0);
  signal input_y                : std_logic_vector(reg_size - 2 downto 0);
  signal reg_sync_interm        : std_logic;
  signal meta_data_1            : meta_data_t;
  signal meta_data_2            : meta_data_t;
  signal meta_data_5            : meta_data_t;
  signal meta_data_Z_2_0_out    : meta_data_t;
  signal meta_data_DS_out       : meta_data_t;
  signal scz_1                  : reg_sin_cos_z;
  signal scz_DS_out             : reg_sin_cos_z;
  signal scz_Z_2_0_out          : reg_sin_cos_z;
  signal scz_3                  : reg_sin_cos_z;
  signal report_cordic_bundle_2 : std_logic                      := '0';
  signal xy_is_neg              : std_logic_vector(1 downto 0);
  constant null_vector          : std_logic_vector(0 downto 1)   := "";
  constant stages_catch_list    : cordic_stages_num_list(1 to 0) := (others => 1);  -- 0 length
  constant metadata_catch_list  : meta_data_list_t(1 to 0)       := (others => octave_note_to_meta_data(0, 0));  -- 0 length

  constant stages_offsets : prefilter_stages_offset_list(0 to 0) := (others => 1.0);
begin

  switch_proc : process (the_input, input_x_not_y) is
  begin
    if input_x_not_y = '1' then
      input_x <= the_input;
      input_y <= ( others => '0' );
    else
      input_x <= ( others => '0' );
      input_y <= the_input;
    end if;
  end process switch_proc;

  angle_gene_instanc : AngleGene
    generic map
    (
      debug_mode => false
      )
    port map (
      CLK       => CLK,
      RST       => RST,
      reg_sync  => reg_sync,
      full_sync => full_sync,
      angle_z   => angle_z,
      meta_data => meta_data_1
      );

  cordic_first_stage_Z_2_0_instanc : Cordic_FirstStage_Z_to_0
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync_in   => reg_sync,
      reg_sync_out  => reg_sync_interm,
      angle_z       => angle_z,
      meta_data_in  => meta_data_1,
      meta_data_out => meta_data_2,
      input_x       => input_x,
      input_y       => input_y,
      scz_out       => scz_1);


  cordic_bundle_Z_2_0_instanc : Cordic_Bundle_Z_to_0 generic map (
    stages_nbre         => nbre_Z_2_0_stages,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list
    )
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync_interm,
      full_sync     => full_sync,
      meta_data_in  => meta_data_2,
      meta_data_out => meta_data_Z_2_0_out,
      scz_in        => scz_1,
      scz_out       => scz_Z_2_0_out,
      report_in     => report_cordic_bundle_1,
      report_out    => report_cordic_bundle_2);

  assert with_downsampling > 0
    report "With the light filters, it is a non sense to run this test"
    severity error;

  --! The first set of pre-filters are instantiated as they should
  Prefilter_bundle_1 : Prefilter_bundle
    generic map (
      --! Defines the number of stages and their offsets ratios
      stages_offsets)
    port map (
      CLK,
      RST,
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_Z_2_0_out,
      meta_data_out => meta_data_pref_1_out,
      scz_in        => scz_z_2_0_out,
      scz_out       => scz_pref_1_out
      );

  Downsampling_bundle_instanc : Downsampling_bundle
    generic map (
      extra_downsampling => with_downsampling)
    port map (
      CLK,
      RST,
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_pref_1_out,
      meta_data_out => meta_data_DS_out,
      scz_in        => SCZ_pref_1_out,
      scz_out       => scz_DS_out,
      xy_is_neg     => xy_is_neg
      );

  --! The filter should be the final "real" filter
  --! In this test it is replaced by the pre-filter
  Prefilter_bundle_2 : Prefilter_bundle
    generic map (
      --! Defines the number of stages and their offsets ratios
      stages_offsets)
    port map (
      CLK,
      RST,
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_DS_out,
      meta_data_out => meta_data_pref_2_out,
      scz_in        => scz_DS_out,
      scz_out       => scz_pref_2_out
      );

  cordic_first_stage_Y_2_0_instanc_DS : Cordic_FirstStage_Y_to_0
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_pref_2_out,
      meta_data_out => meta_data_5,
      scz_in        => scz_pref_2_out,
      scz_out       => scz_3,
      xy_is_neg     => xy_is_neg);



  cordic_bundle_Y_2_0_instanc : Cordic_Bundle_Y_to_0 generic map (
    stages_nbre         => nbre_Y_2_0_stages,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list,
    extra_shifts        => extra_shifts
    )
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync_interm,
      full_sync     => full_sync,
      meta_data_in  => meta_data_5,
      meta_data_out => meta_data_Y_2_0_out,
      scz_in        => scz_3,
      scz_out       => SCZ_out_Y_2_0,
      report_in     => report_cordic_bundle_2,
      strobe_stable => strobe_stable);


end architecture arch;
