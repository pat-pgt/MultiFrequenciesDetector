library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;
--! @brief Prefilter stage
--!
--! This is a pair of sine and cosine calculation
--! with their associated memory storage and
--! delay for the metadata.
entity Prefilter_stage is
  generic (
    the_stage_offset          : real    := 1.0;
    prefilter_not_lightfilter : boolean := true
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_stage;

architecture arch of Prefilter_stage is
  signal shifts_calc               : shifts_IIR_data;
  signal sin_diff_shift            : reg_type;
  signal cos_diff_shift            : reg_type;
  signal sin_shift_add             : reg_type;
  signal cos_shift_add             : reg_type;
  signal meta_data_diff            : meta_data_t;
  -- Is nice for testing with 3 frequencies
  --   separately from the RAM test
  signal output_from_RAM           : reg_sin_cos_z;
  -- Do not touch
  constant prefilter_diff_latency  : positive := 1;
  -- In case of a large number of octaves
  --   the shift can take more steps.
  constant prefilter_shift_latency : positive := 1;
  -- Do not touch
  constant prefilter_add_latency   : positive := 1;
  constant prefilter_all_latency   : positive := prefilter_diff_latency +
                                               prefilter_shift_latency +
                                               prefilter_add_latency;
  -- The latency of the diff module is already handled
  --   in the shift "detector" component
  signal meta_data_delay : meta_data_list_t(prefilter_all_latency - 1 downto 1);
  signal scz_delayed     : reg_sin_cos_z;
begin
  meta_data_out <= meta_data_delay(meta_data_delay'low);

--  assert state_var_delay_s'length > 1 and state_var_delay_c'length > 1
--    report "Internal error, the delay should be at least 2 reg_sync"
--    severity failure;


  main_proc : process (CLK) is
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        -- The metadata is transferred using parallel mode
        meta_data_delay(meta_data_delay'high - 1 downto meta_data_delay'low) <=
          meta_data_delay(meta_data_delay'high downto meta_data_delay'low + 1);
        meta_data_delay(meta_data_delay'high) <= meta_data_diff;
      -- The state variable delay line has nothing to do during the sync
      -- only load
--        state_var_delay_s(state_var_delay_s'high) <= output_from_RAM_S;
--        state_var_delay_c(state_var_delay_c'high) <= output_from_RAM_C;
      end if REGSYNC_IF;
    end if CLK_if;
  end process main_proc;


  selected_storage : Prefilter_RAM_Storage
    generic map (
      Prefilter_latency => prefilter_all_latency)
    port map (
      CLK,
      RST,
      reg_sync,
      -- Out for the filter, then in for the storage
      meta_data_in  => meta_data_out,
      -- Out for the filter, then in for the storage
      meta_data_out => meta_data_in,
      -- Out for the filter, then in for the storage
      scz_in        => scz_out,
      -- Out for the filter, then in for the storage
      scz_out       => output_from_RAM
      );

  delay_IIR : Prefilter_Delay generic map (
    latency => prefilter_all_latency - 1
    )
    port map (
      CLK      => CLK,
      RST      => RST,
      reg_sync => reg_sync,
      scz_in   => output_from_RAM,
      scz_out  => scz_delayed
      );

  sine_IIR_diff : Prefilter_IIR_stage_diff port map(
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    state_var_in  => output_from_RAM.the_sin,
    data_out      => sin_diff_shift,
    data_input_in => scz_in.the_sin);

  sine_IIR_shift : Prefilter_IIR_stage_shift port map(
    CLK         => CLK,
    RST         => RST,
    reg_sync    => reg_sync,
    shifts_calc => shifts_calc,
    data_in     => sin_diff_shift,
    data_out    => sin_shift_add);

  sine_IIR_add : Prefilter_IIR_stage_add port map(
    CLK                => CLK,
    RST                => RST,
    reg_sync           => reg_sync,
    state_var_in       => scz_delayed.the_sin,
    data_in            => sin_shift_add,
    state_var_data_out => scz_out.the_sin);

  cose_IIR_diff : Prefilter_IIR_stage_diff port map(
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    state_var_in  => output_from_RAM.the_cos,
    data_out      => cos_diff_shift,
    data_input_in => scz_in.the_cos);

  cose_IIR_shift : Prefilter_IIR_stage_shift port map(
    CLK         => CLK,
    RST         => RST,
    reg_sync    => reg_sync,
    shifts_calc => shifts_calc,
    data_in     => cos_diff_shift,
    data_out    => cos_shift_add);

  cose_IIR_add : Prefilter_IIR_stage_add port map(
    CLK                => CLK,
    RST                => RST,
    reg_sync           => reg_sync,
    state_var_in       => scz_delayed.the_cos,
    data_in            => cos_shift_add,
    state_var_data_out => scz_out.the_cos);

  meta_data_compute : Prefilter_metadata_and_shifts_compute generic map (
    the_stage_offset,
    prefilter_not_lightfilter,
    latency => prefilter_all_latency
    )
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync,
      meta_data_in  => meta_data_in,
      meta_data_out => meta_data_diff,
      shifts_calc   => shifts_calc);

end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Prefilter_package.all;
--! @brief Prefilter bundle
--!
--! This is the bundle for the whose wants more stages
entity Prefilter_bundle is
  generic (
    --! Defines the number of stages and their offsets ratios
    stages_offsets            : prefilter_stages_offset_list;
    prefilter_not_lightfilter : boolean := true
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z
    );
end entity Prefilter_bundle;

architecture arch of Prefilter_bundle is
  signal scz_interm       : reg_sin_cos_z_list(stages_offsets'length downto 0);
  signal meta_data_interm : meta_data_list_t(stages_offsets'length downto 0);
begin
  assert stages_offsets'length > 0 report "The number of pre-filter stages should not be 0" severity failure;
  assert false report "Instanciating " & integer'image(stages_offsets'length) & " Pre filters stages" severity note;

  meta_data_interm(meta_data_interm'high) <= meta_data_in;
  meta_data_out                           <= meta_data_interm(meta_data_interm'low);
  scz_interm(scz_interm'high)             <= scz_in;
  scz_out                                 <= scz_interm(scz_interm'low);

  Prefilter_generate : for ind in 0 to stages_offsets'length - 1 generate
    bundle_elem : Prefilter_stage
      generic map (
        the_stage_offset          => stages_offsets(stages_offsets'low - ind),
        prefilter_not_lightfilter => prefilter_not_lightfilter
      )
      port map (
        CLK           => CLK,
        RST           => RST,
        reg_sync      => reg_sync,
        meta_data_in  => meta_data_interm(meta_data_interm'low + ind + 1),
        meta_data_out => meta_data_interm(meta_data_interm'low + ind),
        scz_in        => scz_interm(scz_interm'low + ind + 1),
        scz_out       => scz_interm(scz_interm'low + ind));
  end generate Prefilter_generate;

end architecture arch;




configuration Prefilter_stage_Dummy_storage of Prefilter_stage is

  for arch
    for selected_storage : Prefilter_RAM_Storage
      use entity work.Prefilter_Dummy_storage(arch);
    end for;
  end for;

end configuration Prefilter_stage_Dummy_storage;

configuration Prefilter_bundle_Dummy_storage of Prefilter_bundle is

  for arch
    for Prefilter_generate
      for bundle_elem : Prefilter_stage
        use configuration work.Prefilter_stage_Dummy_storage;
      end for;
    end for;
  end for;

end configuration Prefilter_bundle_Dummy_storage;




configuration Prefilter_stage_Direct_storage of Prefilter_stage is

  for arch
    for selected_storage : Prefilter_RAM_Storage
      use entity work.Prefilter_Direct_storage(arch);
    end for;
  end for;

end configuration Prefilter_stage_Direct_storage;

configuration Prefilter_bundle_Direct_storage of Prefilter_bundle is

  for arch
    for Prefilter_generate
      for bundle_elem : Prefilter_stage
        use configuration work.Prefilter_stage_Direct_storage;
      end for;
    end for;
  end for;

end configuration Prefilter_bundle_Direct_storage;




configuration Prefilter_stage_Barrel_shifter_storage of Prefilter_stage is

  for arch
    for selected_storage : Prefilter_RAM_Storage
      use entity work.Prefilter_Barrel_shifter_storage(arch);
    end for;
  end for;

end configuration Prefilter_stage_Barrel_shifter_storage;

configuration Prefilter_bundle_Barrel_shifter_storage of Prefilter_bundle is

  for arch
    for Prefilter_generate
      for all : Prefilter_stage
        use configuration work.Prefilter_stage_Barrel_shifter_storage;
      end for;
    end for;
  end for;

end configuration Prefilter_bundle_Barrel_shifter_storage;


