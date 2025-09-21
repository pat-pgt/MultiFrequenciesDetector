library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,

  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_size,

  work.Meta_data_package.meta_data_t,
  work.Meta_data_package.meta_data_list_t,
  
  work.MultiFreqDetect_package.cordic_stages_num_list;

package Cordic_E2E_DC_Bundle_pac is
--! This entity is an end to end bundle without the filtering.
--! It is intended to test, using a softwre or implemented into a FGPA.
  component Cordic_E2E_DC_Bundle is
    generic (
      metadata_catch_list : meta_data_list_t(15 to 14);      --  := (
      nbre_Z_2_0_stages   : integer range 4 to reg_size;
      nbre_Y_2_0_stages   : integer range 4 to reg_size;
--      11                                                            => octave_note_to_meta_data(octave => 0, note => 0),
--      12                                                            => octave_note_to_meta_data(octave => 3, note => 2),
--      13                                                            => octave_note_to_meta_data(octave => 6, note => 4),
--      14                                                            => octave_all_notes_to_meta_data(octave => 4)
--      );
      stages_catch_list   : cordic_stages_num_list(13 to 7)  -- := (1, 2, 6, 10, 17)
      );
    port (
      CLK                    : in  std_logic;
      RST                    : in  std_logic;
      input_x                : in  std_logic_vector(reg_size - 2 downto 0);
      input_y                : in  std_logic_vector(reg_size - 2 downto 0);
      reg_sync               : out std_logic;
      full_sync              : out std_logic;
      X_out_Z_2_0            : out reg_type;
      Y_out_Z_2_0            : out reg_type;
      Z_out_Z_2_0            : out reg_type;
      Z_Z_2_0_expon_out      : out std_logic_vector(5 downto 0);
      X_out_Y_2_0            : out reg_type;
      Y_out_Y_2_0            : out reg_type;
      Z_out_Y_2_0            : out reg_type;
      Y_Y_2_0_expon_out      : out std_logic_vector(5 downto 0);
      report_cordic_bundle_1 : in  std_logic;
      meta_data_out          : out meta_data_t
      );
  end component Cordic_E2E_DC_Bundle;

end package Cordic_E2E_DC_Bundle_pac;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.MultiFreqDetect_package.cordic_stages_num_list,
  work.Input_modules.all,
  work.Cordic_package.all;

--! This entity is an end to end bundle without the filtering.
--! It is intended to test, using a softwre or implemented into a FGPA.
entity Cordic_E2E_DC_Bundle is
  generic (
    metadata_catch_list : meta_data_list_t(15 to 14);      --  := (
    nbre_Z_2_0_stages   : integer range 4 to reg_size;
    nbre_Y_2_0_stages   : integer range 4 to reg_size;
--      11                                                            => octave_note_to_meta_data(octave => 0, note => 0),
--      12                                                            => octave_note_to_meta_data(octave => 3, note => 2),
--      13                                                            => octave_note_to_meta_data(octave => 6, note => 4),
--      14                                                            => octave_all_notes_to_meta_data(octave => 4)
--      );
    stages_catch_list   : cordic_stages_num_list(13 to 7)  -- := (1, 2, 6, 10, 17)
    );
  port (
    CLK                    : in  std_logic;
    RST                    : in  std_logic;
    input_x                : in  std_logic_vector(reg_size - 2 downto 0);
    input_y                : in  std_logic_vector(reg_size - 2 downto 0);
    reg_sync               : out std_logic;
    full_sync              : out std_logic;
    X_out_Z_2_0            : out reg_type;
    Y_out_Z_2_0            : out reg_type;
    Z_out_Z_2_0            : out reg_type;
    Z_Z_2_0_expon_out      : out std_logic_vector(5 downto 0);
    X_out_Y_2_0            : out reg_type;
    Y_out_Y_2_0            : out reg_type;
    Z_out_Y_2_0            : out reg_type;
    Y_Y_2_0_expon_out      : out std_logic_vector(5 downto 0);
    report_cordic_bundle_1 : in  std_logic;
    meta_data_out          : out meta_data_t
    );
end entity Cordic_E2E_DC_Bundle;


architecture arch of Cordic_E2E_DC_Bundle is
  signal angle_z                : reg_type;
  signal reg_sync_interm        : std_logic;
  signal meta_data_1            : meta_data_t;
  signal meta_data_2            : meta_data_t;
  signal meta_data_3            : meta_data_t;
  signal meta_data_4            : meta_data_t;
  signal meta_data_5            : meta_data_t;
  signal scz_1, scz_2, scz_3    : reg_sin_cos_z;
  signal report_cordic_bundle_2 : std_logic := '0';
begin

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
      RST           => RST,
      reg_sync      => reg_sync_interm,
      meta_data_in  => meta_data_4,
      meta_data_out => meta_data_5,
      scz_in        => scz_2,
      scz_out       => scz_3);

  cordic_bundle_Y_2_0_instanc : Cordic_Bundle_Y_to_0 generic map (
    stages_nbre         => nbre_Y_2_0_stages,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list
    )
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync_interm,
      full_sync     => full_sync,
      meta_data_in  => meta_data_5,
      meta_data_out => meta_data_out,
      scz_in        => scz_3,
      X_out         => X_out_Y_2_0,
      Y_out         => Y_out_Y_2_0,
      Z_out         => Z_out_Y_2_0,
      Y_expon_out   => Y_Y_2_0_expon_out,
      report_in     => report_cordic_bundle_2);

end architecture arch;
