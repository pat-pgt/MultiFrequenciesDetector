library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;


entity Cordic_Bundle_Z_to_0 is
  generic (
    debug_mode          : boolean               := false;
    stages_nbre         : integer range 2 to reg_size - 4 := 20;
    metadata_catch_list : meta_data_list_t;
    stages_catch_list   : cordic_stages_num_list
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    full_sync     : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z;
    X_out         : out reg_type;
    Y_out         : out reg_type;
    Z_out         : out reg_type;
    Z_expon_out   : out std_logic_vector(5 downto 0);
    report_in     : in  std_logic;
    report_out    : out std_logic);
end entity Cordic_Bundle_Z_to_0;


architecture rtl of Cordic_Bundle_Z_to_0 is
  type scz_array_t is array (0 to stages_nbre) of reg_sin_cos_z;
  signal scz_array                : scz_array_t;
  type meta_data_array_t is array(0 to stages_nbre) of meta_data_t;
  signal meta_data_array          : meta_data_array_t;
  signal debug_catch_in_operation : std_logic;
  signal report_catch_chain       : std_logic_vector(stages_catch_list'length downto 0);

begin
  has_catch_debugs : if stages_catch_list'length > 0 and metadata_catch_list'length > 0 generate
    Catch_monitor_stages : for ind in 1 to stages_catch_list'length generate
      debug_catch_in_operation <= '1';
      Interm_monitor_instanc : Cordic_Interm_monitor generic map(
        Z_not_Y_to_0        => true,
        stage_num           => stages_catch_list(stages_catch_list'low + ind - 1),
        metadata_catch_list => metadata_catch_list)
        port map(
          CLK             => CLK,
          RST             => RST,
          reg_sync        => reg_sync,
          full_sync       => full_sync,
          report_in       => report_catch_chain(report_catch_chain'low + ind - 1),
          report_out      => report_catch_chain(report_catch_chain'low + ind),
          meta_data_after => meta_data_array(ind),
          scz_before      => scz_array(ind - 1),
          scz_after       => scz_array(ind)
          );
    end generate Catch_monitor_stages;
  end generate has_catch_debugs;
  report_catch_chain(report_catch_chain'low) <= report_in;
  report_out                                 <= report_catch_chain(report_catch_chain'high);
  
  scz_array(0) <= scz_in;
  scz_out      <= scz_array(stages_nbre);

  meta_data_array(0) <= meta_data_in;
  meta_data_out      <= meta_data_array(stages_nbre);

  gene_interm : for ind in 1 to stages_nbre generate
    interm_stage_instanc : Cordic_IntermStage generic map
      (
        Z_not_Y_to_0 => true,
        shifts_calc  => ind
        )
      port map
      (
        CLK           => CLK,
        RST           => RST,
        reg_sync      => reg_sync,
        meta_data_in  => meta_data_array(ind - 1),
        meta_data_out => meta_data_array(ind),
        scz_in        => scz_array(ind - 1),
        scz_out       => scz_array(ind));
  end generate gene_interm;

  last_stage_instanc : Cordic_LastStage_4_test
    generic map (
      Z_not_Y_2_0    => true)
    port map (
      CLK            => CLK,
      RST            => RST,
      reg_sync       => reg_sync,
      scz_in         => scz_array(stages_nbre),
      X_out          => X_out,
      Y_out          => Y_out,
      Z_out          => Z_out,
      error_exponent => Z_expon_out);

end architecture rtl;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;


entity Cordic_Bundle_Y_to_0 is
  generic (
    debug_mode          : boolean               := false;
    stages_nbre         : integer range 2 to reg_size - 4 := 20;
    metadata_catch_list : meta_data_list_t;
    stages_catch_list   : cordic_stages_num_list
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    full_sync     : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    X_out         : out reg_type;
    Y_out         : out reg_type;
    Z_out         : out reg_type;
    Y_expon_out   : out std_logic_vector(5 downto 0);
    report_in     : in  std_logic;
    report_out    : out std_logic);
end entity Cordic_Bundle_Y_to_0;


architecture rtl of Cordic_Bundle_Y_to_0 is
  type scz_array_t is array (0 to stages_nbre) of reg_sin_cos_z;
  signal scz_array                : scz_array_t;
  type meta_data_array_t is array(0 to stages_nbre) of meta_data_t;
  signal meta_data_array          : meta_data_array_t;
  signal debug_catch_in_operation : std_logic;
  signal report_catch_chain       : std_logic_vector(stages_catch_list'length downto 0);

begin
  has_catch_debugs : if stages_catch_list'length > 0 and metadata_catch_list'length > 0 generate
    Catch_monitor_stages : for ind in 1 to stages_catch_list'length generate
      debug_catch_in_operation <= '1';
      Interm_monitor_instanc : Cordic_Interm_monitor generic map(
        Z_not_Y_to_0        => false,
        stage_num           => stages_catch_list(stages_catch_list'low + ind - 1),
        metadata_catch_list => metadata_catch_list)
        port map(
          CLK             => CLK,
          RST             => RST,
          reg_sync        => reg_sync,
          full_sync       => full_sync,
          report_in       => report_catch_chain(report_catch_chain'low + ind - 1),
          report_out      => report_catch_chain(report_catch_chain'low + ind),
          meta_data_after => meta_data_array(ind),
          scz_before      => scz_array(ind - 1),
          scz_after       => scz_array(ind)
          );
    end generate Catch_monitor_stages;

  end generate has_catch_debugs;
  report_catch_chain(report_catch_chain'low) <= report_in;
  report_out                                 <= report_catch_chain(report_catch_chain'high);
    
  scz_array(0) <= scz_in;

  meta_data_array(0) <= meta_data_in;
  meta_data_out      <= meta_data_array(stages_nbre);

  gene_interm : for ind in 1 to stages_nbre generate
    interm_stage_instanc : Cordic_IntermStage generic map
      (
        Z_not_Y_to_0 => false,
        shifts_calc  => ind
        )
      port map
      (
        CLK           => CLK,
        RST           => RST,
        reg_sync      => reg_sync,
        meta_data_in  => meta_data_array(ind - 1),
        meta_data_out => meta_data_array(ind),
        scz_in        => scz_array(ind - 1),
        scz_out       => scz_array(ind));
  end generate gene_interm;

  last_stage_instanc : Cordic_LastStage_4_test
    generic map (
      Z_not_Y_2_0    => false)
    port map (
      CLK            => CLK,
      RST            => RST,
      reg_sync       => reg_sync,
      scz_in         => scz_array(stages_nbre),
      X_out          => X_out,
      Y_out          => Y_out,
      Z_out          => Z_out,
      error_exponent => Y_expon_out);

  
end architecture rtl;
