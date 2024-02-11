library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.MultiFreqDetect_package.all,
  work.Cordic_package.all;


entity Cordic_Bundle_Z_to_0 is
  generic (
    debug_mode  : boolean               := false;
    stages_nbre : integer range 2 to 25 := 20
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    scz_out       : out reg_sin_cos_z;
    X_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Z_expon_out   : out std_logic_vector(5 downto 0));
end entity Cordic_Bundle_Z_to_0;


architecture rtl of Cordic_Bundle_Z_to_0 is
  type scz_array_t is array (0 to stages_nbre) of reg_sin_cos_z;
  signal scz_array       : scz_array_t;
  type meta_data_array_t is array(0 to stages_nbre) of meta_data_t;
  signal meta_data_array : meta_data_array_t;
begin
  scz_array(0) <= scz_in;
  scz_out <= scz_array(stages_nbre);
  X_out <= scz_array(stages_nbre).the_cos(scz_array(stages_nbre).the_cos'low + arithm_size - 1 downto
                                          scz_array(stages_nbre).the_cos'low);
  Y_out <= scz_array(stages_nbre).the_sin(scz_array(stages_nbre).the_sin'low + arithm_size - 1 downto
                                          scz_array(stages_nbre).the_sin'low);

  meta_data_array(0) <= meta_data_in;
  meta_data_out      <= meta_data_array(stages_nbre);

  gene_interm : for ind in 1 to stages_nbre generate
    interm_stage_instanc : Cordic_IntermStage generic map
      (
        debug_mode   => debug_mode,
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

  last_stage_instanc : Cordic_LastStage_Z_to_0
    port map (
      CLK              => CLK,
      RST              => RST,
      reg_sync         => reg_sync,
      scz_in           => scz_array(stages_nbre),
      Z_error_exponent => Z_expon_out);

end architecture rtl;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.MultiFreqDetect_package.all,
  work.Cordic_package.all;


entity Cordic_Bundle_Y_to_0 is
  generic (
    debug_mode  : boolean               := false;
    stages_nbre : integer range 2 to 25 := 20
    );
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    X_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Z_expon_out   : out std_logic_vector(5 downto 0));
end entity Cordic_Bundle_Y_to_0;


architecture rtl of Cordic_Bundle_Y_to_0 is
  type scz_array_t is array (0 to stages_nbre) of reg_sin_cos_z;
  signal scz_array       : scz_array_t;
  type meta_data_array_t is array(0 to stages_nbre) of meta_data_t;
  signal meta_data_array : meta_data_array_t;
begin
  scz_array(0) <= scz_in;
  X_out <= scz_array(stages_nbre).the_cos(scz_array(stages_nbre).the_cos'low + arithm_size - 1 downto
                                          scz_array(stages_nbre).the_cos'low);
  Y_out <= scz_array(stages_nbre).the_sin(scz_array(stages_nbre).the_sin'low + arithm_size - 1 downto
                                          scz_array(stages_nbre).the_sin'low);

  meta_data_array(0) <= meta_data_in;
  meta_data_out      <= meta_data_array(stages_nbre);

  gene_interm : for ind in 1 to stages_nbre generate
    interm_stage_instanc : Cordic_IntermStage generic map
      (
        debug_mode   => debug_mode,
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

  
end architecture rtl;
