library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;


entity Cordic_Bundle_Z_to_0_partialSynth is

  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    full_sync     : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    X_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Z_expon_out   : out std_logic_vector(5 downto 0);
    report_in     : in  std_logic;
    report_out    : out std_logic);

end entity Cordic_Bundle_Z_to_0_partialSynth;

architecture arch of Cordic_Bundle_Z_to_0_partialSynth is
    constant debug_mode          : boolean               := false;
    constant stages_nbre         : integer range 2 to 25 := 20;
    signal metadata_catch_list : meta_data_list_t( 0 downto 1 );
    signal stages_catch_list   : cordic_stages_num_list( 0 downto 1 );

component Cordic_Bundle_Z_to_0 is
  generic (
    debug_mode          : boolean               := false;
    stages_nbre         : integer range 2 to 25 := 20;
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
    X_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Z_expon_out   : out std_logic_vector(5 downto 0);
    report_in     : in  std_logic;
    report_out    : out std_logic);
end component Cordic_Bundle_Z_to_0;

begin

main_instanc : Cordic_Bundle_Z_to_0 generic map(
    debug_mode    => debug_mode,
    stages_nbre   => stages_nbre,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list
    )
  port map(
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    full_sync     => full_sync,
    meta_data_in  => meta_data_in,
    meta_data_out => meta_data_out,
    scz_in        => scz_in,
    X_out         => X_out,
    Y_out         => Y_out,
    Z_expon_out   => Z_expon_out,
    report_in     => report_in,
    report_out    => report_out);

  
end architecture  arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;


entity Cordic_Bundle_Y_to_0_partialSynth is

  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    full_sync     : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    scz_in        : in  reg_sin_cos_z;
    X_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Z_expon_out   : out std_logic_vector(5 downto 0);
    report_in     : in  std_logic;
    report_out    : out std_logic);

end entity Cordic_Bundle_Y_to_0_partialSynth;

architecture arch of Cordic_Bundle_Y_to_0_partialSynth is
    constant debug_mode          : boolean               := false;
    constant stages_nbre         : integer range 2 to 25 := 20;
    signal metadata_catch_list : meta_data_list_t( 0 downto 1 );
    signal stages_catch_list   : cordic_stages_num_list( 0 downto 1 );

component Cordic_Bundle_Y_to_0 is
  generic (
    debug_mode          : boolean               := false;
    stages_nbre         : integer range 2 to 25 := 20;
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
    X_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Y_out         : out std_logic_vector(arithm_size - 1 downto 0);
    Z_expon_out   : out std_logic_vector(5 downto 0);
    report_in     : in  std_logic;
    report_out    : out std_logic);
end component Cordic_Bundle_Y_to_0;

begin

main_instanc : Cordic_Bundle_Y_to_0 generic map(
    debug_mode    => debug_mode,
    stages_nbre   => stages_nbre,
    metadata_catch_list => metadata_catch_list,
    stages_catch_list   => stages_catch_list
    )
  port map(
    CLK           => CLK,
    RST           => RST,
    reg_sync      => reg_sync,
    full_sync     => full_sync,
    meta_data_in  => meta_data_in,
    meta_data_out => meta_data_out,
    scz_in        => scz_in,
    X_out         => X_out,
    Y_out         => Y_out,
    Z_expon_out   => Z_expon_out,
    report_in     => report_in,
    report_out    => report_out);

  
end architecture  arch;
