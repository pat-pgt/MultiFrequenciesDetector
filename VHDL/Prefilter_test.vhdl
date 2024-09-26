library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all;

entity Prefilter_stages_gen_pattern is
  generic (
    cycles_bits     : integer range 2 to reg_size;
    sub_cycles_bits : integer range 2 to reg_size
    );
  port (
    CLK_fall         : in  std_logic;
    sample_count     : in  integer range 0 to 2;
    cycles_count     : in  natural;
    sub_cycles_count : in  natural;
    val_out          : out reg_type
    );
end entity Prefilter_stages_gen_pattern;

architecture Patterns_1_RAM_tied_0 of Prefilter_stages_gen_pattern is
begin
  main_proc : process(CLK_fall) is
  begin
    CLK_IF : if falling_edge(CLK_fall) then
      val_out(val_out'high downto val_out'high - cycles_bits + 1) <=
        std_logic_vector(unsigned(to_unsigned(cycles_count, cycles_bits)));
      val_out(val_out'high - cycles_bits downto val_out'high - cycles_bits - sub_cycles_bits + 1) <=
        std_logic_vector(unsigned(to_unsigned(sub_cycles_count, sub_cycles_bits)));
      if sample_count < 2 then
        val_out(val_out'high - cycles_bits - sub_cycles_bits downto val_out'low) <=
          (others => '0');
      else
        val_out(val_out'high - cycles_bits - sub_cycles_bits downto val_out'low) <=
          (others => '1');
      end if;
    end if CLK_IF;
  end process main_proc;
end architecture Patterns_1_RAM_tied_0;

architecture Patterns_2_RAM_tied_0 of Prefilter_stages_gen_pattern is
begin
  main_proc : process(CLK_fall) is
  begin
    CLK_IF : if falling_edge(CLK_fall) then
      val_out(val_out'high downto val_out'high - sub_cycles_bits + 1) <=
        std_logic_vector(unsigned(to_unsigned(sub_cycles_count, sub_cycles_bits)));
      if sample_count < 2 then
        val_out(val_out'high - sub_cycles_bits downto val_out'low + cycles_bits) <=
          (others => '1');
      else
        val_out(val_out'high - sub_cycles_bits downto val_out'low + cycles_bits) <=
          (others => '0');
      end if;
      val_out(val_out'low + cycles_bits - 1 downto val_out'low) <=
        std_logic_vector(unsigned(to_unsigned(cycles_count, cycles_bits)));
    end if CLK_IF;
  end process main_proc;
end architecture Patterns_2_RAM_tied_0;

architecture arch of Prefilter_stages_gen_pattern is
begin
  assert false report "Prefilter_Stages_test should not be called without pattern" severity error;
  assert false report "use one of the configuration blocs" severity failure;
end architecture arch;

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all;

entity Prefilter_stages_check_pattern is
  port (
    CLK        : in  std_logic;
    RST        : in  std_logic;
    reg_sync   : in  std_logic;
    val_in     : in  reg_type;
    report_in  : in  bit;
    report_out : out bit
    );
end entity Prefilter_stages_check_pattern;

-- architecture Patterns_1_RAM_tied_0 of Prefilter_stages_check_pattern is

-- end architecture Patterns_1_RAM_tied_0;

-- architecture Patterns_2_RAM_tied_0 of Prefilter_stages_check_pattern is

-- end architecture Patterns_2_RAM_tied_0;

architecture arch of Prefilter_stages_check_pattern is
begin
  assert false report "Prefilter_Stages_test should not be called without checker" severity error;
  assert false report "use one of the configuration blocs" severity failure;
end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.MultiFreqDetect_package.prefilter_stages_offset_list,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Frequencies.all,
  work.Prefilter_package.all;


--! @brief Test the pre-filter stages
--!
--! The test is performed without RAM.\n
--! Signals are send to the input,
--!   The output should be the input
--!   delayed and reduced by the coefficient\n
--! Different signals are sent,
--!   including between the sine and the cosine.
--! There is a need to check there is no crosstalk
--!   between the sine and the cosine and
--!   between steps of the processing.
entity Prefilter_Stages_test is
  generic (
    debug_level : integer range 0 to 2 := 2
    );
end entity Prefilter_Stages_test;


architecture arch of Prefilter_Stages_test is
  signal CLK                        : std_logic                                := '0';
  signal reg_sync_count             : integer range 0 to 32                    := 0;
  signal sample_count               : integer range 0 to 2                     := 0;
-- Defined by the user, or should be moved into the generics
  constant cycles_bits              : integer range 2 to reg_size              := 3;
  constant cycles_max               : natural                                  := 2 ** cycles_bits;
  signal cycles_count               : natural                                  := 0;
-- Defined by the user, or should be moved into the generics
  constant sub_cycles_bits          : integer range 2 to reg_size              := 5;
  constant sub_cycles_max           : natural                                  := 2 ** sub_cycles_bits - 1;
  signal sub_cycles_count           : natural                                  := 0;
  signal reset_count                : natural                                  := 5;
  signal reset_check_count          : natural                                  := 5;
  signal reports_chain              : bit_vector(2 downto 0)                   := (others => '0');
  signal RST                        : std_logic                                := '1';
  signal RST_check                  : std_logic                                := '1';
  signal reg_sync                   : std_logic;
  signal sc_in, sc_out              : reg_sin_cos_z;
  signal meta_data_in               : meta_data_t;
  signal meta_data_out              : meta_data_t;
  constant offsets_list             : prefilter_stages_offset_list(1 downto 1) := (1      => 1.0);
  -- The package should include functions to return the latencies
  constant latencies                : natural                                  := 3;
  signal out_and_delay_s            : reg_type_list(latencies - 1 downto 0);
  signal out_and_delay_c            : reg_type_list(latencies - 1 downto 0);
  signal out_real_s                 : real;
  signal delayed_real_s             : real;
  signal out_real_c                 : real;
  signal delayed_real_c             : real;
  signal input_real_s, input_real_c : real;
  signal ratio_s_min, ratio_c_min   : real                                     := real'high;
  signal ratio_s_max, ratio_c_max   : real                                     := real'low;
  component Prefilter_stages_gen_pattern is
    generic (
      cycles_bits     : integer range 2 to reg_size;
      sub_cycles_bits : integer range 2 to reg_size
      );
    port (
      CLK_fall         : in  std_logic;
      sample_count     : in  integer range 0 to 2;
      cycles_count     : in  natural;
      sub_cycles_count : in  natural;
      val_out          : out reg_type
      );
  end component Prefilter_stages_gen_pattern;
  signal sin_gene_pattern : reg_type;
  signal cos_gene_pattern : reg_type;
begin
  assert (cycles_bits + sub_cycles_bits) < reg_size
    report "The sum of the numbers of bits of cycles (" & integer'image(cycles_bits) & ") " &
    "and sub cycles (" & integer'image(sub_cycles_bits) & ") " &
    "should be lower than the register size (" & integer'image(reg_size) & ")"
    severity failure;
  
  main_proc : process
    variable sin_cos_in    : reg_type;
    variable ratio_working : real;
  begin
    if cycles_count < cycles_max then
      CLK_IF : if CLK = '1' then
        REGSYNC_IF : if reg_sync_count < 32 then
          reg_sync       <= '0';
          reg_sync_count <= reg_sync_count + 1;
          if reg_sync_count = 0 and RST_check = '0' then
            out_real_s     <= real(to_integer(signed(sc_out.the_sin)));
            delayed_real_s <= real(to_integer(signed(out_and_delay_s(out_and_delay_s'low))));
            out_real_c     <= real(to_integer(signed(sc_out.the_cos)));
            delayed_real_c <= real(to_integer(signed(out_and_delay_c(out_and_delay_c'low))));
            if real(to_integer(signed(out_and_delay_s(out_and_delay_s'low)))) /= 0.0 then
              ratio_working := real(to_integer(signed(sc_out.the_sin))) /
                               real(to_integer(signed(out_and_delay_s(out_and_delay_s'low))));
              if ratio_working < ratio_s_min then
                ratio_s_min <= ratio_working;
              end if;
              if ratio_working > ratio_s_max then
                ratio_s_max <= ratio_working;
              end if;
            elsif real(to_integer(signed(sc_out.the_sin))) /= 0.0 then
                                        -- The other one should be null as well
                                        -- otherwise, it is an error
              ratio_s_min <= real'low;
              ratio_s_max <= real'high;
            end if;
          end if;
        else
          sample_count_if : if sample_count < 2 then
            sample_count <= sample_count + 1;
          else
            if reset_count > 0 then
              reset_count <= reset_count - 1;
            elsif reset_check_count > 0 then
              reset_check_count <= reset_check_count - 1;
              RST <= '0';
            else
              RST_check <= '0';
              if sub_cycles_count /= sub_cycles_max then
                sub_cycles_count <= sub_cycles_count + 1;
              else
                sub_cycles_count <= 0;
                cycles_count     <= cycles_count + 1;
                report integer'image(cycles_count + 1) & "/" & integer'image(cycles_max) & " done"
                  severity note;
              end if;
            end if;
            sample_count <= 0;
          end if sample_count_if;

          reg_sync_count <= 0;
          reg_sync       <= '1';
        end if REGSYNC_IF;
      end if CLK_IF;
      CLK <= not CLK;
      wait for 50 ns;
    elsif cycles_count = cycles_max then
      reports_chain(reports_chain'high) <= '1';
      cycles_count                      <= cycles_count + 1;
      wait for 50 ns;
    else
      wait;
    end if;
  end process main_proc;

  env_proc : process(CLK) is

  -- This process simulates what the environment should do:
  -- * Run the delay for the check module to know what has been supplied
  --     a certain number of cycles before.
  -- * Shift the input register as it come from a previous stage
  begin
    CLK_IF : if rising_edge(CLK) then
      if reg_sync = '1' then
        -- parallel shift of the delay 
        out_and_delay_s(out_and_delay_s'high) <= sin_gene_pattern;
        out_and_delay_s(out_and_delay_s'high - 1 downto out_and_delay_s'low) <=
          out_and_delay_s(out_and_delay_s'high downto out_and_delay_s'low + 1);
        out_and_delay_c(out_and_delay_c'high) <= cos_gene_pattern;
        out_and_delay_c(out_and_delay_c'high - 1 downto out_and_delay_c'low) <=
          out_and_delay_c(out_and_delay_c'high downto out_and_delay_c'low + 1);
        -- Fill up signal to be seen in the wave viewer
        sc_in.the_sin <= sin_gene_pattern;
        input_real_s  <= real(to_integer(signed(sin_gene_pattern)));
        sc_in.the_cos <= cos_gene_pattern;
        input_real_c  <= real(to_integer(signed(cos_gene_pattern)));
      else
        -- serial shift the registers 
        sc_in.the_sin(sc_in.the_sin'high - arithm_size downto sc_in.the_sin'low) <=
          sc_in.the_sin(sc_in.the_sin'high downto sc_in.the_sin'low + arithm_size);
        sc_in.the_cos(sc_in.the_cos'high - arithm_size downto sc_in.the_cos'low) <=
          sc_in.the_cos(sc_in.the_cos'high downto sc_in.the_cos'low + arithm_size);
      end if;
    end if CLK_IF;
  end process env_proc;

  sin_gene_pattern_instanc : Prefilter_stages_gen_pattern
    generic map (cycles_bits, sub_cycles_bits)
    port map (
      CLK_fall         => reg_sync,
      sample_count     => sample_count,
      cycles_count     => cycles_count,
      sub_cycles_count => sub_cycles_count,
      val_out          => sin_gene_pattern
      );
  cos_gene_pattern_instanc : Prefilter_stages_gen_pattern
    generic map (cycles_bits, sub_cycles_bits)
    port map (
      CLK_fall         => reg_sync,
      sample_count     => sample_count,
      cycles_count     => cycles_count,
      sub_cycles_count => sub_cycles_count,
      val_out          => cos_gene_pattern
      );

  
  DUT_instanc : Prefilter_bundle generic map (
    stages_offsets => offsets_list,
    debug_level    => debug_level)
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync,
      meta_data_in  => meta_data_in,
      meta_data_out => meta_data_out,
      scz_in        => sc_in,
      scz_out       => sc_out);

  report_proc : process
  begin
    wait until reports_chain(reports_chain'high) = '1';
    report"C'est fini" severity note;
  end process report_proc;

end architecture arch;


configuration Prefilter_Stages_test_debug2_reversed of Prefilter_Stages_test is
  for arch
    for sin_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_2_RAM_tied_0);
    end for;
    for cos_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_1_RAM_tied_0);
    end for;
  end for;
end configuration Prefilter_Stages_test_debug2_reversed;

configuration Prefilter_Stages_test_debug2 of Prefilter_Stages_test is
  for arch
    for sin_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_1_RAM_tied_0);
    end for;
    for cos_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_2_RAM_tied_0);
    end for;
  end for;
end configuration Prefilter_Stages_test_debug2;
