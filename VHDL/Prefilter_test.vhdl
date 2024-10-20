library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.InterModule_formats.all;

--! @brief Patterns generator
--!
--! This is a standard interface to generate the test patterns\n
--! A new value should be issued at each fall of the clock,
--!   which is not the master clock of the project.\n
--! The global counter is divided into
--! * a cycles count from 0 to 2**cycles_bits -1
--! * a sub_cycles count from 0 to 2**sub_cycles_bits -1
--! * a sample count from 0 to the Prefilter latency ( -1 ).\n
--! The sum of the cycles and the sub_cycles bits
--!   is guarantee to be less than the reg_size.\n
--! Many patterns are possible and are instantiated
--!   into many architectures.
--! The switch is done, using the configuration statements,
--!   in order to choose which one is tested or
--!   to run run all of them in parallel.\n
--! The checker should be the one designed for the generator in use.
entity Prefilter_stages_gen_pattern is
  generic (
    cycles_bits     : integer range 6 to reg_size;
    sub_cycles_bits : integer range 2 to reg_size;
    which_pattern   : natural
    );
  port (
    CLK_fall         : in  std_logic;
    sample_count     : in  integer range 0 to 2;
    cycles_count     : in  natural;
    sub_cycles_count : in  natural;
    val_out          : out reg_type
    );
end entity Prefilter_stages_gen_pattern;

--! @brief Basic counters stitching, mode 1
--!
--! This is intended to check the input passes through
--!   the pre-filter without RAM, with a constant shift.\n
--! The values issued are independent to each other.
--!   The counters generate as many as possible values
--!   in order to check rail to rail.\n
--! The content of the RAM (last state variable) should be tied to 0.\n
--! In the mode 0, the bits are padded at the bottom by 0's or 1's,
--!   according with the sample_count.
--! In the mode 1, the bits are padded at the middle by 0's or 1's,
--!   according with the sample_count.\n
--! All the tests here test the full range of the 2'nd complement
--!   regardless all the values are reachable or not (see the input Cordic handling).
architecture Patterns_RAM_tied_0 of Prefilter_stages_gen_pattern is
begin
  main_proc : process(CLK_fall) is
  begin
    CLK_IF : if falling_edge(CLK_fall) then
      PATTERN_CASE : case which_pattern is
        when 1 =>
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
        when 2 =>
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

        when others =>
          assert false report "The pattern " & integer'image(which_pattern) & " is not known" severity failure;
      end case PATTERN_CASE;
    end if CLK_IF;
  end process main_proc;
end architecture Patterns_RAM_tied_0;

--! @brief
--!
--! This mode is intended to check the filter with a light RAM
--!   of a couple of state variables.\n
--! The values issued are 3 step signals, in the ratio of 1:4:16?\n
--! The content of the RAM (last state variable) should NOT be tied to 0.\n
--! This test is intended to test for reachable values (see the input Cordic handling).
--! Due to the cumulative product of the cosine(h),
--!   the input is supplied by numbers contained between 1010xxx and 0101xxx.\n
--! Another test check the fall back outside these values.
architecture Patterns_1_RAM_light of Prefilter_stages_gen_pattern is

begin
  main_proc : process(CLK_fall) is
    variable cycle_count_std_logic : std_logic_vector(cycles_bits downto 1);
  begin
    CLK_IF : if falling_edge(CLK_fall) then
      cycle_count_std_logic := std_logic_vector(to_unsigned(cycles_count, cycle_count_std_logic'length));
      SAMPLE_COUNT_CASE : case sample_count is
        when 0 =>
          val_out(val_out'high downto val_out'high - 2) <=
            ( others => not cycle_count_std_logic( cycle_count_std_logic'high ));
          val_out(val_out'high - 3 downto val_out'high - 4) <=
            cycle_count_std_logic(cycle_count_std_logic'high downto cycle_count_std_logic'high - 1);
          val_out(val_out'high - 5 downto val_out'low) <=
            (others => not cycle_count_std_logic(cycle_count_std_logic'high));
        when 1 =>
          val_out(val_out'high downto val_out'high - 1) <=
            ( others => not cycle_count_std_logic( cycle_count_std_logic'high ));
          val_out(val_out'high - 2 downto val_out'high - 3) <=
            cycle_count_std_logic(cycle_count_std_logic'high - 2 downto cycle_count_std_logic'high - 3);
          val_out(val_out'high - 4 downto val_out'low) <=
            (others => not cycle_count_std_logic(cycle_count_std_logic'high));
        when 2 =>
          val_out(val_out'high ) <= not cycle_count_std_logic( cycle_count_std_logic'high );
          val_out(val_out'high - 1 downto val_out'high - 2) <=
            cycle_count_std_logic( cycle_count_std_logic'high - 4 downto cycle_count_std_logic'high - 5 );
          val_out(val_out'high - 3 downto val_out'high - 4) <=
            cycle_count_std_logic(cycle_count_std_logic'high - 4 downto cycle_count_std_logic'high - 5);
          -- Fill up with the not high bit in order to test
          --   as more rail to rail as possible
          val_out(val_out'high - 5 downto val_out'low) <=
            (others => not cycle_count_std_logic(cycle_count_std_logic'high));
      end case SAMPLE_COUNT_CASE;
    end if CLK_IF;
  end process main_proc;
end architecture Patterns_1_RAM_light;

--! @brief Default architecture
--!
--! Assume, all the VHDL compilers take the last one
--!  when nothing is specified.\n
--! This can occur if no configuration statement is defined
--!   or if the top level is the test entity, rather than the configure.\n
--! This should ALWAYS be the last one.
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

--! @brief Patterns checker
--!
--! This is a standard interface to check the test patterns\n
--! The latency is handled by the "host" test entity,
--!   as the pattern generator is delayed by the latency of the pre-filter.\n 
--! A new value should be issued at each fall of the clock,
--!   which is not the master clock of the project.\n 
--! The global counter is divided into
--! * a cycles count from 0 to 2**cycles_bits -1
--! * a sub_cycles count from 0 to 2**sub_cycles_bits -1
--! * a sample count from 0 to the Prefilter latency ( -1 ).\n
--! The sum of the cycles and the sub_cycles bits
--!   is guarantee to be less than the reg_size.\n
--! Many checks are possible and are instantiated
--!   into many architectures.
--! The switch is done, using the configuration statements,
--!   in order to choose which one is tested or
--!   to run run all of them in parallel.\n
--! The pattern generator should be the one designed for the checker in use.\n
--! This entity does not have outputs, as they store the results
--!   and their statistics.
--! For the multiple test configuration, to display them properly,
--!   at the end of the simulation, a report chain triggers them
--!   one after the other.
entity Prefilter_stages_check_pattern is
  generic (
    cycles_bits     : integer range 6 to reg_size;
    sub_cycles_bits : integer range 2 to reg_size;
    debug_text      : string
    );
  port (
    CLK_fall         : in  std_logic;
    RST              : in  std_logic;
    reg_sync         : in  std_logic;
    sample_count     : in  integer range 0 to 2;
    cycles_count     : in  natural;
    sub_cycles_count : in  natural;
    val_in           : in  reg_type;
    prefiltered_in   : in  reg_type;
    -- metadata    : in  metadata;
    report_in        : in  bit;
    report_out       : out bit
    );
end entity Prefilter_stages_check_pattern;

--! @brief Basic counters stitching, mode 1 and 2
--!
--! This is intended to check the input passes through
--!   the pre-filter without RAM, with a constant shift.\n
--! The content of the RAM (last state variable) should be tied to 0.\n
--! The test check the ratio between the prefiltered and the input
--!   is the expecrted one.\n
--! Due to the limited precision, the ratio may be wrong
--!   especially for low values.
--! The criteria is to check if the shifted is equal to what is expected,
--!   1 lower than, 1 greter than, or other.
--! The last case is a unique counter as it should not happened
--!   for any values.
--! TODO: check the constant is the one that match the meta-data.
architecture Patterns_1_2_RAM_tied_0 of Prefilter_stages_check_pattern is
  signal input_int_in                 : integer;
  signal input_int_pre                : integer;
  signal delta_0, delta_p1, delta_m1  : natural := 0;
  signal delta_other_p, delta_other_m : natural := 0;
begin
  main_proc : process(CLK_fall) is
    variable shifts          : natural;
    variable input_int_pre_v : integer;
    variable input_int_in_v  : integer;
  begin
    CLK_IF : if falling_edge(CLK_fall) then
      RST_IF : if RST = '0' then
        -- This is a tremporary solution
        shifts          := 4;
        input_int_in_v  := to_integer(signed(val_in));
        input_int_pre_v := to_integer(signed(prefiltered_in));
        input_int_in_v  := input_int_in_v / 2 ** shifts;
        case input_int_in_v - input_int_pre_v is
          when 0  => delta_0  <= delta_0 + 1;
          when 1  => delta_p1 <= delta_p1 + 1;
          when -1 => delta_m1 <= delta_m1 + 1;
          when others =>
            if input_int_in_v > input_int_pre_v then
              delta_other_p <= delta_other_p + 1;
            else
              delta_other_m <= delta_other_m + 1;
            end if;
        end case;
        input_int_pre <= input_int_pre_v;
        input_int_in  <= input_int_in_v;
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

  report_proc : process
  begin
    wait until report_in = '1';
    report "RAM tied 0: " & debug_text
      severity note;
    report " = " & integer'image(delta_0) & ",  " &
      " >1 " & integer'image(delta_p1) & ",  " &
      " <1 " & integer'image(delta_m1) & ",  " &
      " > " & integer'image(delta_other_p) & ",  " &
      " < " & integer'image(delta_other_m)
      severity note;
    report_out <= '1';
  end process report_proc;

end architecture Patterns_1_2_RAM_tied_0;

architecture Paterns1_RAM_light of Prefilter_stages_check_pattern is
  signal delayed_in_1, out_1 : integer;
  signal delayed_in_2, out_2 : integer;
  signal delayed_in_3, out_3 : integer;
begin

  main_proc : process(CLK_fall) is
    variable shifts          : natural;
    variable input_int_pre_v : integer;
    variable input_int_in_v  : integer;
  begin
    CLK_IF : if falling_edge(CLK_fall) then
      RST_IF : if RST = '0' then
        -- This is a tremporary solution
        shifts          := 4;
        input_int_in_v  := to_integer(signed(val_in));
        input_int_pre_v := to_integer(signed(prefiltered_in));
        case sample_count is
          when 0 =>
            delayed_in_1 <= input_int_in_v;
            out_1        <= input_int_pre_v;
          when 1 =>
            delayed_in_2 <= input_int_in_v;
            out_2        <= input_int_pre_v;
          when 2 =>
            delayed_in_3 <= input_int_in_v;
            out_3        <= input_int_pre_v;
        end case;
      end if RST_IF;
    end if CLK_IF;
  end process main_proc;

  report_proc : process
  begin
    wait until report_in = '1';
    report "RAM light FIFO: " & debug_text
      severity note;
    report_out <= '1';
  end process report_proc;

end architecture Paterns1_RAM_light;
--! @brief Default architecture
--!
--! Assume, all the VHDL compilers take the last one
--!  when nothing is specified.\n
--! This can occur if no configuration statement is defined
--!   or if the top level is the test entity, rather than the configure.\n
--! This should ALWAYS be the last one.
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
end entity Prefilter_Stages_test;


architecture arch of Prefilter_Stages_test is
  signal CLK                        : std_logic                                := '0';
  signal reg_sync_count             : integer range 0 to 32                    := 0;
  signal sample_count               : integer range 0 to 2                     := 0;
-- Defined by the user, or should be moved into the generics
  constant cycles_bits              : integer range 6 to reg_size              := 7;
  constant cycles_max               : natural                                  := 2 ** cycles_bits;
  signal cycles_count               : natural                                  := 0;
-- Defined by the user, or should be moved into the generics
  constant sub_cycles_bits          : integer range 2 to reg_size              := 4;
  constant sub_cycles_max           : natural                                  := 2 ** sub_cycles_bits - 1;
  signal sub_cycles_count           : natural                                  := 0;
  signal reset_count                : natural                                  := 5;
  signal reset_check_count          : natural                                  := 6;
  signal reports_chain              : bit_vector(3 downto 0)                   := (others => '0');
  signal RST                        : std_logic                                := '1';
  signal RST_check                  : std_logic                                := '1';
  signal reg_sync                   : std_logic;
  signal sc_in, sc_out              : reg_sin_cos_z;
  signal meta_data_in               : meta_data_t;
  signal meta_data_out              : meta_data_t;
  constant offsets_list             : prefilter_stages_offset_list(1 downto 1) := (1      => 1.0);
  -- The package should include functions to return the latencies
  constant latencies                : natural                                  := 3;
  signal out_and_delay_s            : reg_type_list(latencies downto 0);
  signal out_and_delay_c            : reg_type_list(latencies downto 0);
  signal out_real_s                 : real;
  signal delayed_real_s             : real;
  signal out_real_c                 : real;
  signal delayed_real_c             : real;
  signal input_real_s, input_real_c : real;
  -- signal ratio_s_min, ratio_c_min   : real                                     := real'high;
  -- signal ratio_s_max, ratio_c_max   : real                                     := real'low;
  component Prefilter_stages_gen_pattern is
    generic (
      cycles_bits     : integer range 6 to reg_size;
      sub_cycles_bits : integer range 2 to reg_size;
      which_pattern   : natural
      );
    port (
      CLK_fall         : in  std_logic;
      sample_count     : in  integer range 0 to 2;
      cycles_count     : in  natural;
      sub_cycles_count : in  natural;
      val_out          : out reg_type
      );
  end component Prefilter_stages_gen_pattern;
  component Prefilter_stages_check_pattern is
    generic (
      cycles_bits     : integer range 6 to reg_size;
      sub_cycles_bits : integer range 2 to reg_size;
      debug_text      : string
      );
    port (
      CLK_fall         : in  std_logic;
      RST              : in  std_logic;
      reg_sync         : in  std_logic;
      sample_count     : in  integer range 0 to 2;
      cycles_count     : in  natural;
      sub_cycles_count : in  natural;
      val_in           : in  reg_type;
      prefiltered_in   : in  reg_type;
      -- metadata    : in  metadata;
      report_in        : in  bit;
      report_out       : out bit
      );
  end component Prefilter_stages_check_pattern;
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
            -- This is to find the bugs, it is going to be deleted when done
            out_real_s     <= real(to_integer(signed(sc_out.the_sin)));
            delayed_real_s <= real(to_integer(signed(out_and_delay_s(out_and_delay_s'low))));
            out_real_c     <= real(to_integer(signed(sc_out.the_cos)));
            delayed_real_c <= real(to_integer(signed(out_and_delay_c(out_and_delay_c'low))));
          end if;
        else
          sample_count_if : if sample_count < 2 then
            sample_count <= sample_count + 1;
          else
            if reset_count > 0 then
              reset_count <= reset_count - 1;
            elsif reset_check_count > 0 then
              reset_check_count <= reset_check_count - 1;
              RST               <= '0';
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
    generic map (cycles_bits, sub_cycles_bits, which_pattern => 0)
    port map (
      CLK_fall         => reg_sync,
      sample_count     => sample_count,
      cycles_count     => cycles_count,
      sub_cycles_count => sub_cycles_count,
      val_out          => sin_gene_pattern
      );

  cos_gene_pattern_instanc : Prefilter_stages_gen_pattern
    generic map (cycles_bits, sub_cycles_bits, which_pattern => 0)
    port map (
      CLK_fall         => reg_sync,
      sample_count     => sample_count,
      cycles_count     => cycles_count,
      sub_cycles_count => sub_cycles_count,
      val_out          => cos_gene_pattern
      );

  sin_check_pattern_instanc : Prefilter_stages_check_pattern
    generic map (cycles_bits, sub_cycles_bits,
                 debug_text => "SIN")
    port map (
      CLK_fall         => reg_sync,
      RST              => RST_check,
      reg_sync         => reg_sync,
      sample_count     => sample_count,
      cycles_count     => cycles_count,
      sub_cycles_count => sub_cycles_count,
      prefiltered_in   => sc_out.the_sin,
      val_in           => out_and_delay_s(out_and_delay_s'low),
      report_in        => reports_chain(reports_chain'high - 1),
      report_out       => reports_chain(reports_chain'high - 2)
      );

  cos_check_pattern_instanc : Prefilter_stages_check_pattern
    generic map (cycles_bits, sub_cycles_bits,
                 debug_text => "COS")
    port map (
      CLK_fall         => reg_sync,
      RST              => RST_check,
      reg_sync         => reg_sync,
      sample_count     => sample_count,
      cycles_count     => cycles_count,
      sub_cycles_count => sub_cycles_count,
      prefiltered_in   => sc_out.the_cos,
      val_in           => out_and_delay_c(out_and_delay_c'low),
      report_in        => reports_chain(reports_chain'high - 2),
      report_out       => reports_chain(reports_chain'high - 3)
      );

  
  DUT_instanc : Prefilter_bundle
    generic map (
      stages_offsets => offsets_list)
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
    report"C'est fini, TODO" severity note;
    reports_chain(reports_chain'high - 1) <= '1';
  end process report_proc;

end architecture arch;


configuration Prefilter_Stages_test_debug1 of Prefilter_Stages_test is
  for arch
    for all : Prefilter_bundle
      use entity work.Prefilter_bundle(arch)
        generic map (debug_level    => 2,
                     stages_offsets => offsets_list);
    end for;
    for sin_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_RAM_tied_0)
      generic map (
        cycles_bits => cycles_bits, sub_cycles_bits => sub_cycles_bits, which_pattern => 1);        
    end for;
    for cos_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_RAM_tied_0)
      generic map (
        cycles_bits => cycles_bits, sub_cycles_bits => sub_cycles_bits, which_pattern => 2);
    end for;
    for all : Prefilter_stages_check_pattern
      use entity work.Prefilter_stages_check_pattern(Patterns_1_2_RAM_tied_0);
    end for;
  end for;
end configuration Prefilter_Stages_test_debug1;

configuration Prefilter_Stages_test_debug2 of Prefilter_Stages_test is
  for arch
    for all : Prefilter_bundle
      use entity work.Prefilter_bundle(arch)
        generic map (debug_level    => 2,
                     stages_offsets => offsets_list);
    end for;
    for sin_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_RAM_tied_0)
      generic map (
        cycles_bits => cycles_bits, sub_cycles_bits => sub_cycles_bits, which_pattern => 2);
    end for;
    for cos_gene_pattern_instanc : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_RAM_tied_0)
      generic map (
        cycles_bits => cycles_bits, sub_cycles_bits => sub_cycles_bits, which_pattern => 1);
    end for;
    for all : Prefilter_stages_check_pattern
      use entity work.Prefilter_stages_check_pattern(Patterns_1_2_RAM_tied_0);
    end for;
  end for;
end configuration Prefilter_Stages_test_debug2;

configuration Prefilter_Stages_test_debug3 of Prefilter_Stages_test is
  for arch
    for all : Prefilter_bundle
      use entity work.Prefilter_bundle(arch)
        generic map (debug_level    => 1,
                     stages_offsets => offsets_list);
    end for;
    for all : Prefilter_stages_gen_pattern
      use entity work.Prefilter_stages_gen_pattern(Patterns_1_RAM_light);
    end for;
    for all : Prefilter_stages_check_pattern
      use entity work.Prefilter_stages_check_pattern(Paterns1_RAM_light);
    end for;
  end for;
end configuration Prefilter_Stages_test_debug3;
