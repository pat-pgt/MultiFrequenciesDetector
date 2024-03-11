library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.MultiFreqDetect_package.all,
  work.Prefilter_package.all;


entity Prefilter_Storage_Test is
  generic (
    --! The debug note shorters the simulation while discarding many values
    debug_mode : boolean := false);
end entity Prefilter_Storage_Test;

architecture arch of Prefilter_Storage_Test is
  signal CLK                     : std_logic                                 := '0';
  signal reg_sync, reg_sync_last : std_logic;
  signal arithm_cycle            : integer range 0 to reg_size / arithm_size := 0;
  constant arithm_cycle_max      : integer range 0 to reg_size / arithm_size := reg_size / arithm_size;
  constant ram_locations_size    : positive                                  := 18;
  --! Counter to keep everything in reset for the first reg_sync cycles
  signal RST_count               : natural                                   := 2;
  --! Disable the compare for the first complete ram_locations cycle
  --! Since the storage returns the values after N reg_sync cycles,
  --! The begining of the simulation would have been always false
  --! as it contains 'U' or the initialisation of the RAM
  signal RST_count_compare       : natural                                   := ram_locations_size + 4;
  signal RST                     : std_logic                                 := '1';
  type ram_t is array(0 to 2**(ram_locations_size - 3) - 1) of std_logic_vector(reg_size - 1 downto 0);
  --! The RAM to store in parallel the sine values by the most basic way
  --! It is used to compare with the result of the storage component.
  signal ram_test_sin            : ram_t;
  --! The RAM to store in parallel the cosine values by the most basic way
  --! It is used to compare with the result of the storage component.
  signal ram_test_cos            : ram_t;
  --! Counter of the sine and cosine memories which store in parallel
  signal compare_count           : natural;
  -- The test patern is a base value replicated (as a barrel shifter) N times.
  -- A second counter of N bits toggles or not each replication bloc.
  -- By this way, the simulation time is reduced.
  signal base_cycles             : unsigned(9 downto 0)                      := (others => '0');
  signal base_cycles_max         : unsigned(9 downto 0)                      := (others => '1');
  signal toggle_cycles           : unsigned(7 downto 0)                      := (others => '0');
  --! Sine value to be stored in the DUT component and copied into the compare memory
  signal SV_sin_in               : reg_type;
  --! Sin value read from the compare memory
  signal SV_sin_compare          : reg_type;
  --! Unlatched sine result from the component
  signal SV_sin_out              : reg_type;
  --! Latched sine result from the component.
  --! It is useful to use with a wave viewer as the value is stable for each value
  signal SV_sin_out_RS_latched   : reg_type;
  --! Cosine value to be stored in the DUT component and copied into the compare memory
  signal SV_cos_in               : reg_type;
  --! Cosine value read from the compare memory
  signal SV_cos_compare          : reg_type;
  --! Unlatched cosine result from the component
  signal SV_cos_out              : reg_type;
  --! Latched cosine result from the component.
  --! It is useful to use with a wave viewer as the value is stable for each value
  signal SV_cos_out_RS_latched   : reg_type;
  --! Error counter which is reported at the end of the simulation
  signal errors_count            : natural                                   := 0;
begin
  assert (base_cycles'length * (toggle_cycles'length - 1)) >= (2 * reg_size) report "Internal error "
    severity failure;
  assert toggle_cycles'length mod 2 = 0 report "Toggle cycles size (" & integer'image(toggle_cycles'length) & ") should be even, not odd"
    severity failure;
  
  main_proc : process is
    variable toggle_cycles_v : unsigned(toggle_cycles'range);
    variable base_cycles_v   : unsigned(base_cycles'range);
    variable xor_mask        : std_logic;
    variable ind2, ind3      : natural;
  begin
    if toggle_cycles /= to_unsigned(2**(toggle_cycles'length-1)+6, toggle_cycles'length) then
      CLK_ONE_IF : if CLK = '1' then
        reg_sync_last <= reg_sync;
        if reg_sync = '1' then
          SV_sin_out_RS_latched       <= SV_sin_out;
          SV_cos_out_RS_latched       <= SV_cos_out;
          SV_sin_compare              <= ram_test_sin(compare_count);
          SV_cos_compare              <= ram_test_cos(compare_count);
          ram_test_sin(compare_count) <= SV_sin_in;
          ram_test_cos(compare_count) <= SV_cos_in;
          if RST_count_compare = 0 and ((ram_test_sin(compare_count) /= SV_sin_out) or
                                        (ram_test_cos(compare_count) /= SV_cos_out)) then
            errors_count <= errors_count + 1;
          end if;
          if compare_count /= ram_locations_size then
            compare_count <= compare_count + 1;
          else
            compare_count <= 0;
          end if;
        end if;
        ARITHM_CYCLE_IF : if arithm_cycle /= arithm_cycle_max then
          arithm_cycle <= arithm_cycle + 1;
          reg_sync     <= '0';
        else
          arithm_cycle <= 0;
          reg_sync     <= '1';
          if base_cycles = base_cycles_max then
            base_cycles_v   := (others => '0');
            toggle_cycles_v := toggle_cycles + 1;
            report "Simulation: " & integer'image(to_integer(toggle_cycles_v)) & "/" &
              integer'image(2**(toggle_cycles_v'length-1)+ 7)
              severity note;
            debug_mode_shortcut : if debug_mode and to_integer(toggle_cycles_v) = 2 then
              toggle_cycles_v(toggle_cycles_v'high)                                  := '1';
              toggle_cycles_v(toggle_cycles_v'high - 1 downto toggle_cycles_v'low+2) := (others => '0');
            end if debug_mode_shortcut;
          else
            toggle_cycles_v := toggle_cycles;
            base_cycles_v   := base_cycles + 1;
          end if;
          ind2 := base_cycles_v'low;
          ind3 := toggle_cycles_v'low;
          for ind in SV_sin_in'low to SV_sin_in'length - 1 loop
            xor_mask       := std_logic (toggle_cycles_v(ind3));
            SV_sin_in(ind) <= std_logic(base_cycles_v(ind2)) xor xor_mask;
            if ind2 /= base_cycles_v'high then
              -- next bit in the base
              ind2 := ind2 + 1;
            else
              -- base is reset, toggle is incremented
              ind2 := base_cycles_v'low;
              ind3 := ind3 + 1;
            end if;
          end loop;
          -- Do the same for the cosine
          -- the base is reset as the toggle is incremented by 1
          -- The assert, at the begining, guarantee there is not overflow
          ind2 := base_cycles_v'low;
          ind3 := ind3 + 1;
          for ind in SV_cos_in'low to SV_cos_in'length - 1 loop
            xor_mask       := std_logic (toggle_cycles_v(ind3));
            SV_cos_in(ind) <= std_logic(base_cycles_v(ind2)) xor xor_mask;
            if ind2 /= base_cycles_v'high then
              -- next bit in the base
              ind2 := ind2 + 1;
            else
              -- base is reset, toggle is incremented
              ind2 := base_cycles_v'low;
              ind3 := ind3 + 1;
            end if;
          end loop;

          if RST_count > 0 then
            RST_count <= RST_count - 1;
          else
            RST <= '0';
            if RST_count_compare > 0 then
              RST_count_compare <= RST_count_compare - 1;
            end if;
          end if;

          base_cycles   <= base_cycles_v;
          toggle_cycles <= toggle_cycles_v;
        end if ARITHM_CYCLE_IF;
      end if CLK_ONE_IF;
      CLK <= not CLK;
      wait for 10 ns;
    else
      report "Simulation is over" severity note;
      if errors_count > 0 then
        report "****** There is/are " & natural'image(errors_count) & " errors ******" severity error;
      elsif debug_mode then
        report "There is no error, *** however it is a short debug mode simulation ***" severity warning;
        else
          report "There is no error" severity note;
      end if;
      wait;
    end if;
  end process main_proc;

  Prefilter_Storage_instanc : Prefilter_Storage
    generic map (ram_locations_size => ram_locations_size)
    port map (CLK        => clk,
              RST        => RST,
              reg_sync   => reg_sync,
              SV_sin_in  => SV_sin_in,
              SV_sin_out => SV_sin_out,
              SV_cos_in  => SV_cos_in,
              SV_cos_out => SV_cos_out);
end architecture arch;
