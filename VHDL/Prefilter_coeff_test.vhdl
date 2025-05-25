library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.Meta_data_package.all,
  work.Frequencies.all,
  work.PreFilter_package.all;


--! @brief Pre-filter coefficients test
--!
--! This computes the filtering, using using math types
--!   in order to speed up and test many configurations.\n
--! The goal is to build something as close as possible
--!   to the real implementation, rather than applying
--!   a formula.
entity Prefilter_coeff_test is
  generic
    (
      -- The defaults values are only intended to run this entity as a top (test) level
      master_clk             : real                            := 20.0e3;
      frequency              : real                            := 1.2e2;
      coeff_shifts           : natural                         := 10;
      periods_before_measure : integer range 2 to integer'high := 10;
      --! Issue the original sine and cosine before the filter.\n
      --! This is to estimate the error due to the non infinite sampling
      --!   see in the code.
      full_debug_notes       : boolean                         := true
      );
  port
    (
      done       : out std_logic;
--      report_in :  in bit;
      report_out : out bit
      );
end entity Prefilter_coeff_test;


architecture arch of Prefilter_coeff_test is
  constant master_period  : time                 := (1.0 / master_clk) * 1 sec;
  signal CLK              : bit                  := '0';
  signal state            : integer range 0 to 3 := 0;
  constant clk_per_period : real                 := master_clk / frequency;
  signal curr_period      : real                 := 0.0;
  signal nbre_periods     : natural              := 0;
  signal periods          : natural              := 0;
  signal capture_result   : boolean              := false;

  signal state_var                            : real := 0.0;
  constant coeff_real                         : real := 1.0 / real(2 ** coeff_shifts);
  signal average_output                       : real := 0.0;
  signal avg_abs_input_sin, avg_abs_input_cos : real := 0.0;
  signal avg_abs_output                       : real := 0.0;

  signal report_in : std_logic;
begin

  --! Since there is a "warm up" to stabilize the output signal,
  --!   the filter is going to run a certain number of periods
  --!   before the monitoring starts
  main_proc : process
  begin
    case state is
      when 0 =>
        curr_period <= curr_period + 1.0;
        period_if : if curr_period >= clk_per_period then
          curr_period <= curr_period - clk_per_period;
          nbre_periods_if : if nbre_periods < (periods_before_measure - 1) then
            nbre_periods <= nbre_periods + 1;
          else
            nbre_periods   <= 0;
            state          <= 1;
            capture_result <= true;
          end if nbre_periods_if;
        end if period_if;
        wait for master_period;
      when 1 =>
        curr_period <= curr_period + 1.0;
        period_if_2 : if curr_period >= clk_per_period then
          curr_period    <= curr_period - clk_per_period;
          capture_result <= false;
          state          <= 2;
        end if period_if_2;
        wait for master_period;
      when 2 =>
        state <= 3;
        wait for master_period;
      when 3 =>
        done <= '1';

        report_in <= '1';


        wait;
    end case;
    
  end process main_proc;

  --! The sine and cosine are generated and the algo is applied.
  --! In order to measure the amplitude and offset in one period,
  --!   there is a need to synchronise the capture.
  filter_simul : process (curr_period)
    variable the_sin, the_cos : real;
    variable the_output       : real;
    variable input_minus_prev : real;
  begin
    the_sin          := sin(2.0 * math_pi * real(curr_period) / clk_per_period);
    the_cos          := cos(2.0 * math_pi * real(curr_period) / clk_per_period);
    -- the formula is SV_next = ( 1 - K ) . SV_prev + K . input
    -- For computation, we calculate
    -- * input - SV_prev
    -- * multiply by K
    -- * add to sv_prev
    input_minus_prev := (the_sin - state_var) * coeff_real;
    the_output       := input_minus_prev + state_var;
    state_var        <= the_output;
    if capture_result then
      average_output    <= average_output + the_output;
      avg_abs_output    <= avg_abs_output + abs(the_output);
      avg_abs_input_sin <= avg_abs_input_sin + abs(the_sin);
      -- For report only, see below
      avg_abs_input_cos <= avg_abs_input_cos + abs(the_cos);
    end if;
  end process filter_simul;

  report_proc : process
  begin
    wait until report_in = '1';
    -- The sampling is not infinite.
    -- The number of samples is not a multiple of 4,
    --   even worst, it is not an integer. 
    -- Then there are some variations of the result when the phase shifts.
    -- Since we are verifying a filter, there are phase shifts.
    -- The cosine and the full report are to estimate the error.
    -- The integration from 0 to 2.PI of the absolute value of sine or cosine is 4.0.
    -- They are normalized to 1.0
    if full_debug_notes then
      report
        "Freq: " & real'image(frequency) &
        ", coeff: " & real'image(coeff_real) &
        ", avg (0): " & real'image(average_output / clk_per_period) &
        ", abs sin (1): " & real'image(avg_abs_input_sin * math_pi / (2.0 * clk_per_period)) &
        ", abs cos (1): " & real'image(avg_abs_input_sin * math_pi / (2.0 * clk_per_period)) &
        ", abs out: " & real'image(avg_abs_output * math_pi / (2.0 * clk_per_period)) &
        -- ... The goal is the ratio
        ", abs out db: " & real'image(20.0 * log10(avg_abs_output / avg_abs_input_sin))
        severity note;
      else
        report
          "Freq: " & real'image(frequency) &
          ", coeff: " & real'image(coeff_real) &
          ", avg (0): " & real'image(average_output / clk_per_period) &
          ", abs out: " & real'image(avg_abs_output * math_pi / (2.0 * clk_per_period)) &
          -- ... The goal is the ratio
          ", abs out db: " & real'image(20.0 * log10(avg_abs_output / avg_abs_input_sin))
          severity note;
      end if;
    report_out <= '1';
    wait for 1 ns;
  end process report_proc;
  
end architecture arch;
