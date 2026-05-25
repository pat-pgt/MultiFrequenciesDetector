library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  ieee.math_real.all,
  work.Downsampling_package.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;


--! @brief Test of the down-sampling, controller part.
--!
--! Especially with new values of N_octaves, N_notes or extra-downsamling,
--!   this check the selections are still correct.\n
--! The CXX DC end to end with down-sampling enabled
--!   should detect if something is wrong.
--! However, it is better to run this one.
--! Especially with low precision data, an error can be missed.
--! This is really faster as it does not perform any Arithmetic.\n
--! Some conditions on the values may apply,
--!   especially with an extra down-sampling.
--! If it is wrong, some octave note couples are ignored.
--! Some parameters should be premier to each other.
--! It has not yet been formally verified.
--! Then this test has to be re-run for each parameters set.\n
--! For octaves from low to high (A to C), the selection should be like C B C A C B C /
--! In case of a 2 ratio extra down-sampling
--!   the selection should be like C / B / C / A / C / B / C / / / \n
--! This test displays the results for each octave note couple.\n
--! The final goals are:\
--! * All the octave note couples should appear.
--!   Some have a very low occurrence, but all should be present.\n
--! * For a given octave, all the notes should appear the same number of times.
--!     The number of samples is calculated to have a full cycle.
--!   At least if the number of samples is increased,
--!     the appearances should converge to the same value.
--! * The ratio of the number of the selections of all the notes of the highest octave
--!     should be the half of their occurrences.
--!   This should apply for all its notes.\n
--! * Recursively the other half (see above) is divided between a half for the N-1 octave,
--!     and the other half for the lower octaves according the same schema.
--!   The occurrences of all the notes for each lower octave should be the half
--!     of the one of all the notes of the upper one.\n
--! * The intervals of appearances should be constant and related
--!     to the octave note couple.
--!   The occurrences should be correctly scrambled as for instance C C C C B B A / are wrong.
--!   Statistics check, for each octave note couple, the interval is constant.
--!   The interval should be the number of samples divided by the number of occurrences.
--!   The standard deviation should be a perfect 0.\n
entity Downsampling_test is
  generic (
    extra_downsampling : positive := 1);
end entity Downsampling_test;


architecture arch of Downsampling_test is
  signal main_counter                 : unsigned(19 downto 0)               := (others             => '0');
  signal main_counter_max             : unsigned(main_counter'range)        := ("11111111", others => '0');
  signal main_counter_end             : unsigned(main_counter'range)        := (others             => '1');
  signal CLK                          : std_logic                           := '0';
  signal RST                          : std_logic_vector(8 downto 0)        := (others             => '1');
  signal reg_sync                     : std_logic;
  signal meta_data_in                 : meta_data_t;
  signal the_octave_in                : unsigned(meta_data_in.octave'range) := (others             => '0');
  signal the_note_in                  : unsigned(meta_data_in.note'range)   := (others             => '0');
  signal does_catch                   : std_logic;
  signal does_forward                 : std_logic;
  signal does_preforward              : std_logic;
  signal debug_does_catch_and_forward : std_logic;
  signal start_offset                 : natural                             := 30;
  type on_data_elem is record
    count           : natural;
    last_time_stamp : natural;
    space           : real;
    space2          : real;
  end record on_data_elem;
  type on_data_t is array (0 to N_octaves - 1, 0 to N_notes - 1) of on_data_elem;
  signal on_data : on_data_t :=
    (others => (others => (0, 0, 0.0, 0.0)));
  signal reg_sync_counter : natural;

begin
  meta_data_in.octave <= std_logic_vector(the_octave_in);
  meta_data_in.note   <= std_logic_vector(the_note_in);

  debug_does_catch_and_forward <= does_catch and does_preforward;

  main_proc : process is
    variable on_var    : on_data_elem;
    variable the_space : real;
  begin
--    if main_counter /= main_counter_max and main_counter /= main_counter_end then
    if reg_sync_counter /= 3 * N_notes * N_octaves * 2 ** N_octaves * extra_downsampling then
      -- * 3 times all the cycle
      -- * The down-sampling is repeated for N_notes times
      -- * For each note, there is a choice between N_octaves samples
      -- * The down-sampling has a full cycle on 2 ** N_octaves
      --   The are 2 ** N_octaves - 1 active processing and 1 with the strobe down
      -- * The extra_downsampling needs more cycles to get the same output
      if CLK = '1' then
        RST(RST'high)                    <= '0';
        RST(RST'high - 1 downto RST'low) <= RST(RST'high downto RST'low + 1);

        main_counter <= main_counter + 1;

        -- 1 reg_sync clock cycles and 7 data clock cycles are enough
        --   as there is no arithmetic.
        -- 15 has been chosen to avoid a false positive if the number
        --   of octaves is increased.
        if or(main_counter(main_counter'low + 3 downto main_counter'low)) = '0' then
          reg_sync <= '1';
          if start_offset > 0 then
            start_offset <= start_offset - 1;
          else
            reg_sync_counter <= reg_sync_counter + 1;
            if does_catch = '1' then
              on_data(to_integer(the_octave_in), to_integer(the_note_in)).count <=
                on_data(to_integer(the_octave_in), to_integer(the_note_in)).count + 1;
              if on_data(to_integer(the_octave_in), to_integer(the_note_in)).last_time_stamp /= 0 then
                -- Skip the first one
                the_space := real(reg_sync_counter -
                                  on_data(to_integer(the_octave_in), to_integer(the_note_in)).last_time_stamp);
                on_data(to_integer(the_octave_in), to_integer(the_note_in)).space <=
                  on_data(to_integer(the_octave_in), to_integer(the_note_in)).space +
                  the_space;
                on_data(to_integer(the_octave_in), to_integer(the_note_in)).space2 <=
                  on_data(to_integer(the_octave_in), to_integer(the_note_in)).space2 +
                  the_space * the_space;
              end if;
              on_data(to_integer(the_octave_in), to_integer(the_note_in)).last_time_stamp <= reg_sync_counter;
            end if;
          end if;
        else
          reg_sync <= '0';
          REG_SYNC_IF : if reg_sync = '1' then
            if to_integer(the_octave_in) < N_octaves - 1 then
              the_octave_in <= the_octave_in + 1;
            else
              the_octave_in <= (others => '0');
              if to_integer(the_note_in) < N_notes - 1 then
                the_note_in <= the_note_in + 1;
              else
                the_note_in <= (others => '0');
              end if;
            end if;
          end if REG_SYNC_IF;
        end if;

      end if;
      CLK <= not CLK;
      wait for 1 ps;
    elsif main_counter /= main_counter_end then
      main_counter <= main_counter_end;

      assert false report "Simulation is over" severity note;
      assert false report
        "Based on " & natural'image(reg_sync_counter) & " samples: " &
        ", (" & integer'image(reg_sync_counter/(N_notes * N_octaves)) & " per octave note)"
        severity note;
      OCTAVES_LOOP : for ind in 0 to N_octaves - 1 loop
        NOTES_LOOP : for ind2 in 0 to N_notes - 1 loop
          on_var := on_data(ind, ind2);
          assert false report
            "O:" & natural'image(ind) & " N:" & integer'image(ind2) &
            " processed " & natural'image(on_var.count) & " times   " &
            " avg space " & real'image(on_var.space / real(on_var.count - 1)) & "   " &
            " std dev " & real'image(on_var.space2 / real(on_var.count - 1) -
                                     (on_var.space / real(on_var.count - 1)) ** 2)
            severity note;
        end loop NOTES_LOOP;  -- ind
      end loop OCTAVES_LOOP;
      wait for 1 ps;
    else
      wait;
    end if;

  end process main_proc;


  Downsampling_controller_instanc : Downsampling_controller
    generic map (
      extra_downsampling)
    port map (
      CLK,
      RST          => RST(RST'low),
      reg_sync     => reg_sync,
      meta_data_in => meta_data_in,
      does_catch   => does_catch,
      does_forward => does_forward,
      pre_forward  => does_preforward
      );

end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.Downsampling_package.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;


--! @brief Test of the down-sampling, mostly the data management.
--!
--! The goals are mostly
--! * to help the design
--! * a quick verification there is no cross talk in the channels \n
--! For a complete verification, see the CXX software
--!   @ref ...
entity Downsampling_bundle_test is

end entity Downsampling_bundle_test;

architecture arch of Downsampling_bundle_test is
  signal reg_counter : unsigned(StateNumbers_2_BitsNumbers(reg_size / arithm_size) + 1 - 1 downto
                                0) := (others => '0');
  signal main_counter     : unsigned(12 downto 0)        := (others      => '0');
  signal main_counter_max : unsigned(main_counter'range) := ('1', others => '0');
  signal CLK              : std_logic                    := '0';
  signal RST              : std_logic_vector(8 downto 0) := (others      => '1');
  signal reg_sync         : std_logic;

  signal next_meta_data : meta_data_t;
  signal next_octave    : std_logic_vector(next_meta_data.octave'range);
  signal next_note      : std_logic_vector(next_meta_data.note'range);
  signal next_strobe    : std_logic;
  signal next_xy_is_neg : std_logic_vector(1 downto 0);

  signal next_latched_meta_data : meta_data_t;
  signal next_latched_octave    : std_logic_vector(next_meta_data.octave'range);
  signal next_latched_note      : std_logic_vector(next_meta_data.note'range);
  signal next_latched_strobe    : std_logic;

  signal next_scz : reg_sin_cos_z;
  signal next_sin : std_logic_vector(next_scz.the_sin'range);
  signal next_cos : std_logic_vector(next_scz.the_cos'range);

  signal prev_meta_data : meta_data_t;
  signal the_octave_in  : unsigned(prev_meta_data.octave'range) := (others => '0');
  signal the_note_in    : unsigned(prev_meta_data.note'range)   := (others => '0');
  signal prev_scz       : reg_sin_cos_z;
  signal prev_sin       : reg_type;
  signal prev_cos       : reg_type;

  signal fillup_x_scz  : std_logic;
  signal fillup_y_scz  : std_logic;
  signal last_fillup_x : std_logic;
  signal last_fillup_y : std_logic;

  signal match_tint_x_vector : std_logic_vector( arithm_size - 1 downto 0 );
  signal match_tint_y_vector : std_logic_vector( arithm_size - 1 downto 0 );

  signal match_tint_x                : std_logic;
  signal match_tint_y                : std_logic;
  signal number_good_X, number_bad_X : natural := 0;
  signal number_good_Y, number_bad_Y : natural := 0;

  signal run_match_tint                     :    boolean;
--! In order to verify the data matches what has been catched,
  --!   we use a maximum number of std_logic symbols to tint.
  --! A system-C version would have make the things easy
  --!   as one can inherit the type by some meta-data.
  function tint_bits_per_octave_note (the_value : in natural)
    return std_logic is
    variable the_return : std_logic;
  begin
    case the_value is
      when 0      => the_return := '0';
      when 1      => the_return := '1';
      when 2      => the_return := 'W';
      when 3      => the_return := 'Z';
      when 4      => the_return := 'U';
      when 5      => the_return := 'X';
      when others => null;
    end case;
    return the_return;
  end function tint_bits_per_octave_note;
begin
  prev_meta_data.octave <= std_logic_vector(the_octave_in);
  prev_meta_data.note   <= std_logic_vector(the_note_in);

  prev_scz.the_sin <= prev_sin;
  prev_scz.the_cos <= prev_cos;

  next_octave <= next_meta_data.octave;
  next_note   <= next_meta_data.note;
  next_strobe <= next_meta_data.strobe;

  next_latched_octave <= next_latched_meta_data.octave;
  next_latched_note   <= next_latched_meta_data.note;
  next_latched_strobe <= next_latched_meta_data.strobe;

  next_sin <= next_scz.the_sin;
  next_cos <= next_scz.the_cos;

  main_proc : process is
    variable match_tint_x_v      : boolean;
    variable match_tint_y_v      : boolean;
    variable match_tint_cmp_bloc : std_logic_vector(arithm_size - 1 downto 0);
  begin
    if main_counter /= main_counter_max then
      CLK_IF : if CLK = '1' then
        RST(RST'high)                    <= '0';
        RST(RST'high - 1 downto RST'low) <= RST(RST'high downto RST'low + 1);

        PRE_REG_IF : if to_integer(reg_counter) = reg_size / arithm_size then
          reg_counter   <= (others => '0');
          main_counter  <= main_counter + 1;
          reg_sync      <= '1';
        else
          reg_counter <= reg_counter + 1;
          reg_sync    <= '0';
          REG_SYNC_IF : if reg_sync = '1' then
            if to_integer(the_octave_in) < N_octaves - 1 then
              the_octave_in <= the_octave_in + 1;
            else
              the_octave_in <= (others => '0');
              if to_integer(the_note_in) < N_notes - 1 then
                the_note_in <= the_note_in + 1;
              else
                the_note_in <= (others => '0');
              end if;
            end if;
          end if REG_SYNC_IF;
          STROBE_IF : if run_match_tint then
            match_tint_cmp_bloc := (others => tint_bits_per_octave_note(to_integer(unsigned(next_octave))));
            match_tint_x_v      := true;
            -- TEMP TEMP TEMP For now, check ony the body, not the edges
            for ind in 4 to reg_size / arithm_size - 4 loop
              if match_tint_cmp_bloc /=
                next_scz.the_cos(next_scz.the_cos'low + (ind + 1) * arithm_size - 1 downto
                                 next_scz.the_cos'low + ind * arithm_size) then
                match_tint_x_v := false;
              end if;
            end loop;
            if match_tint_x_v then
              match_tint_x <= '1';
              number_good_X <= number_good_X + 1;
            else
              match_tint_x <= '0';
              number_bad_X <= number_bad_X + 1;
            end if;
            match_tint_x_vector <= match_tint_cmp_bloc;

            match_tint_cmp_bloc := (others => tint_bits_per_octave_note(to_integer(unsigned(next_note))));
            match_tint_y_v      := true;
            -- TEMP TEMP TEMP For now, check ony the body, not the edges
            for ind in 4 to reg_size / arithm_size - 4 loop
              if match_tint_cmp_bloc /=
                next_scz.the_sin(next_scz.the_sin'low + (ind + 1) * arithm_size - 1 downto
                                 next_scz.the_sin'low + ind * arithm_size) then
                match_tint_y_v := false;
              end if;
            end loop;
            if match_tint_y_v then
              match_tint_y <= '1';
              number_good_Y <= number_good_Y + 1;
            else
              match_tint_y <= '0';
              number_bad_Y <= number_bad_Y + 1;
            end if;
            match_tint_y_vector <= match_tint_cmp_bloc;
            run_match_tint      <= false;
          else
            match_tint_x <= 'Z';
            match_tint_y <= 'Z';
          end if STROBE_IF;
        end if PRE_REG_IF;
        prev_cos(prev_cos'high downto prev_cos'high - arithm_size + 1) <=
          (others => tint_bits_per_octave_note(to_integer(the_octave_in)));
        prev_sin(prev_sin'high downto prev_sin'high - arithm_size + 1) <=
          (others => tint_bits_per_octave_note(to_integer(the_note_in)));
        prev_cos(prev_cos'high - arithm_size downto prev_cos'low) <=
          prev_cos(prev_cos'high downto prev_cos'low + arithm_size);
        prev_sin(prev_sin'high - arithm_size downto prev_sin'low) <=
          prev_sin(prev_sin'high downto prev_sin'low + arithm_size);
      else
        if reg_sync = '1' then
          next_latched_meta_data <= next_meta_data;
          if next_strobe = '1' then
            run_match_tint <= true;
          end if;
        end if;
      end if CLK_IF;
      CLK <= not CLK;
      wait for 1 ps;
    else
      wait;                             -- for ever;
    end if;
  end process main_proc;


  Downsampling_bundle_instanc : Downsampling_bundle
    generic map (
      extra_downsampling => 1)
    port map (
      CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync,
      meta_data_in  => prev_meta_data,
      meta_data_out => next_meta_data,
      scz_in        => prev_scz,
      scz_out       => next_scz,
      xy_is_neg     => next_xy_is_neg
      );


end architecture arch;
