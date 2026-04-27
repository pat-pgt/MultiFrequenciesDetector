library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.Downsampling_package.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;


--! @brief Test of the downsampling, mostly the controller.
--!
--! The goals are mostly
--! * to help the design
--! * a quick verification of the signals in general
--! * a verification of the scrambling
--!   A B A C A B A /
--! * to help he debug, in case the CXX verification fails.\n
--! For a complete verification, see the CXX software
--!   @ref ...
entity Downsampling_test is

end entity Downsampling_test;


architecture arch of Downsampling_test is
  signal main_counter                 : unsigned(15 downto 0)               := (others      => '0');
  signal main_counter_max             : unsigned(main_counter'range)        := ('1', others => '0');
  signal CLK                          : std_logic                           := '0';
  signal RST                          : std_logic_vector(8 downto 0)        := (others      => '1');
  signal reg_sync                     : std_logic;
  signal meta_data_in                 : meta_data_t;
  signal the_octave_in                : unsigned(meta_data_in.octave'range) := (others      => '0');
  signal the_note_in                  : unsigned(meta_data_in.note'range)   := (others      => '0');
  signal does_catch                   : std_logic;
  signal does_forward                 : std_logic;
  signal does_preforward              : std_logic;
  signal debug_does_catch_and_forward : std_logic;
begin
  meta_data_in.octave <= std_logic_vector(the_octave_in);
  meta_data_in.note   <= std_logic_vector(the_note_in);

  debug_does_catch_and_forward <= does_catch and does_preforward;

  main_proc : process is
  begin
    if main_counter /= main_counter_max then
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
    else
      wait;
    end if;

  end process main_proc;


  Downsampling_controller_instanc : Downsampling_controller
    generic map (
      extra_downsampling => 0)
    port map (
      CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync,
      meta_data_in  => meta_data_in,
      does_catch    => does_catch,
      does_forward  => does_forward,
      pre_forward   => does_preforward
      );

end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.Downsampling_package.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;


--! @brief Test of the downsampling, mostly the data management.
--!
--! The goals are mostly
--! * to help the design
--! * a quick verification there is no cross talk in the channels \n
--! For a complete verification, see the CXX software
--!   @ref ...
entity Downsampling_bundle_test is

end entity Downsampling_bundle_test;

architecture arch of Downsampling_bundle_test is
  signal reg_counter      : unsigned(StateNumbers_2_BitsNumbers( reg_size / arithm_size ) + 1 - 1 downto
                                     0)         := (others      => '0');
  signal main_counter     : unsigned(12 downto 0)        := (others      => '0');
  signal main_counter_max : unsigned(main_counter'range) := ('1', others => '0');
  signal CLK              : std_logic                    := '0';
  signal RST              : std_logic_vector(8 downto 0) := (others      => '1');
  signal reg_sync         : std_logic;
  
  signal next_meta_data : meta_data_t;
  signal next_octave    : std_logic_vector(next_meta_data.octave'range);
  signal next_note      : std_logic_vector(next_meta_data.note'range);
  signal next_xy_is_neg : std_logic_vector(1 downto 0);

  signal next_scz : reg_sin_cos_z;
  signal next_sin : std_logic_vector(next_scz.the_sin'range);
  signal next_cos : std_logic_vector(next_scz.the_cos'range);

  signal next_xz_is_neg : std_logic_vector(1 downto 0);

  signal prev_meta_data : meta_data_t;
  signal the_octave_in                : unsigned(prev_meta_data.octave'range) := (others      => '0');
  signal the_note_in                  : unsigned(prev_meta_data.note'range)   := (others      => '0');
  signal prev_scz       : reg_sin_cos_z;
  signal prev_sin : reg_type;
  signal prev_cos : reg_type;

  signal fillup_scz : std_logic;
begin
  prev_meta_data.octave <= std_logic_vector(the_octave_in);
  prev_meta_data.note <= std_logic_vector(the_note_in);
  
  prev_scz.the_sin <= prev_sin;
  prev_scz.the_cos <= prev_cos;

  next_octave <= next_meta_data.octave;
  next_note   <= next_meta_data.note;

  next_sin <= next_scz.the_sin;
  next_cos <= next_scz.the_cos;

  main_proc : process is
  begin
    if main_counter /= main_counter_max then
      CLK_IF : if CLK = '1' then
        RST(RST'high)                    <= '0';
        RST(RST'high - 1 downto RST'low) <= RST(RST'high downto RST'low + 1);

        if to_integer( reg_counter ) = reg_size / arithm_size then
          reg_counter <= ( others => '0');
          main_counter <= main_counter + 1;
          reg_sync <= '1';
          SET_FILLUP: case to_integer(the_octave_in) is
            when 0 => fillup_scz <= '0';
            when 1 => fillup_scz <= '1';
            when 2 => fillup_scz <= 'L';
            when 3 => fillup_scz <= 'H';
            when 4 => fillup_scz <= 'U';
            when 5 => fillup_scz <= 'X';
            when others => null;
          end case SET_FILLUP;
        else
          reg_counter <= reg_counter + 1;
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
        prev_cos(prev_cos'high downto prev_cos'high - arithm_size + 1 ) <=
          ( others => fillup_scz );
        prev_sin(prev_sin'high downto prev_sin'high - arithm_size + 1 ) <=
          ( others => fillup_scz );
        prev_cos(prev_cos'high - arithm_size downto prev_cos'low) <=
          prev_cos(prev_cos'high downto prev_cos'low + arithm_size);
        prev_sin(prev_sin'high - arithm_size downto prev_sin'low) <=
          prev_sin(prev_sin'high downto prev_sin'low + arithm_size);
      end if CLK_IF;
      CLK <= not CLK;
      wait for 1 ps;
    else
      wait;                             -- for ever;
    end if;
  end process main_proc;


  Downsampling_bundle_instanc : Downsampling_bundle
    generic map (
      extra_downsampling => 0)
    port map (
      CLK,
      RST           => RST(RST'low),
      reg_sync      => reg_sync,
      meta_data_in  => prev_meta_data,
      meta_data_out => next_meta_data,
      scz_in        => prev_scz,
      scz_out       => next_scz,
      xy_is_neg     => next_xz_is_neg
      );


end architecture arch;
