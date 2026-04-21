library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.Downsampling_package.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;


entity Downsampling_test is

end entity Downsampling_test;


architecture arch of Downsampling_test is
  signal main_counter     : unsigned(15 downto 0)               := (others      => '0');
  signal main_counter_max : unsigned(main_counter'range)        := ('1', others => '0');
  signal CLK              : std_logic                           := '0';
  signal RST              : std_logic_vector(8 downto 0)        := (others      => '1');
  signal reg_sync         : std_logic;
  signal full_sync        : std_logic;
  signal meta_data_in     : meta_data_t;
  signal meta_data_out    : meta_data_t;
  signal the_octave_in    : unsigned(meta_data_in.octave'range) := (others      => '0');
  signal the_note_in      : unsigned(meta_data_in.note'range)   := (others      => '0');
begin
  meta_data_in.octave <= std_logic_vector(the_octave_in);
  meta_data_in.note   <= std_logic_vector(the_note_in);

  main_proc : process is
  begin
    if main_counter /= main_counter_max then
      if CLK = '1' then
        RST(RST'high)                    <= '0';
        RST(RST'high - 1 downto RST'low) <= RST(RST'high downto RST'low + 1);

        main_counter <= main_counter + 1;

        if or(main_counter(main_counter'low + 3 downto main_counter'low)) = '0' then
          reg_sync <= '1';
        else
          reg_sync <= '0';
          if reg_sync = '1' then
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
          end if;
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
      full_sync     => full_sync,
      meta_data_in  => meta_data_in,
      meta_data_out => meta_data_out    --
     --! Input of X and Y. Z is voided.
--    scz_in        : in  reg_sin_cos_z;
--    scz_out       : out reg_sin_cos_z
      );

end architecture arch;
