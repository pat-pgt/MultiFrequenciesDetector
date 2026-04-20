library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all;

--! @brief Computes the down-sampling indexes and select the data
--!
--! The frequencies should be generated as
--!   for each note for each octave do it  (see @ref AngleGene_entity).\n
--! After the down sampling, only one octave comes during each note.
--! The time slots of the n - 1 octaves receive an idle octave code.\n
--! To keep the features of the filter standard per note for all the octaves,
--!   if an octave N is down sampled by 2 ** P,
--!   the octave N - 1 is down sampled by 2 ** ( P + 1 ).\n
--! An example with 4 octaves is A by 2, B by 4, C by 8, D by 16
--!   A B A C A B A D A B A C A B A /

package Downsampling_package is

  component Downsampling_controller is
    generic (
      -- TEMP
      --! If the filter requires more down sampling
      --!   and the sampling rate is enought high,
      --!   an additional ratio is added.
      extra_downsampling : natural := 0);
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      full_sync     : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      does_catch    : out std_logic;
      does_forward  : out std_logic
      );
  end component Downsampling_controller;

end package Downsampling_package;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Controler of the downsampling
--!
--! 

entity Downsampling_controller is
  generic (
    --! If the filter requires more down sampling
    --!   and the sampling rate is enought high,
    --!   an additional ratio is added.
    extra_downsampling : natural := 0);
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    full_sync     : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t;
    does_catch    : out std_logic;
    does_forward  : out std_logic
   --! Input of X and Y. Z is voided.
--    scz_in        : in  reg_sin_cos_z;
--    scz_out       : out reg_sin_cos_z
    );
end entity Downsampling_controller;


architecture arch of Downsampling_controller is
  signal downsample_counter : std_logic_vector(N_octaves - 1 downto 0);
-- TODO make it dynamic
  signal octave_counter     : std_logic_vector(meta_data_in.octave'range);
  signal current_octave     : std_logic_vector(octave_counter'range);
begin  -- architecture arch

  assert reg_size > N_octaves report
    "The registers size (" & integer'image(reg_size) &
    ") should be at least the number of octaves (" & integer'image(N_octaves) & ")"
    severity failure;

  main_proc : process(CLK)
    variable meta_data_temp : meta_data_t;
  begin
    CLK_IF : if rising_edge(CLK) then
      if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          if meta_data_in.octave = std_logic_vector(to_unsigned(N_octaves - 1, meta_data_in.octave'length)) then
            if meta_data_in.note = std_logic_vector(to_unsigned(N_notes - 1, meta_data_in.note'length)) then
              -- Increment the counter for the convertion is the next run
              -- The octave value is still the one active during the previous run
              downsample_counter <= std_logic_vector(unsigned(downsample_counter) + 1);
              octave_counter     <= (others => '0');
            end if;
            does_forward <= '1';
          else
            does_forward <= '0';
          end if;
          if meta_data_in.octave = current_octave then
            does_catch <= '1';
          else
            does_catch <= '0';
          end if;
        else
          --! It is a basic find the first 1 bits.
          --! However, we have dozen of clock cycles to find it.
          --! Then we don't take any risk on the timing.
          if true then
            if downsample_counter(to_integer(unsigned(octave_counter))) = '1' then
              current_octave <= octave_counter;
            elsif octave_counter /= std_logic_vector(to_unsigned(N_octaves - 1, octave_counter'length)) then
              octave_counter <= std_logic_vector(unsigned(octave_counter) + 1);
            else
              current_octave <= (others => '1');
            end if;
          end if;
        end if REGSYNC_IF;
      else
        octave_counter     <= (others => '0');
        downsample_counter <= (others => '0');
      end if;
    end if CLK_IF;
  end process main_proc;
end architecture arch;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all;

--! @brief Buffer of the catched registers
--!
--! Since the downsampling is intended to give more clock cycles
--!   to the filter, the output should have always the space intervals.
--! 

entity Downsampling_buffer is
  generic (
    extra_downsampling : natural := 0);
  port (
    CLK        : in  std_logic;
    RST        : in  std_logic;
    does_catch : in  std_logic;
    scz_in     : in  reg_sin_cos_z;
    scz_out    : out reg_sin_cos_z
    );
end entity Downsampling_buffer;

architecture arch of Downsampling_buffer is

begin

end architecture arch;
