library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Computes the down-sampling indexes and select the data
--!
--! The frequencies should be generated as
--!   for each note for each octave do it  (see @ref AngleGene_entity).\n
--! After the down sampling, only one octave comes during each note.
--! The time slots of the n - 1 octaves receive an idle octave code.\n
--! To keep the features of the filter standard per note for all the octaves,
--!   if an octave N is down sampled by 2 ** P,
--!   the octave N + 1 is down sampled by 2 ** ( P + 1 ).\n
--! An example with 4 octaves is A by 2, B by 4, C by 8, D by 16
--!   A B A C A B A D A B A C A B A /
entity Downsampling is
  generic (
    -- TEMP
    --! If the filter requires more down sampling
    --!   and the sampling rate is enought high,
    --!   an additional ratio is added.
    extrabits : natural := 0);
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    full_sync     : in  std_logic;
    meta_data_in  : in  meta_data_t;
    meta_data_out : out meta_data_t     -- ;
    --! Input of X and Y. Z is voided.
--    scz_in        : in  reg_sin_cos_z;
--    scz_out       : out reg_sin_cos_z
    );
end entity Downsampling;


architecture arch of Downsampling is
  signal downsample_counter : std_logic_vector(N_octaves + 1 + extrabits - 1 downto 0);
-- TODO make it dynamic
  signal extra_bit_max      : std_logic_vector(extrabits downto 1) := (others => '1');
  signal octave_counter     : std_logic_vector(meta_data_in.octave'range);
  signal current_octave     : std_logic_vector(octave_counter'range);
begin  -- architecture arch


  main_proc : process(CLK)
    variable meta_data_temp : meta_data_t;
  begin
    CLK_IF : if rising_edge(CLK) then
      if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          downsample_counter <= std_logic_vector(unsigned(downsample_counter) + 1);
          meta_data_temp := meta_data_in;
          meta_data_temp.octave := current_octave;
          meta_data_out <= meta_data_temp;
        else
          --! It is a basic find the first 1 bits.
          --! However, we have dozen of clock cycles to find it.
          --! Then we don't take any risk on the timing.
          if or(downsample_counter(downsample_counter'low + extrabits - 1 downto
                                   downsample_counter'low)) = '0' then
            if downsample_counter(to_integer(unsigned(octave_counter)) + extrabits) = '1' then
              current_octave <= octave_counter;
            elsif octave_counter /= std_logic_vector(to_unsigned(N_octaves - 1, octave_counter'length)) then
              octave_counter <= std_logic_vector(unsigned(octave_counter) + 1);
            else
              current_octave <= (others => '1');
            end if;
          else
            octave_counter <= std_logic_vector(unsigned(octave_counter) + 1);
          end if;
        end if REGSYNC_IF;
      else
        downsample_counter <= (others => '0');
      end if;
    end if CLK_IF;
  end process main_proc;
end architecture arch;
