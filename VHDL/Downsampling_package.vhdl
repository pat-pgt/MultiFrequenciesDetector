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
      --!   and the sampling rate is enough high,
      --!   an additional ratio is added.
      extra_downsampling : natural := 0);
    port (
      CLK          : in  std_logic;
      RST          : in  std_logic;
      reg_sync     : in  std_logic;
      meta_data_in : in  meta_data_t;
      does_catch   : out std_logic;
      does_forward : out std_logic;
      pre_forward  : out std_logic
      );
  end component Downsampling_controller;

  component Downsampling_buffer is
    generic (
      extra_downsampling : natural := 0);
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      does_catch    : in  std_logic;
      does_forward  : in  std_logic;
      pre_forward   : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z;
      xy_is_neg     : out std_logic_vector(1 downto 0)
      );
  end component Downsampling_buffer;

  component Downsampling_bundle is
    generic (
      -- TEMP
      --! If the filter requires more down sampling
      --!   and the sampling rate is en-ought high,
      --!   an additional ratio is added.
      extra_downsampling : natural := 0);
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z;
      xy_is_neg     : out std_logic_vector(1 downto 0)
      );
  end component Downsampling_bundle;

end package Downsampling_package;

