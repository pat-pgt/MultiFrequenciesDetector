
library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all;

--! @brief Pre-filter 
--!
--! This computes the prefilter
--! to remove high frequencies with a minimum of resources.\n
--! They are first order infinite impulse response filters
--! using 1/2**N as a coefficient.\n
--! Since the final filtering is very low (against the sampling rate),
--! a downsampling follows.\n
--! That reduces the final filtering resources because
--! * the low pass frequency is closer than the sampling rate (calculation precision)
--! * there are less multiplication and additions.
package PreFilter_package is
  constant ram_data_size : positive := 16;

--! @brief Pre-filter IIR compute
--!
--! This computes one prefilter. They should be bundled by pair
--! for the sine and the cosine.\n
--! The state variable (of the infinite impulse response)
--! is given at the reg_sync.
--! the N-1 state variable is outputted as well.
  component Prefilter_IIR_compute is
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      shifts_calc   : in  std_logic_vector(4 downto 0);
      state_var_in  : in  reg_type;
      state_var_out : out reg_type;
      sc_in         : in  reg_type;
      sc_out        : out reg_type
      );
  end component Prefilter_IIR_compute;


--! @brief Pre-filter state variable storage
--!
--! Stores in a RAM based barrel shifter two state variables
--!
  component Prefilter_Storage is
    generic (
      --! ram_locations_size : positive := N_notes * N_octaves - 3;
      --! N_notes * N_octaves is the nulber of IIR filters involved\n
      --! multiplied by 2 because there is the sine and the cosine to handle\n
      --! multiplied by ram_bloc_size because reg_size is lower than ram_data_size,
      --! then there are multiple reads and writes.\n
      --! minus 3 because 3 values are in the pipelined process of the pre-filter.
      ram_locations_size : positive := 29
      );
    port (
      CLK        : in  std_logic;
      RST        : in  std_logic;
      reg_sync   : in  std_logic;
      SV_sin_in  : in  reg_type;
      SV_cos_in  : in  reg_type;
      --! Output sine register.\n
      --! To keep a standard inter-modules interface, theese reg_type registers
      --! are shifted by arithm size between the reg_sync (active)
      SV_sin_out : out reg_type;
      --! Output cosine register.\n
      --! To keep a standard inter-modules interface, theese reg_type registers
      --! are shifted by arithm size between the reg_sync (active)
      SV_cos_out : out reg_type
      );
  end component Prefilter_Storage;
--! @brief Prefilter stage
--!
--! This is a pair of sine and cosine calculation
--! with their associated memory storage and
--! delay for the metadata.
  component Prefilter_stage is
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : in  meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z
      );
  end component Prefilter_stage;
--! @brief Prefilter bundle
--!
--! This is the bundle for the whoose wants more stages
  component Prefilter_bundle is
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : in  meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      scz_out       : out reg_sin_cos_z
      );
  end component Prefilter_bundle;


--  function Prefilter_cnv_metadata_2_shifts(signal metadata_in : meta_data_t; constant ratiofrom_cuttoff : real)
--    return std_logic_vector;

end package PreFilter_package;

