--! @brief Inter module format definitions
--!
--! This package defines all the constants, vector types etc...
--!   to communicate from a module to others.\n
--! It is used by about all the project.\n
--! It contains some user definitions as well.
--!   They define indirectly the circuit size,
--!   the data accuracy and the data latency.

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package InterModule_formats is
  -- This part contains the user's definitions

  --! Number of bits that are added or subtracted at a time
  --! Please note, not all values are supported by components
  --! such the filtering
  constant arithm_size      : integer range 1 to 24   := 1;
  --! @brief Number of bits of the vectors in the cordic algo
  --!
  --! The angle internal calculation uses twice\n
  --! reg_size should be a multiple of arithm_size
  --! and not equal to it
  constant reg_size         : integer range 16 to 255 := 32;
  --! @brief Exponent size
  --!
  --! The output is sent as mantissa and exponent,
  --! in order to speed up amplitude comparisons
  constant exponent_size    : integer range 1 to 8    := 5;
  --! @brief Mantissa size
  --!
  --! The output is sent as mantissa and exponent,
  --! in order to speed up amplitude comparisons
  constant detect_mant_size : integer range 3 to 16   := 12;

  
  -- This part contains privates definitions
  -- It should not be modified
  
  --! @brief Type of the cordic X, Y and Z
  --!
  --! For the Cordic algorithm, X and Y should be the same type.
  --! It is relevant to also keep Z in the same scale as X and Y.\n
  --! Since we are using serial calculation, all the 3 have the same length.
  subtype reg_type is std_logic_vector(reg_size - 1 downto 0);
  --! List of reg types
  --! This is mostly used for latency compensation FIFO
  type reg_type_list is array(integer range<>) of reg_type;  
  --! Data link between the stages
  --!
  --! All the values are passed in serial mode bit by bits
  --! or small groups by small groups
  --! Metadata and quadrant are passed in parallel mode.
  type reg_sin_cos_z is record
    --! LSB comes first.
    --! For this version, the size is 1 as arithm_size is 1
    the_sin : reg_type;
    --! LSB comes first.
    --! For this version, the size is 1 as arithm_size is 1
    the_cos : reg_type;
    --! LSB comes first.
    --! For this version, the size is 1 as arithm_size is 1
    angle_z : reg_type;
  end record reg_sin_cos_z;

  type reg_sin_cos_z_list is array(integer range<>) of reg_sin_cos_z;
  
end package InterModule_formats;
