--! @brief Frequencies
--!
--! This package defines the frequencies used by the circuit


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all;


package Frequencies is

  -- Is going to be removed
  -- as it is defined by the other parameters
  constant global_freq      : real                    := 4.0e3;
  --! @brief Master clock
  --!
  --! Used by the angle generator and by the filters
  constant CLK_freq         : real                    := 25.0e6;
  --! @brief prefilter ratio offset
  --!
  --! Defines the ratio between the theoretical cutoff frequency
  --!   and the actual one.
  --! It is intended to keep the phase linear or use a lot of pre-filter stages.
  --! For more information, see the README.modules file.
  constant Prefilter_ratio_freq : real := 1.0;

end package Frequencies;
