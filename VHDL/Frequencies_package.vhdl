--! @brief Frequencies
--!
--! This package defines the frequencies used by the circuit


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all;


package Frequencies is

  -- Is going to be removed
  constant global_freq      : real                    := 4.0e3;
  --! @brief Master clock
  --!
  --! Used by the angle generator and by the filters
  constant CLK_freq         : real                    := 25.0e6;


end package Frequencies;
