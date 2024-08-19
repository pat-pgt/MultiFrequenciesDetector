--! @brief General definitions of the frequencies detection
--!
--! OUTDATED should contain the definition of the number of cordic stages

library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.math_real.all;

package MultiFreqDetect_package is
  --! @brief
  --!
  --! TODO
  type cordic_stages_num_list is array(integer range<>) of positive;

  --! @brief
  --!
  --! TODO
  type prefilter_stages_offset_list is array(integer range<>) of real;
end package MultiFreqDetect_package;

