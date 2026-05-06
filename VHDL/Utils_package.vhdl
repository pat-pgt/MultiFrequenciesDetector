package Utils_pac is

  --! @brief Get the size of the vectors for a known number of states
  --!
  --! Speaks by itself.\n
  --! If the input is not a positive, the result is 0
  --! Designed for vectors with a range xyz downto 0.
  function StateNumbers_2_BitsNumbers (chans_number : in integer) return natural;

end package Utils_pac;

package body Utils_pac is

  function StateNumbers_2_BitsNumbers (chans_number : in integer) return natural is
    variable bits_numbers : natural := 0;
  begin
    while chans_number > 2 ** bits_numbers loop
      bits_numbers := bits_numbers + 1;
    end loop;
    return bits_numbers;
  end function StateNumbers_2_BitsNumbers;
  
end package body Utils_pac;
