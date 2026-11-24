library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.InterModule_formats.arithm_size,
  work.InterModule_formats.reg_size,
  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_sin_cos_z,
  work.Meta_data_package.meta_data_t;

--! @brief Converts the result into mantissa exponent
--!
--! This is a extra entity of the project
--!   but it is useful for some tests as it reduces the output vector size.
--! It can be used for a pseudo logarithm conversion.
--! It can be used for the VGA Q&D display extra project.\n
--! The latency is 2 as there is a register sync
--!   to detect the exponent and another one to apply it.\n
--! The exponent of this entity is a multiple of the arithm_size.
--! It can be used directly in case of an arithmetic size equal to 1.\n
--! For other values, a simple switch can be the solution,
--!   or there is a need to apply an algorithm to compute the fine value.
--! In such case, the arithmetic size should be lower than
--!   the number of clock cycles between 2 register sync.

entity MantissaExponent_CNV_coarse is
  port (
    CLK           : in  std_logic;
    RST           : in  std_logic;
    reg_sync      : in  std_logic;
    meta_data_in  : in  meta_data_t;
    --! Output the meta-data.\n
    --! The strobe is really a data valid and stable.\n
    --! When the strobe goes high, it remains until the data is no longer valid.
    --! In some cases, it can be used as a lower frequency clock.
    meta_data_out : out meta_data_t;
    --! X is taken from the scz\n
    --! Y is voided\n
    --! Z is forwarded as it or voided,
    --!   to be decided if this is handled or another entity is going to do.
    scz_in        : in  reg_sin_cos_z;
    --! The size of the mantissa output can be shorter than the registers.\n
    --! There is a few restrictions.
    --! Please note the highest bit is always 0 is this project,
    --!   as the conversion X,Y to module angle is a non negative and a 0 2.PI.
    mantissa_out  : out std_logic_vector;
    --! The power 2 of the number of bits is the size of the registers
    --!   divided by the arithmetic.\n
    --! Since it may not be a perfect power of 2,
    --!   the highest exponent is always (others=>'1')\n
    --! It is tricky as the exponent format relays on this ratio.
    --! A new version is going to compute a standard binary exponent
    --!   during the application (see in the code).
    exponent_out  : out std_logic_vector(StateNumbers_2_BitsNumbers(reg_size/arithm_size) - 1 downto 0)
    );

end entity MantissaExponent_CNV_coarse;


architecture arch of MantissaExponent_CNV_coarse is
  signal metadata_during_detection : meta_data_t;
  signal X_D_detect                : reg_type;
  signal exponent_detect           : std_logic_vector(StateNumbers_2_BitsNumbers(reg_size/arithm_size) - 1 downto 0);
  signal exponent_apply            : std_logic_vector(exponent_detect'range); 
  signal bloc_is_zero              : std_logic;
begin
  assert mantissa_out'length > arithm_size + 1
    report "The length of the mantissa output (" & integer'image(mantissa_out'length) & ")" &
    " should be greater than the arithmetic size plus 1" & integer'image( arithm_size + 1 ) & ")"
    severity failure;
  assert mantissa_out'length < ( reg_size - 1 )
    report "The length of the mantissa output (" & integer'image(mantissa_out'length) & ")" &
    " should not be greater than the registers size (" & integer'image( reg_size ) & ")"
    severity warning;
  assert false
    report "There are " & integer'image( reg_size - 1 - mantissa_out'length ) &
    " exponents values without any precision lost in the mantissa"
    severity note;
  
  detect_proc : process(CLK)
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        metadata_during_detection <= meta_data_in;
        bloc_is_zero              <= '0';
        exponent_detect           <= (others => '0');
      elsif metadata_during_detection.strobe = '1' then
        -- Check if they are all zeros.
        -- If so, increment the counter, otherwise clear it.
        if or(scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low)) = '0' then
          --! This is OK until 64 bits arithmetic size 1
          --! There are fast carries strategies.
          -- For larger register a shift register may be preferred.
          -- If the strobe is 1, that does not make conflict with the apply process.
          exponent_detect <= std_logic_vector(unsigned(exponent_detect) + 1);
        else
          exponent_detect <= (others => '0');
        end if;
      end if REGSYNC_IF;
      REGSYNC_IF_2 : if reg_sync = '0' then
      -- As usual, shift the registers
        X_D_detect(X_D_detect'high downto X_D_detect'high - arithm_size + 1) <=
          scz_in.the_cos(scz_in.the_cos'low + arithm_size - 1 downto scz_in.the_cos'low);
        X_D_detect(X_D_detect'high - arithm_size downto X_D_detect'low) <=
          X_D_detect(X_D_detect'high downto X_D_detect'low + arithm_size );
      end if REGSYNC_IF_2;
    end if CLK_IF;
  end process detect_proc;

  apply_proc : process(CLK)
    variable md_temp : meta_data_t;
  begin
    CLK_IF : if rising_edge(CLK) then
      REGSYNC_IF : if reg_sync = '1' then
        if metadata_during_detection.strobe = '1' then
          -- Some new data is arriving.
          -- Clear the strobe. Set the new MD. Set the exponent.
          md_temp := metadata_during_detection;
          md_temp.strobe := '0';
          meta_data_out <= md_temp;
          exponent_out   <= not exponent_detect;
          exponent_apply <= exponent_detect;
        else
          -- No new data is arriving. Set the strobe
          meta_data_out.strobe <= '1';
        end if;
      elsif metadata_during_detection.strobe = '0'  and and(exponent_apply) /= '1' then
        -- If the strobe is on, that does not make conflicts with the detect process
        exponent_apply <= std_logic_vector(unsigned(exponent_apply) + 1);
        mantissa_out( mantissa_out'high downto mantissa_out'high - arithm_size + 1 ) <=
          X_D_detect( X_D_detect'low + arithm_size - 1 downto X_D_detect'low );
        -- Since the length may or may not be a multiple of the arithmetic size
        -- the copy should be made bit to bit.
        for ind in 0 to mantissa_out'length - arithm_size - 1 loop
          mantissa_out( mantissa_out'low + ind ) <= mantissa_out( mantissa_out'low + ind + arithm_size);
        end loop;
      end if REGSYNC_IF;
    end if CLK_IF;
  end process apply_proc;


end architecture arch;
