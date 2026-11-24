library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.InterModule_formats.arithm_size,
  work.InterModule_formats.reg_size,
  work.InterModule_formats.reg_type,
  work.InterModule_formats.reg_sin_cos_z,
  work.Meta_data_package.meta_data_t;

--! @brief Test the mantissa exponent conversion
--!
--! To reduce the number of combinations,
--!   the exhaustive list of slightly small mantissas is generated
--!   and tested with a limited number of cases.\n
--! * step one: 
--!   the high bit of the X input is set to 1, the mantissa is merged just under.
--!   This is only to see what happened as negative values are normally never issued.\n
--! * step two
--!   all the mantissa values populate the X input at different shifts,
--!   from X'high - 1 to the lowest possible shift (mantissa'low goes into X'low).\n
--! * step three
--!   is similar to step two but with a precision lost,
--!   until mantissa'high goes into X'low.

entity MantissaExponent_CNV_coarse_test is

  generic (
    mantissa_out_size    : integer range 3 to reg_size := 4;
    reg_syncs_per_strobe : integer range 2 to 50       := 3);

end entity MantissaExponent_CNV_coarse_test;


architecture arch of MantissaExponent_CNV_coarse_test is
  signal CLK : std_logic := '1';
  signal RST : std_logic := '0';
  signal clock_cycles_counter : unsigned(StateNumbers_2_BitsNumbers(2 * reg_size + reg_syncs_per_strobe) - 1 downto
                                         0) :=
    to_unsigned(reg_syncs_per_strobe * reg_size, StateNumbers_2_BitsNumbers(2 * reg_size + reg_syncs_per_strobe));
  signal mantissa_counter          : unsigned(mantissa_out_size - 1 downto 0) := (others => '0');
  signal mantissa_dut_out          : std_logic_vector(mantissa_counter'range);
  signal step_counter              : natural                                  := 0;
  signal step_two_exponent_counter : integer                                  := 0;
  signal step_three_exponent_counter :
    unsigned(StateNumbers_2_BitsNumbers(mantissa_out_size - 1) downto 0);
  signal exponent_dut_out : std_logic_vector(StateNumbers_2_BitsNumbers(reg_size/arithm_size) - 1 downto 0);
  signal reg_sync_dut     : std_logic;
  -- TODO initialize at least with strobe = 1
  signal meta_data_dut : meta_data_t := (octave     => (others => 'U'), note => (others => 'U'),
                                         data_valid => 'U',
                                         strobe     => '1');
  signal meta_data_out_dut  : meta_data_t;
  signal scz_dut            : reg_sin_cos_z;
  --! If the simulation outputs only a .wav file,
  --!   this is a copy of the_cos of the structure.
  signal scz_dut_cos        : reg_type;
  --!   this is a copy of strobe
  signal scz_dut_strobe     : std_logic;
  signal scz_dut_out_strobe : std_logic;
  component MantissaExponent_CNV_coarse is
    port (
      CLK           : in  std_logic;
      RST           : in  std_logic;
      reg_sync      : in  std_logic;
      meta_data_in  : in  meta_data_t;
      meta_data_out : out meta_data_t;
      scz_in        : in  reg_sin_cos_z;
      mantissa_out  : out std_logic_vector;
      exponent_out  : out std_logic_vector(StateNumbers_2_BitsNumbers(reg_size/arithm_size) - 1 downto 0)
      );

  end component MantissaExponent_CNV_coarse;

begin
  scz_dut_cos        <= scz_dut.the_cos;
  scz_dut_strobe     <= meta_data_dut.strobe;
  scz_dut_out_strobe <= meta_data_out_dut.strobe;

  main_proc : process is
    variable enable_mantissa_count : boolean;
    variable dut_cos_var           : std_logic_vector(reg_size - 1 downto 0);
  begin
    if to_integer(clock_cycles_counter) /= reg_size
      or and(mantissa_counter) /= '1'
      or step_counter /= 2
      or CLK /= '0' then
      CLK_1_IF : if CLK = '1' then
        if to_integer(clock_cycles_counter) = 0 then
          case step_counter is
            when 0 =>
              dut_cos_var(dut_cos_var'high) := '1';
              dut_cos_var(dut_cos_var'high - 1 downto
                          dut_cos_var'high - mantissa_counter'length - 1 + 1) :=
                std_logic_vector(mantissa_counter);
              dut_cos_var(dut_cos_var'high - mantissa_counter'length - 1 + 1 - 1 downto
                          dut_cos_var'low) :=
                (others => '0');
              enable_mantissa_count := true;
            when 1 =>
              dut_cos_var := (others => '0');
              -- implicit   dut_cos_var(dut_cos_var'high) <= '0';
              dut_cos_var(dut_cos_var'high - 1 - step_two_exponent_counter downto
                          dut_cos_var'high - mantissa_counter'length - 1 + 1 - step_two_exponent_counter) :=
                std_logic_vector(mantissa_counter);
              if step_two_exponent_counter < (reg_size - mantissa_out_size - 1) then
                step_two_exponent_counter <= step_two_exponent_counter + 1;
                enable_mantissa_count     := false;
              else
                step_two_exponent_counter <= 0;
                enable_mantissa_count     := true;
              end if;
            when 2 =>
              -- TODO TODO
              dut_cos_var           := (others => 'Z');
              enable_mantissa_count := true;
            when others => null;
          end case;
          scz_dut.the_cos <= dut_cos_var;
          if enable_mantissa_count then
            if and(mantissa_counter) = '1' then
              step_counter <= step_counter + 1;
            end if;
            mantissa_counter <= mantissa_counter + 1;
          end if;
          reg_sync_dut         <= '0';
          clock_cycles_counter <= clock_cycles_counter + 1;
        elsif (to_integer(clock_cycles_counter) + 1) mod (reg_size + 1) = 1 then
          reg_sync_dut         <= '0';
          -- redundant reg_sync_dut         <= '0';
          clock_cycles_counter <= clock_cycles_counter + 1;
        elsif to_integer(clock_cycles_counter) + 1 = reg_syncs_per_strobe * (reg_size + 1) then
          reg_sync_dut         <= '1';
          clock_cycles_counter <= (others => '0');
        -- after reg_syncs_per_strobe cycles the statement above has already caught
        -- This is for the other register sync
        elsif (to_integer(clock_cycles_counter) + 1) mod (reg_size + 1) = 0 then
          clock_cycles_counter <= clock_cycles_counter + 1;
          reg_sync_dut         <= '1';
        -- Wait a couple of cycles in order to reset the register sync
        -- in the "else" statement
        -- Set the strobe at the last reg_syncs_per_strobe cycle
        --   to be ready for the next data set.
        elsif (to_integer(clock_cycles_counter) + 1) = ((reg_syncs_per_strobe - 1) * (reg_size + 1) + 2) then
          meta_data_dut.strobe <= '1';
          -- redundant reg_sync_dut         <= '0';
          clock_cycles_counter <= clock_cycles_counter + 1;
        -- Clear the strobe a couple of clock cycles after the register sync
        --   unless the last one, that has already being caught.
        elsif (to_integer(clock_cycles_counter) + 1) mod (reg_size + 1) = 2 then
          meta_data_dut.strobe <= '0';
          -- redundant reg_sync_dut         <= '0';
          clock_cycles_counter <= clock_cycles_counter + 1;
        else
          -- redundant reg_sync_dut         <= '0';
          clock_cycles_counter <= clock_cycles_counter + 1;
        end if;
        -- We are not setting new data, then shift the data register
        if to_integer(clock_cycles_counter) /= 0 then
          scz_dut.the_cos(scz_dut.the_cos'high - arithm_size downto scz_dut.the_cos'low) <=
            scz_dut.the_cos(scz_dut.the_cos'high downto scz_dut.the_cos'low + arithm_size);
          -- For better test, insert X to the unused data
          scz_dut.the_cos(scz_dut.the_cos'high downto scz_dut.the_cos'high - arithm_size + 1) <=
            (others => 'X');
        end if;
      end if CLK_1_IF;
      CLK <= not CLK;
      wait for 1 ps;
    else
      assert false report "Simulation is over" severity note;
      wait;                             -- for ever
    end if;
  end process main_proc;

  MantissaExponent_CNV_coarse_instanc : MantissaExponent_CNV_coarse
    port map (
      CLK,
      RST,
      reg_sync      => reg_sync_dut,
      meta_data_in  => meta_data_dut,
      meta_data_out => meta_data_out_dut,
      scz_in        => scz_dut,
      mantissa_out  => mantissa_dut_out,
      exponent_out  => exponent_dut_out
      );


end architecture arch;
