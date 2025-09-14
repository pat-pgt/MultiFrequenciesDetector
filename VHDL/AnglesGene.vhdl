library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Input_modules.all,
  work.MultiFreqDetect_package.all;


--! @brief Computes angles
--!
--! Computes the angle and the meta data of the next value.\n
--! Due to the downsampling, the loops are\n
--! * a counter of the notes at the top level\n
--! * a counter of the octave inside the notes\n
--! * a run or sync flipflop and an arithmetic bloc counter
--! inside the notes\n
--! \return The angle, the metadata and the sync bit

entity AngleGene is
  generic (
    debug_mode : boolean := false
    );
  port (
    CLK       : in  std_logic;
    RST       : in  std_logic;
    --! A full cycle of all the requested notes of all the requested octaves
    --! completed
    full_sync : out std_logic;
    --! Starts a new computation of a given note and octave
    --! Note, it is not in phase with the meta data, but the shift is constant
    reg_sync  : out std_logic;
    --! angle out, updated with reg_sync
    --! Only the half high bit are available.
    --! The others are for the computation precision.
    angle_z   : out reg_type;
    --! angle out, updated with reg_sync
    meta_data : out meta_data_t
    );
end entity AngleGene;

architecture rtl of AngleGene is
  constant freq_list   : angle_list_per_note_t := angle_constants_populate_reg(true);
-- temporary, the time a ram is implemented
  signal angle_storage : angle_list_per_note_t;

  signal angle_octave    : std_logic_vector(2 * reg_size - 1 downto 0);
  signal angle_note      : std_logic_vector(2 * reg_size - 1 downto 0);
  signal angle_step      : std_logic_vector(2 * reg_size - 1 downto 0);
  -- A function to get the size against the max should be written
  signal arithm_counter  : std_logic_vector(7 downto 0);
  signal octave_counter  : std_logic_vector(meta_data.octave'range);
  signal note_counter    : std_logic_vector(meta_data.note'range);
  signal delay_note_mem  : std_logic_vector(meta_data.note'range);
  signal delay_note_calc : std_logic_vector(meta_data.note'range);

  signal carry_z           : std_logic;
  signal debug_z, debug_z2 : reg_type;
  -- TODO make the vector size dynamic
  signal N_cordic_count    : std_logic_vector(7 downto 0);
begin
  assert reg_size mod arithm_size = 0
    report "The size of the registers (" & integer'image(reg_size) &
    ") should be a multiple of the arithm_size (" & integer'image(arithm_size) & ")"
    severity failure;
  assert reg_size /= arithm_size
    report "The size of the registers (" & integer'image(reg_size) &
    ") should not be equal to the arithm_size (" & integer'image(arithm_size) & ")"
    severity failure;
  
  count_proc : process (CLK)
    constant bit_padding_0 : std_logic_vector(1 downto 1) := "0";
    variable local_sub_add : std_logic_vector(1 downto 0);
    variable reg_temp      : std_logic_vector(reg_type'range);
    variable angle_temp    : std_logic_vector(2 * reg_size - 1 downto 0);
    variable full_sync_v : std_logic;
  begin
    -- May be outdated
    -- For each note:
    -- * step 1: add a constant to the cumulative angle
    -- ** get it from the RAM
    -- ** sequencially add the constant
    -- ** re-write the result
    -- * step2: pass the result to the next pipelined process
    -- ** multiply by 2 for each octave 
    -- * step3: update the angle out, using only the half high bits
    CLK_IF : if rising_edge(CLK) then
      RST_IF : if RST = '0' then
        full_sync_v := '0';
        -- arithm counter run from 0 to N as 0 is the sync
        ARITHM_C : if unsigned(arithm_counter) = to_unsigned(reg_size / arithm_size, arithm_counter'length) then
          arithm_counter   <= (others => '0');
          reg_sync         <= '1';
          -- Update the note and octave counters
          -- Octave counter runs from 0 to N - 1
          OCTAVE_C : if unsigned(octave_counter) = to_unsigned(N_octaves - 1, octave_counter'length) then
            octave_counter <= (others => '0');
            -- A new angle current comes here
            angle_octave   <= angle_note;
            -- Note counter runs from 0 to N_notes - 1
            if unsigned(note_counter) = to_unsigned(N_notes - 1, note_counter'length) then
              note_counter <= (others => '0');
              full_sync_v := '1';
            else
              note_counter <= std_logic_vector(unsigned(note_counter) + 1);
            end if;
            -- TEMPORARY, a serial calc should be implement'ed as well
            angle_temp := angle_storage(to_integer(unsigned(note_counter)));
            angle_step <= freq_list(to_integer(unsigned(note_counter)));
            angle_temp := std_logic_vector(unsigned(angle_temp) +
                                           unsigned(freq_list(to_integer(unsigned(note_counter)))));
            angle_storage(to_integer(unsigned(note_counter))) <= angle_temp;
            angle_note                                        <= angle_temp;
            delay_note_mem                                    <= note_counter;
            delay_note_calc                                   <= delay_note_mem;
          else
            octave_counter <= std_logic_vector(unsigned(octave_counter) + 1);
            -- The angle current is mutiply by 2 for the new octave
            -- modulo 2.PI, coded as (others=>'1') + 1
            angle_octave(angle_octave'high downto angle_octave'low + 1) <=
              angle_octave(angle_octave'high - 1 downto angle_octave'low);
            angle_octave(angle_octave'low) <= angle_octave(angle_octave'high);
          end if OCTAVE_C;
        else
          if reg_sync = '1' then
            angle_z          <= angle_octave(angle_octave'high downto angle_octave'high + 1 - angle_z'length);
            meta_data.note   <= delay_note_calc;
            meta_data.octave <= octave_counter;
            reg_sync <= '0';
          end if;
          arithm_counter <= std_logic_vector(unsigned(arithm_counter) + 1);
        end if ARITHM_C;
        full_sync <= full_sync_v;
      else
        for ind in angle_storage'range loop
          angle_storage(ind) <= (others => '0');
        end loop;
        delay_note_calc <= (others => '0');
        delay_note_mem  <= (others => '0');
        angle_z         <= (others => '0');
        angle_octave    <= (others => '0');
        angle_note      <= (others => '0');
        angle_step      <= (others => '0');
        arithm_counter  <= (others => '0');
        octave_counter  <= (others => '0');
        note_counter    <= (others => '0');
        reg_sync        <= '0';
        full_sync       <= '0';
      end if RST_IF;
    end if CLK_IF;
  end process count_proc;
end architecture rtl;
