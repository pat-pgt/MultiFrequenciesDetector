library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.Utils_pac.StateNumbers_2_BitsNumbers,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Cordic_package.all;

--! @brief Controller of the down-sampling
--!
--! 

entity Downsampling_controller is
  generic (
    --! If the filter requires more down sampling
    --!   and the sampling rate is enough high,
    --!   an additional ratio is multiplied.
    extra_downsampling : positive := 1);
  port (
    CLK          : in  std_logic;
    RST          : in  std_logic;
    reg_sync     : in  std_logic;
    meta_data_in : in  meta_data_t;
    does_catch   : out std_logic;
    does_forward : out std_logic;
    pre_forward  : out std_logic
    );
end entity Downsampling_controller;


architecture arch of Downsampling_controller is
  signal downsample_counter         : std_logic_vector(N_octaves - 1 downto 0);
  signal downsample_shiftreg        : std_logic_vector(downsample_counter'range);
  signal octave_counter             : std_logic_vector(meta_data_in.octave'range);
  signal regsync_delayed            : std_logic;
  signal extra_downsampling_counter : std_logic_vector(StateNumbers_2_BitsNumbers(extra_downsampling) - 1 downto 0);
  signal extra_downsampling_note    : std_logic_vector(meta_data_in.note'range);
  signal octave_found               : std_logic;
begin  -- architecture arch

  assert extra_downsampling = 1 report
    "The extra down-sampling (" & integer'image(extra_downsampling) &
    ") and the number of notes (" & integer'image(N_notes) &
    ") should be premier to each other, otherwise some data is never processed." &
    " Future work are going to verify this is enough and not too restrictive."
    severity warning;
  assert (3 + reg_size / arithm_size) >= N_octaves report
    "The registers size (" & integer'image(reg_size) &
    ") divided by the arithmetic size (" & integer'image(arithm_size) &
    ") should be at least the number of octaves plus 3 (" & integer'image(N_octaves) & ")"
    severity failure;

  main_proc : process(CLK)
    variable meta_data_temp : meta_data_t;
  begin
    CLK_IF : if rising_edge(CLK) then
      if RST = '0' then
        REGSYNC_IF : if reg_sync = '1' then
          if meta_data_in.octave = std_logic_vector(to_unsigned(N_octaves - 1, meta_data_in.octave'length)) then
            if extra_downsampling = 1 then
              -- Don't consume resources as a simple comparator
              -- on the note found in the meta data is enough
              if meta_data_in.note = std_logic_vector(to_unsigned(N_notes - 1, meta_data_in.note'length)) then
                downsample_counter <= std_logic_vector(unsigned(downsample_counter) + 1);
              end if;
            else
              if extra_downsampling_counter =
                std_logic_vector(to_unsigned(extra_downsampling - 1, extra_downsampling_counter'length)) then
                if extra_downsampling_note =
                  std_logic_vector(to_unsigned(N_notes - 1, extra_downsampling_note'length)) then
                  extra_downsampling_note <= (others => '0');
                else
                  extra_downsampling_note <= std_logic_vector(unsigned(extra_downsampling_note) + 1);
                  downsample_counter      <= std_logic_vector(unsigned(downsample_counter) + 1);
                end if;
                extra_downsampling_counter <= (others => '0');
              else
                extra_downsampling_counter <= std_logic_vector(unsigned(extra_downsampling_counter) + 1);
              end if;
            end if;
            octave_found       <= '0';
            does_forward <= pre_forward;
            pre_forward  <= '0';
          elsif meta_data_in.octave = std_logic_vector(to_unsigned(N_octaves - 2, meta_data_in.octave'length)) then
            pre_forward  <= octave_found;
            -- Relevant only if there are only 2 octaves
            does_forward <= '0';
          else
            does_forward <= '0';
          end if;
          does_catch <= '0';
        elsif regsync_delayed = '1' then
          octave_counter <= meta_data_in.octave;
          if extra_downsampling = 1 then
            downsample_shiftreg <= downsample_counter;
          elsif extra_downsampling_counter =
            std_logic_vector(to_unsigned (0, extra_downsampling_counter'length)) then
            downsample_shiftreg <= downsample_counter;
          else
            downsample_shiftreg <= (others => '0');
          end if;
        else
          --! It is a basic find the first 1 bits to get the octave number.
          --! The goal is to avoid arithmetic in order to not break the timing.
          --! We have dozen of clock cycles to find it.
          -- The shift down-sample counter (copy) is shifted until a 1 is found.
          -- Sometimes no one is found, it shifts for ever.
          -- To avoid a compare between 2 variables, the octave is incremented
          --   at each shift. If the value is N_octaves - 1 at the end, the
          --   catch and the octave_found are asserted.
          if true then
            BITFOUND_IF : if downsample_shiftreg(downsample_shiftreg'low) = '1' then
              if octave_counter = std_logic_vector(to_unsigned(N_octaves - 1, octave_counter'length)) then
                does_catch <= '1';
              end if;
              octave_found <= '1';
            else
              downsample_shiftreg(downsample_shiftreg'high - 1 downto downsample_shiftreg'low) <=
                downsample_shiftreg(downsample_shiftreg'high downto downsample_shiftreg'low + 1);
              octave_counter <= std_logic_vector(unsigned(octave_counter) + 1);
            end if BITFOUND_IF;

          end if;
        end if REGSYNC_IF;
        regsync_delayed <= reg_sync;
      else
        octave_counter     <= (others => '0');
        downsample_counter <= (others => '0');
        if extra_downsampling /= 1 then
          extra_downsampling_counter <= (others => '0');
          extra_downsampling_note    <= (others => '0');
        end if;
      end if;
    end if CLK_IF;
  end process main_proc;
end architecture arch;



library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all;

--! @brief Buffer of the caught registers
--!
--! Since the down-sampling is intended to give more clock cycles
--!   to the filter, the output should have always the same intervals.\n

--! If the register has been loaded earlier,
--!   it is going to shift if the forward is active.
--! In 1 / <number of octaves> cases, the down-sampler acts as transparent.
--! That means the register is shifted just after being parallel loaded.\n
--! Then the data is not valid during the reg-sync.\n
--! It is not a problem for the filter (see ref ... )
--!   that does not have anything to do during the reg_sync.
--! However, if the output is directly connected to the Y to 0 input stage
--!   (see @ref Cordic_FirstStage_Y_to_0_entity) in test modes,
--!   the sign of the sine and the cosine are wrong.
--! For this reason, this data should be picked up on the xy_is_neg signal.
--!   The xy_is_neg is switched from the scz_in or the register.\n

entity Downsampling_buffer is
  generic (
    extra_downsampling : positive := 1);
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
end entity Downsampling_buffer;

architecture arch of Downsampling_buffer is
  signal forward_latch   : std_logic;
  signal meta_data_latch : meta_data_t;
begin

  meta_data_proc : process (scz_in, scz_out, pre_forward, does_catch, meta_data_latch) is
    variable meta_data_v : meta_data_t;
  begin
    meta_data_v := meta_data_latch;
    if pre_forward = '1' then
      --! The pre forward is active from the forward detection
      --!   (about 5 to 10 clock cycles after the reg_sync),
      --!   until the next reg_sync.
      --! Only one clock cycle before the reg_sync is relevant.
      --! The MD and the negative data can change and should be valid
      --!   just before the reg_sync.
      --! It would have been irrelevant to compute the assertion.
      -- This part is not synchronous to the CLK.
      -- However there is only a switch between two options
      --   without any processing.
      if does_catch = '1' then
                                        -- The previous stage (currently running) is going
                                        --   to be directly forwarded without the usage of the latch
                                        -- Then the scz_in is shown for latching by the next stage.
        xy_is_neg(1)  <= scz_in.the_cos(scz_in.the_cos'high);
        xy_is_neg(0)  <= scz_in.the_sin(scz_in.the_sin'high);
      else
        xy_is_neg(1)  <= scz_out.the_cos(scz_out.the_cos'high);
        xy_is_neg(0)  <= scz_out.the_sin(scz_out.the_sin'high);
      end if;
      meta_data_v.strobe := '1';
    else
      meta_data_v.strobe := '0';
      xy_is_neg            <= (others => '-');
    end if;
    meta_data_out <= meta_data_v;
  end process meta_data_proc;

  main_proc : process (CLK) is
  begin
    if rising_edge(CLK) then
      -- It is a similar problem than the xy_is_neg
      -- Since the does_catch should be valid
      --   at least one clock cycle before the reg_sync,
      --   and the metadata is valid a couple of clk cycles
      --   after each reg_sync, it is selected here
      if does_catch = '1' then
        meta_data_latch <= meta_data_in;
      end if;
      if reg_sync = '1' then
        if does_catch = '1' then
          scz_out <= scz_in;
        end if;
      else
        if does_forward = '1' then
          scz_out.the_cos(scz_out.the_cos'high - arithm_size downto scz_out.the_cos'low) <=
            scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'low + arithm_size);
          scz_out.the_sin(scz_out.the_sin'high - arithm_size downto scz_out.the_sin'low) <=
            scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'low + arithm_size);
                                        -- For debug reasons, insert don't cares at the top
          scz_out.the_cos(scz_out.the_cos'high downto scz_out.the_cos'high - arithm_size + 1) <=
            (others => '0');
          scz_out.the_sin(scz_out.the_sin'high downto scz_out.the_sin'high - arithm_size + 1) <=
            (others => '0');
        end if;
      end if;
    end if;
  end process main_proc;

end architecture arch;


library IEEE;
use IEEE.STD_LOGIC_1164.all,
  IEEE.NUMERIC_STD.all,
  work.InterModule_formats.all,
  work.MultiFreqDetect_package.all,
  work.Meta_data_package.all,
  work.Downsampling_package.all;


entity Downsampling_bundle is
  generic (
    -- TEMP
    --! If the filter requires more down sampling
    --!   and the sampling rate is enough high,
    --!   an additional ratio is added.
    extra_downsampling : positive := 1);
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
end entity Downsampling_bundle;

architecture arch of Downsampling_bundle is
  signal does_catch                   : std_logic;
  signal does_forward                 : std_logic;
  signal pre_forward                  : std_logic;
  signal meta_data_local              : meta_data_t;
  signal debug_does_catch_and_forward : std_logic;
begin

  debug_does_catch_and_forward <= does_catch and pre_forward;

  Downsampling_controller_instanc : Downsampling_controller
    generic map (
      extra_downsampling)
    port map (
      CLK,
      RST,
      reg_sync,
      meta_data_in,
      does_catch,
      does_forward,
      pre_forward
      );

  Downsampling_buffer_instanc : Downsampling_buffer
    generic map (
      extra_downsampling)
    port map (
      CLK,
      RST,
      reg_sync,
      does_catch,
      does_forward,
      pre_forward,
      meta_data_in  => meta_data_in,
      meta_data_out => meta_data_out,
      scz_in        => scz_in,
      scz_out       => scz_out,
      xy_is_neg     => xy_is_neg
      );
end architecture arch;
