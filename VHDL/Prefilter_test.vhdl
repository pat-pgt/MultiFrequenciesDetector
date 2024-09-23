library IEEE;
use IEEE.STD_LOGIC_1164.all,
  ieee.numeric_std.all,
  ieee.math_real.all,
  work.MultiFreqDetect_package.prefilter_stages_offset_list,
  work.InterModule_formats.all,
  work.Meta_data_package.all,
  work.Frequencies.all,
  work.Prefilter_package.all;


--! @brief Test the pre-filter stages
--!
--! The test is performed without RAM
--!   with 2 times 3 notes 1 octaves or 3 octaves 1 note.
--! Indeed, there is a need to check there is no crosstalk
--!   between the sine and the cosine
entity Prefilter_Stages_test is

end entity Prefilter_Stages_test;



architecture arch of Prefilter_Stages_test is
  signal CLK            : std_logic                                := '0';
  signal reg_sync_count : integer range 0 to 32                    := 0;
  signal sample_count   : integer range 0 to 2                     := 0;
  constant cycles_max   : integer range 5 to integer'high          := 300;
  signal cycles_count   : integer range 0 to cycles_max            := 0;
  signal reset_count    : natural                                  := 0;
  signal send_report    : bit                                      := '0';
  signal RST            : std_logic                                := '1';
  signal reg_sync       : std_logic;
  signal sc_in, sc_out  : reg_sin_cos_z;
  signal meta_data_in   : meta_data_t;
  signal meta_data_out  : meta_data_t;
  constant offsets_list : prefilter_stages_offset_list(1 downto 1) := (1 => 1.0);
  signal out_s1         : reg_type;
  signal out_c1         : reg_type;
  signal out_s2         : reg_type;
  signal out_c2         : reg_type;
  signal out_s3         : reg_type;
  signal out_c3         : reg_type;      
begin

  main_proc : process
    variable sin_cos_in : real;
  begin
    if cycles_count < cycles_max then
      CLK_IF : if CLK = '1' then
        REGSYNC_IF : if reg_sync_count < 32 then
          reg_sync       <= '0';
          reg_sync_count <= reg_sync_count + 1;
          if reg_sync_count = 0 then
            case sample_count is
              when 0 => out_s1 <= sc_out.the_sin; out_c1 <= sc_out.the_cos;
              when 1 => out_s2 <= sc_out.the_sin; out_c2 <= sc_out.the_cos;
              when 2 => out_s3 <= sc_out.the_sin; out_c3 <= sc_out.the_cos;
            end case;
          end if;
        else
          sample_count_if : if sample_count < 2 then
            sample_count <= sample_count + 1;
          else
            if reset_count < 10 then
              reset_count <= reset_count + 1;
            else
              RST          <= '0';
              cycles_count <= cycles_count + 1;
            end if;
            sample_count <= 0;
          end if sample_count_if;

          reg_sync_count <= 0;
          reg_sync       <= '1';
        end if REGSYNC_IF;
      else
        -- The raising edge of the clock
        if reg_sync = '1' then
          sin_cos_in := round (real(2 ** (reg_size - 1) - 1)) *
                        sin(real(6 * cycles_count + 2 * sample_count) * MATH_PI / 500.0);
          sc_in.the_sin <= std_logic_vector(to_signed(integer(sin_cos_in), sc_in.the_sin'length));
          sin_cos_in    := round (real(2 ** (reg_size - 1) - 1)) *
                        sin(real(6 * cycles_count + 2 * sample_count + 1) * MATH_PI / 500.0);
          sc_in.the_cos <= std_logic_vector(to_signed(integer(sin_cos_in), sc_in.the_cos'length));
        else
          sc_in.the_sin(sc_in.the_sin'high - arithm_size downto sc_in.the_sin'low) <=
            sc_in.the_sin(sc_in.the_sin'high downto sc_in.the_sin'low + arithm_size);
          sc_in.the_cos(sc_in.the_cos'high - arithm_size downto sc_in.the_cos'low) <=
            sc_in.the_cos(sc_in.the_cos'high downto sc_in.the_cos'low + arithm_size);          
        end if;
      end if CLK_IF;
      CLK <= not CLK;
      wait for 50 ns;
    else
      wait;
    end if;
  end process main_proc;

  report_proc : process
  begin
    wait until send_report = '1';
    report"C'est fini" severity note;
  end process report_proc;

  DUT_instanc : Prefilter_bundle generic map (
    stages_offsets => offsets_list,
    is_debug_mode => 2 )
    port map (
      CLK           => CLK,
      RST           => RST,
      reg_sync      => reg_sync,
      meta_data_in  => meta_data_in,
      meta_data_out => meta_data_out,
      scz_in        => sc_in,
      scz_out       => sc_out);

end architecture arch;


