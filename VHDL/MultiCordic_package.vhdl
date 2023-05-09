--
--	Package File Template
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
--
--   To use any of the example code shown below, uncomment the lines and modify as necessary
--

library IEEE;
use IEEE.STD_LOGIC_1164.all,
   ieee.numeric_std.all,
  ieee.math_real.all;

package MultiCordic_package is
	-- With new versions of VHDL, put that into package generic
	constant reg_size			: integer range 16 to 256 := 28;
    constant exponent_size      : integer range 1 to 8 := 5;
	constant N_cordics		    : integer range 1 to 60 := 3;
	constant global_freq		: real := 4.0e3;
	constant CLK_freq			: real := 25.0e6;
    constant detect_mant_size   : integer range 3 to 16 := 12;

    --! @brief Type of the cordic X, Y and Z
    --!
    --! For the Cordic algorithm, X and Y should be the same type.
    --! It is relevant to also keep Z in the same scale as X and Y.\n
    --! Since we are using serial calculation, all the 3 have the same length.
	subtype reg_type is std_logic_vector( reg_size - 1 downto 0 );
    --! @brief Met data of the frequencies
    --!
    --! This data follows each value.\n
    --! Since the computations run is sequence, it is not that relevant.
    --! However, it is useful for debug.
    --! It can be used to verify values are in sync in an improved version.
    type meta_data_t is record
      --! First one 0x0 => octave -1, last one 0x7 => octave 6.
      octave            : std_logic_vector( 2 downto 0 );
      --! First one 0 => C/do, last one 0xb => B/si.
      note              : std_logic_vector( 3 downto 0 );
    end record meta_data_t;
    --! Data link between the stages
    --!
    --! All the values are passed in serial mode.
    --! Metadata and quadrant are passed in parallel mode.
	type reg_sin_cos_toggle is record
        RUN			    : std_logic;
		reg_sync 	   	: std_logic;
        --! LSB comes first.
		the_sin			: std_logic;
        --! LSB comes first.
		the_cos			: std_logic;
        --! LSB comes first.
        --! 3 high bits are always 0 (or 1 if Z is negative),
        --!   or Z might be equal to 0010..0
		angle_z			: std_logic;
        meta_data       : meta_data_t;
        --! In which PI/4 zone is Z.\n
        --! In the Cordic before the filtering,
        --!   only X and Y are used, Z should converge to 0.
        --! If Z is between PI/4 and PI/2 modulo PI/2, X and Y are toggled.\n
        --! In the Cordic after the filtering,
        --!   only X and Z are used, Y should converge to 0.
        --! If X and Y has been toggled in the first stage,
        --!   the symmetry against PI/4 is computed.
        --! If X, Y or both have been converted to positive,
        --!   Z is updated accordingly.
        quadrant        : std_logic_vector( 2 downto 0 );
    end record reg_sin_cos_toggle;
    
	type reg_global is array( N_cordics - 1 downto 0 ) of reg_type;

    --! @brief Common ports
    --!
    --! This is a dummy component, never instantiated.
    --! It is designed to document the signal
    --!   used by most of the components.
    --! It is also designed to run some asserts.
    component MultiCordic_Dummy_Doc_and_Asserts is
      generic (
        --! Unless otherwise, it is in debug mode is true.
        debug_mode			: boolean
		);
      port (
        --! Master clock. It is active on rising edges.
		CLK					:  in std_logic;
        --! Reset. It is active when '1', it is in normal run if '0'.
		RST					:  in std_logic;
        --! @brief Run this sequence
        --!
        --! The interleaves make some components idle at some frames.
        --! The component runs if '1'. 
		RUN					:  in std_logic
        );
      end component MultiCordic_Dummy_Doc_and_Asserts;
    --! @brief Computes angles
    --!
    --! Computes the angle of the next value.\n
    --! It adds the current angle to the constant
    --!  according to the frequency.\n
    --! This module also handles which the notes and the frequencies
    --!  come in which order
    --! \return The angle and the metadata
    component MultiCordic_Angle is
      generic (
        debug_mode			: boolean := false
		);
      port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		RUN					:  in std_logic;
		reg_sync 			:  in std_logic;
		global_sync			:  in std_logic;
		angle_z				:  out reg_type;
        meta_data           : out meta_data_t
		);
    end component MultiCordic_Angle;
    --! @brief Cordic input stage 
    --!
    --! Runs the 3 first Cordic iterations of the bloc before the filters.
    --! The input value would have been placed on the X axis,
    --!   Y would have been set to 0 and the angle would have been set to Z.\n
    --! The vector is placed into the relevant PI/2 steps quadrant.
    --! A PI/2 multiple is subtracted from the angle. Then Z is between 0 and PI/2.
    --! The 2 high bits of the quadrant signal are set.\n
    --! In case Z is greater than PI/4, the symmetry against PI/4 is computed
    --!   and the quadrant low bit is set.
    --! At the end, X and Y should be toggled accordingly.\n
    --! This is mostly to avoid X and Y to grow too much,
    --!  as only low arc-tg are computed.
    component MultiCordic_FirstStage is
      generic (
		debug_mode			: boolean := false
		);
      port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		RUN					:	in std_logic;
		reg_sync 			:  in std_logic;
        -- unsigned min = 0, max = 2.PI - epsilon
		angle_z				:  in reg_type;
        meta_data           : in meta_data_t;
        -- signed 2's complement
        -- practically, the input_s is used only for tests
        --   set it to others => '0' otherwise
		input_c, input_s	:  in reg_type;
		output_scsa			: out reg_sin_cos_toggle
	);
    end component MultiCordic_FirstStage;
    --! @brief Cordic input stage
    --!
    --! Runs the 3 first Cordic iterations of the detection bloc, after the filters.
    --! The X and Y values would have been placed accordingly to X and Y,
    --!   and the angle would have been set to 0.\n
    --! The X and Y are placed into the relevant PI/2 steps quadrant.
    --! Then X, Y or both are negated.
    --! A PI/2 multiple is set to the angle.
    --! The 2 high bits of the quadrant signal are set.\n
    --! In this version, there is no comparison to toggle X and Y
    --!   if the angle is greater then PI/4.\n
    --! This is mostly to avoid X and Y to grow to much,
    --!  as only low arc-tg are computed.
    component MultiCordic_Detect_FS is
      generic (
        debug_mode          : boolean := false
        );
      port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		input_scsa			:  in reg_sin_cos_toggle;
		output_scsa			: out reg_sin_cos_toggle
        );
    end component MultiCordic_Detect_FS;
    --! @brief Cordic intermediary stage
    --!
    --! Runs one iteration of the Cordic alghorithm.
    --! It is used for all the frequencies.\n
    --! It can run in the P->R or R->P mode.
    --! This component should be instantiated as many times as iterations.
    --! The output of the previous one is connected to the input of the next one.\n
    --! Its register latency is 1 full register to receive and 1 to transmit
    --! See details in the implementation
    component MultiCordic_IntermStage is
      generic (
		posit_interm			: natural;
        --! @brief Cordic mode
        --!
        --! True: angle converges to 0 as the vector is turned accordingly
        --! False: vector converges to (x,0) as the angle is updated accordingly 
        vector_from_angle       : boolean;
        --! Debug mode: angle is the real angle or a simple value taken from the position
		debug_mode_angle		: boolean := false;
        --! Debug mode: send the input to the output as it
		debug_mode_donothing	: boolean := false
		);
      port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		input_scsa			:  in reg_sin_cos_toggle;
		output_scsa			: out reg_sin_cos_toggle
        );
    end component MultiCordic_IntermStage;
    --! @brief Cordic last stage
    --!
    --! It is not, strictly speaking a Cordic stage.
    --! It is used for tests while converting serial to parallel data.
    --! The data is left as fixed values.
    --! For test purposes, it can be used everywhere in the chain.\n
    --! While using the filter, only one frequency is selected.
    --! That make the result friendly viewable.
    component MultiCordic_LastStage is
      generic (
        --! True: selects one frequency.
        --! False: all the frequencies passes through.
        use_metadata_filter   : boolean := true;
        --! If use_metadata_filter is true, which frequency.
        metadata_filter       : meta_data_t);
      port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		input_scsa			:  in reg_sin_cos_toggle;
		output_sin			: out reg_type;
		output_cos			: out reg_type
	);
    end component MultiCordic_LastStage;

    --! @brief Cordic last detection stage
    --!
    --! It converts serial data into parallel data.\n
    --! It can be used for test purposes with a conversion into float values.
    --! It can be used as the final stage to process easily the amplitude
    --!   and the phase.\n
    --! It detects the exponent. The exponent is common
    --!   for the sine and the cosine. It is based on the greater value.\n
    --! While using the filter, only one frequency is selected.
    --! That make the result friendly viewable.
    --! Its latency is 1 full register to receive, the output is parallel
    component MultiCordic_Detect_LS is
      generic (
        --! True: selects one frequency.
        --! False: all the frequencies passes through.
        use_metadata_filter   : boolean := true;
        --! If use_metadata_filter is true, which frequency.
        metadata_filter       : meta_data_t);
      port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		input_scsa			:  in reg_sin_cos_toggle;
        --! Used for debug as it always should converge to 0.
        output_sin_s        : out std_logic;
		output_sin_m   		: out std_logic_vector(detect_mant_size - 1 downto 0);
        --! Amplitude of the signal
        output_cos_s        : out std_logic;
		output_cos_m   		: out std_logic_vector(detect_mant_size - 1 downto 0);
		output_exponent		: out std_logic_vector(exponent_size - 1 downto 0);
        --! Phase of the signal
        output_angle        : out reg_type;
        --! Meta-data octave
        output_octave       : out std_logic_vector( 2 downto 0 );
        --! Meta-data note
        output_note         : out std_logic_vector( 3 downto 0 )
	);
    end component MultiCordic_Detect_LS;


component MultiCordic_Prefilter is
	port (
		CLK					:  in std_logic;
		RST					:  in std_logic;
		input_scsa			:  in reg_sin_cos_toggle;
		output_scsa		    : out reg_sin_cos_toggle
	);
end component MultiCordic_Prefilter;



component Multicordic_bundle is
	generic(
		interm_size : positive := 5
		);
	port (
		CLK										:  in std_logic;
		RST										:  in std_logic;
		input										:  in reg_type;
		output_sin_ser, output_cos_ser				: out std_logic;
        output_gen_sync                         : out std_logic;
        output_sin_s        : out std_logic;
		output_sin_m   		: out std_logic_vector(detect_mant_size - 1 downto 0);
        output_cos_s        : out std_logic;
		output_cos_m   		: out std_logic_vector(detect_mant_size - 1 downto 0);
		output_exponent		: out std_logic_vector(exponent_size - 1 downto 0);
        output_angle        : out reg_type;
        output_octave       : out std_logic_vector( 2 downto 0 );
        output_note         : out std_logic_vector( 3 downto 0 )	);
end component Multicordic_bundle;

function freq_list_populate( constant debug_mode : boolean ) return reg_global;

function arctg_2_angle( constant shift_bits : natural; constant debug_mode : boolean ) return reg_type;


end package MultiCordic_package;

package body MultiCordic_package is


function freq_list_populate( constant debug_mode : boolean ) return reg_global is
	variable temp : reg_global;
begin
	if debug_mode = false then
		assert false report "Sorry not yet implemented" severity failure;
        -- The angle module should be updated to handle notes and octaves.
        -- The size of the freq_list should always be 12
        -- The increments should be multiplied by 2 power octave (+1)
	else
		for ind in temp'range loop
			for ind2 in temp( ind )'range loop
              if ind2 < ( temp( ind )'high - 2 - 2 * ind ) then
					temp(ind)(ind2) := '1';
				else
					temp(ind)(ind2) := '0';
				end if;
			end loop;
		end loop;
	end if;
	return temp;
end function freq_list_populate;

function arctg_2_angle( constant shift_bits : natural; constant debug_mode : boolean ) return reg_type is
  variable temp : reg_type;
  variable the_atg_r : real;
  variable the_atg_i : natural;
  variable the_atg_u : unsigned( reg_type'range );
begin
  the_atg_r := arctan( 1.0, real( 2 ** shift_bits ));
  -- normalise the angle to: min = 0, max = 2.pi - epsilon
  the_atg_r := the_atg_r / ( 2.0 * math_pi );
  -- "convert" into bin
  assert shift_bits < 32 report "Interm stage uses a big shift of " &
    integer'image( shift_bits )
    severity warning;
  the_atg_i := integer( round( the_atg_r * real( 2 ** reg_type'length ) ) );
  temp := std_logic_vector( to_unsigned( the_atg_i , reg_type'length ));
             
  assert not debug_mode report "Angle " & real'image( the_atg_r ) & " =int=> " & natural'image( the_atg_i ) & " pour " & integer'image( shift_bits ) severity note;
  
  return temp;
end function arctg_2_angle;

end MultiCordic_package;
