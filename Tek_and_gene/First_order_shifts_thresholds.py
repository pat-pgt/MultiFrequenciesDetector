import sys
import math

# Validate some assumptions of the prefilter

# The formula of the division at the cut off frequency is
# K = 1 - exp( -2.PI.<cutoff>/<samplingrate>)

# The approximation is: we are linearising
# Since the cut-off frequency is low against the input sampling rate,
#   the exponential is considered linear,
#   at least for the low frequencies.

# 

# The sampling rate is the master clock divided by
#   * the number of bits plus 1
#   * the number of notes per octave
#   * the number of octaves
# The cut off frequency is the last frequency
#   * multiplied by the shift (see the top level documentation)
#   * divided by the number of notes per octaves

# As a high example, for a 45meg master clock, a A6 highest note, 32 (+1) bits, 7 octaves, shift 1.0
#   the ratio is 1 / 121.8
# As a low example, with a starting note at A00, the ratio is 1 / 14168 

# This software estimates the error, in order to help to choose the parameters.
# Since the thresholds are established on the low frequencies,
#   more the frequency is high, more
 
# We are looking for the list of the 1/2**N
#   then we compute log(1-1/2**N)/2.PI
#   and log(1-1/2**N)/2.PI

if len(sys.argv) == 1:
    high_shifts = 12
else:
    high_shifts = int(sys.argv[1])

sys.stdout.write("First, we compute the ratios with full precision and with light formulas\n");
sys.stdout.write("shifts\tratio with approx.\t1/ratio with approx.\tratio without\t1/ratio without\n");

for the_shifts in range( 3, 15 ):
    ratio_actual =  1 / ( pow( 2, the_shifts ) *( 2 * math.pi))
    ratio_theoritical = -( math.log( 1 - 1 / pow( 2, the_shifts ))) /( 2 * math.pi )
    if the_shifts == high_shifts:
        sys.stdout.write('\n')
    sys.stdout.write( str( the_shifts ) + '\t' + str( ratio_theoritical ) + '\t' + str( 1 / ratio_theoritical ) + '\t' + str( ratio_actual ) + '\t\t' + str( 1 / ratio_actual ) + '\n')
    if the_shifts == high_shifts:
        sys.stdout.write('\n')

sys.stdout.write("\nSecond, we compute the higher frequencies errors\n");
sys.stdout.write("Ratio between the ratio for N shifts and the ratio for " + str( high_shifts) + " bits\n");
sys.stdout.write("shifts\tratio with approx.\t1/ratio with approx.\tratio without\t\tfrequency error\n");

ratio_actual_low =  1 / ( pow( 2, high_shifts ) *( 2 * math.pi))
ratio_theoritical_low = -( math.log( 1 - 1 / pow( 2, high_shifts ))) /( 2 * math.pi )

for the_shifts in range( 3, high_shifts + 1):
    ratio_actual =  1 / ( pow( 2, the_shifts ) *( 2 * math.pi))
    ratio_theoritical = -( math.log( 1 - 1 / pow( 2, the_shifts ))) /( 2 * math.pi )
    sys.stdout.write( str( the_shifts ) + '\t' + str( ratio_theoritical / ratio_theoritical_low ) + '\t' + str( ratio_theoritical_low / ratio_theoritical ) + '\t' + str( ratio_actual / ratio_actual_low ) + '\terror: ' + str( 1000 * ( 1 - ratio_theoritical / ( ratio_theoritical_low * pow( 2, high_shifts - the_shifts )))) + ' o/oo\n')
