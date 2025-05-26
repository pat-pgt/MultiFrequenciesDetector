import sys
import math

# Validate some assumptions of the prefilter

# The approximation is: we are linearising
# Since the cut-off frequency is low against the input sampling rate,
#   the exponential is considered linear,
#   at least for the low octaves or for low number of notes per octave

# This software estimates the error

# The formula of the division at the cut off frequency is
# K = 1 - exp( -2.PI.<cutoff>/<samplingrate>)
 
# We are looking for the list of the 1/2**N
#   then we compute log(1-1/2**N)/2.PI
#   and log(1-1/2**N)/2.PI

for the_shifts in range( 2, 25 ):
    Kactual =  1 / ( pow( 2, the_shifts ) *( 2 * math.pi))
    Ktheoritical = -( math.log( 1 - 1 / pow( 2, the_shifts ))) /( 2 * math.pi )
    sys.stdout.write( str( the_shifts ) + '\t' + str( Ktheoritical ) + '\t' + str( 1 / Ktheoritical ) + '\t' + str( Kactual ) + '\terror: ' + str( 1000 * ( 1 - Kactual / Ktheoritical )) + ' per mille\n')
