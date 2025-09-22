import math

""" This project helps to choose the relevant number of stages.
If the number of stages is too low, the angle or the Y do not converge to 0.
If the number of stages is too high, it consumes resources for nothing.

For large angles, multiple CW or CCW are required to reach 0.
The test checks an angle around 0 is reachable from the output of the reprocess around PI/4,
  according with the number of bits of the registers.

For the Z to 0, there is a phase noise due to the oscillations.
The verification is to check the residual oscillation is always lower than
  the step at low frequencies.
By this way it is guaranteed the processed angle always grows.

For the Y to 0 at small angles, or when the angle reaches around zero,
  there is an oscillation.
The test takes the next Cordic angle as a worst case.
"""

reg_size=32


print("Some mathematics around the Cordic algorithm, see the documentation and the comments in the code")

print("-------------------------------------------------------------------")
print("What happened if all the rotations are CW or CCW?")
print("This is to prove 4 Cordic iterations are enough to move the angle or Y close to 0 (and oscillate around), starting from PI/4")
print("-------------------------------------------------------------------")
print("N\tcumul angle\t\t... in degrees\t\t... its cosine\t\t... its sine")
cumul_angle = 0
for ind in range(1, 30):
    cumul_angle += math.atan( 1.0 / float(2**ind))
    print( str(ind) + '\t' + str( cumul_angle ) + '\t' + str( cumul_angle * 360.0 / ( 2.0 * math.pi )) + '\t',end="")
    print( str( math.cos(cumul_angle )) + '\t' + str( math.sin(cumul_angle)))
    if cumul_angle > (19.0 * math.pi / 64.0):
        break
print("-------------------------------------------------------------------")
print("If the size of the registers is low, the preprocessing of the Y to 0")
print("  can left the vectors at angles greater than 45 degrees.")
print("  e.g. X=$3c Y=$3f truncated to X=$f Y=$f (sign bit is omitted)")
print("-------------------------------------------------------------------")
print("1+N\tangle radians%\tangle degrees")
for ind in range(2,9):
    the_X = 2**ind - 2
    the_Y = 2**ind - 1
    print( "1+" + str(ind) + '\t' + str( math.atan( the_Y/the_X )) + '\t' + str( 180.0 * math.atan( the_Y/the_X ) / math.pi))

print("-------------------------------------------------------------------")
print("Computation of the phase noise of the Z to 0")
print("for some ratio between the CLK frequency and the frequency to generate")
print("The result is given as a percent of the maximum angle error on the step angle")
print("Being under 100% guarantee the angle of the next step is always greater than the previous one")
print("-------------------------------------------------------------------")
print("N\t\tR=500\t\t\tR=1000\t\t\tR=1500\t\t\tR=2000")
for ind in range(3,15):
    the_error = math.atan(1/2.0**(ind ))
    print( str(ind), end="" )
    for ind2 in range( 500,2001,500):
        the_step = 2 * math.pi / ind2;
        print ('\t' + str( 100 * the_error / the_step ) + " %", end="")
    print()

print("-------------------------------------------------------------------")
print("Computation of the worst case of the precision of X according with the number of Cordic stages.")
print("It is Y is about 0.00..001 and the next Cordic stage runs CW, or -0.00..001 and a run CCW")
print("-------------------------------------------------------------------")
for ind in range(3,20,2):
    for ind2 in range(0,2):
        the_error = 1.0 - math.cos(math.atan(1/2.0**(ind + ind2 )))
        if ind2 == 0:
            print( str(ind + ind2 ) + '\t', end="" ) 
        else:
            print( "\t\t" + str(ind + ind2 ) + '\t', end="" ) 
        if the_error < 1.0e-9:
            print( str( the_error * 1.0e12) + " ppt", end="")
        elif the_error < 1.0e-6:
            print( str( the_error * 1.0e9) + " ppb", end="")
        elif the_error < 1.0e-3:
            print( str( the_error * 1.0e6) + " ppm", end="")
        else:
            print( str( the_error * 1.0e3) + " %. ", end="")
    print()
