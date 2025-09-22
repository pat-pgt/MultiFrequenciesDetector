import math

""" This project rotates the vector by +1 or -1, and never by 0
Then, the computed X and Y values always grow by 1/cos(arctg(2**N))
  and never keep their amplitude.
By this way, all the coefficients can be grouped.
The project need two Cordic set of stages (with a low pass filter in between).\n
The value itself is or is not relevant as in some case i) only the phase is required
ii) only the scale is required iii) this gain can be considered in the downstream projects.\n
The verification should insure, at any stage, the input value is enough small
to not overflow.
""
This software computes the global gain, for the combinations of preprocessing choices
for the first and the second set of stages.\n
It computes the maximum values as well.\n
For more information, see in the code.\n
TODO find a relevant ceil rounding to consider stages over tan 1/2**25\n
TODO get the reg_size from the code
"""

reg_size=32


print("Some mathematics around the Cordic algorithm, see the documentation and the comments in the code")

# This part computes, for 1 and 0.5 tan, the grow stage by stage:
# * for one set of Cordic
# * for one set of Cordic following another set with tan 1 
# * for one set of Cordic following another set with tan 0.5

max_1 = 1
max_05 = 1
for ind in range(0, 25):
    if ind > 0:
        max_1 *= math.cos(math.atan(1.0/float(2**(ind))))
    max_05 *= math.cos(math.atan(1.0/float(2**(ind))))
#    print("{:.16f}".format(max_1) + '\t' + "{:.16f}".format(max_05))

for start_val in range(0, 2):
    a=range(start_val, 25)
    print("------------------------------------------------------------------------")
    if start_val == 0:
        print("First Cordic stage with N = 0, angle PI/4")
#  May be used for Y to 0, in which the preprocessed vector is in the [ 0 PI/2 ]
    else:
        print("First Cordic stage with N = 1, angle around PI/8")
# Used for Z to 0, in which the preprocessed vector is in the [ -PI/4 PI/4 ]
# May be used for Y to 0, in which the preprocessed vector is in the [ 0 PI/2 ]
    print("------------------------------------------------------------------------")
    cumul_cos = 1.0
    print( "N\t1/R=2**N\t\tAngle\t\t\t1/cos(atg(R))\t\tcumulative\t\tsee code\t\tsee code")
    for ind in a:
        the_shift = 1.0 / float(2**ind)
        the_angle = math.atan(the_shift)
        val_cos = math.cos(the_angle)
        cumul_cos *= val_cos
        if ind < 5 or ind > 20:
            print(str(ind) + ":\t" + "{:.16f}".format(the_shift) + "\t" + str(the_angle) + "\t",end='')
            print("{:.16f}".format(1 / val_cos) + '\t' + "{:.16f}".format(1 / cumul_cos) + "\t",end='')
            print("{:.16f}".format(1.0/(cumul_cos*max_1)) + '\t',end='')
            print ( "{:.16f}".format(1.0/(cumul_cos*max_05)))
        
print("-------------------------------------------------------")
print("Maximum values to prevent overflow in the Cordic stages")
print("and their grow from 0.5")
print("-------------------------------------------------------")
print("N\tMax input\t\tMax input\t\tGain prev. set")
print("\ttheory\t\t\tcomputed\t\twith input=0.5")
#A worst case is used, taking 0.5 rather than 0.499..99
#then if it passed for 0.5, it passes in the project
# For N=0, the maximum input is (0.)10101...
max_coridc_stage_val = 0
for ind in range(1, reg_size, 2):
    max_coridc_stage_val += 1 / 2 ** ind
print ( "N=0\t" + "{:.16f}".format(1.0/(1.0+1.0/2.0)) + '\t' + str(max_coridc_stage_val)+'\t'+str(max_coridc_stage_val/0.5))
# For N=1, the maximum input is (0.)11001100...
max_coridc_stage_val = 0
for ind in range(2, reg_size, 4):
    max_coridc_stage_val += 3 / 2 ** ind
print ( "N=1\t" + "{:.16f}".format(1.0/(1.0+1.0/4.0)) + '\t' + str(max_coridc_stage_val)+'\t'+str(max_coridc_stage_val/0.5))
# For N=2, the maximum input is (0.)111000111000...
max_coridc_stage_val = 0
for ind in range(3, reg_size, 8):
    max_coridc_stage_val += 7 / 2 ** ind
print ( "N=2\t" + "{:.16f}".format(1.0/(1.0+1.0/8.0)) + '\t' + str(max_coridc_stage_val)+'\t'+str(max_coridc_stage_val/0.5))
# For N=3, the maximum input is (0.)1111000011110000...
max_coridc_stage_val = 0
for ind in range(4, reg_size, 16):
    max_coridc_stage_val += 15 / 2 ** ind
print ( "N=3\t" + "{:.16f}".format(1.0/(1.0+1.0/16.0)) + '\t' + str(max_coridc_stage_val)+'\t'+str(max_coridc_stage_val/0.5))
