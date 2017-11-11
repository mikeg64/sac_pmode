#!/bin/bash
#script to gather data from scattered config files
#$ -l h_rt=08:00:00
#$ -cwd
#$ -l arch=intel*
#$ -l mem=6G
#$ -N gathloop_bv20G


#source /etc/profile.d/modules.sh add mpi/intel/openmpi/1.8.3
#module add mpi/intel/openmpi/1.8.3



echo "Run gather program on"
echo $HOSTNAME

#model="p5b0_1_bv10g"
#model="p6b0_0_bv10g"
model="p5b2_2_bv200g"


#n=$1
n=9
i=1
j=1
g=1 # step


    while [ $i -le $n  ]
    do

		data/distribution -s=$i /fastdata/cs1mkg/smaug/${model}/3D_spic_128_bv200g_np160101.out /fastdata/cs1mkg/smaug/${model}/3D_spic_128_bv200g.out
	       mv /fastdata/cs1mkg/smaug/${model}/3D_spic_128_bv200g.out /fastdata/cs1mkg/smaug/${model}/3D_spic_128_bv200g_$j.out
              i=`expr $i + $g`
              j=`expr $j + $g`
              echo $i
              echo "/fastdata/cs1mkg/smaug/${model}/3D_spic_128_bv200g_$j.out"


    done
