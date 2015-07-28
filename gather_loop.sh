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


#n=$1
n=70
i=4  
g=1 # step


    while [ $i -le $n  ]
    do

		data/distribution -s=$i /fastdata/cs1mkg/sac/p5b0_1_bv20g/3D_spic_128_bv20G_np080101.out /fastdata/cs1mkg/sac/p5b0_1_bv20g/3D_spic_128_bv20G.out
	       mv /fastdata/cs1mkg/sac/p5b0_1_bv20g/3D_spic_128_bv20G.out /fastdata/cs1mkg/sac/p5b0_1_bv20g/3D_spic_128_bv20G_$i.out
              i=`expr $i + $g`
              echo $i
              echo "/fastdata/cs1mkg/sac/p5b0_1_bv20g/3D_spic_128_bv20G_$i.out"


    done