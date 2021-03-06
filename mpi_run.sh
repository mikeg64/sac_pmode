#!/bin/bash
#$ -l h_rt=120:00:00

#$ -cwd
#$ -l arch=intel*
#$ -P mhd
#$ -l mem=6G
#$ -pe openmpi-ib 16
#$ -N S3D_bv20G


#source /etc/profile.d/modules.sh add mpi/intel/openmpi/1.8.3
module add mpi/intel/openmpi/1.8.3



echo "SAC will run on the following nodes"
cat $PE_HOSTFILE
echo Starting vac now.

cd src
cp vacusr_180_0_0_v10g.f vacusr.f

make vac -f Makefile_impi

cd ..
cp par/vac_180_0_0_v10g.par vac.par

/usr/local/mpi/intel/openmpi/1.8.3/bin/mpirun vac

#echo "Starting conversion to h5"
#/usr/local/mpi/pgi/openmpi/1.6.4/bin/mpirun convh5

#/home/smq11sjm/.local/bin/pushover "Job Complete"
