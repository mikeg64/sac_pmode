#!/bin/bash
#$ -l arch=intel*
#$ -l mem=12G
#$ -l rmem=12G
#$ -P mhd



##source /data/cs1mkg/tools/vapor/vapor-2.2.2/bin/vapor-setup.sh
module add apps/idl/8.4
export IDL_STARTUP=runconvert_spic
idl
