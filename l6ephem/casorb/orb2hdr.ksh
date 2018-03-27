#!/bin/ksh

#sFile=${INST_ETC}/OrbitNames.list
#sFile=/opt/project/cassini/data/orbit/OrbitNames.list
sFile=OrbitNames.list

echo "/* Generated via ord2hdr.ksh "
echo " *"
echo " *   on:   $(date +%Y-%m-%dT%H:%M:%S)"
echo " *   by:   $(whoami)"
echo " *   from: ${sFile}"
echo " */"
echo
echo "typedef struct cas_orbit_name_tag{"
echo "   const char *sName,*sScetBeg,*sScetEnd;"
echo "}CasOrbitName;"
echo 
echo "CasOrbitName CasOrbit[]={"
cat $sFile |
gawk '
{
gsub(/"/," ")
gsub(/,/," ")
printf "{\"%s\",\"%s\",\"%s\"},\n",$1,$2,$3
}
'

echo "{\"\",\"\",\"\"}"
echo "};"
echo

