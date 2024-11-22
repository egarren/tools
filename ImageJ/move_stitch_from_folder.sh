dir1="$HOME/Documents/5. DMS/Carroll Lab/2022 Cxcl13_Mb1 PDL1/2022.03.01 IF FoxP3-GFP/raw/"
dir1=/Volumes/NO\ NAME/20221210_IF_various/needtostitch
dir1=/Volumes/WDeasystore/Carroll\ Lab\ Temp/Confocal/20241024_pdl1/stitch/
dir1=/Volumes/WDelements/20231219_IF_TFR/stitch/
ls "$dir1"
name="Stitch_A01_G001.oir"
name_2="Stitch_A01_G002.oir"
name_3="Stitch_A01_G003.oir"
name_4="Stitch_A01_G004.oir"
name_5="Stitch_A01_G005.oir"
name_6="Stitch_A01_G006.oir"
name_7="Stitch_A01_G007.oir"
name_8="Stitch_A01_G008.oir"
name_9="Stitch_A01_G009.oir"
suffix="/"

for i in "$dir1"*/; do
  # echo "$i$name"
  # echo "$i"
  name1=${i#"$dir1"} #get name of folder
  name2=${name1%"$suffix"}
  # echo "$dir1${name2}.oir"
  # echo "${i#"$dir1"}.oir"
  # echo "$dir1${i#"$dir1/"}.oir"
  mv "$i$name" "$dir1${name2}.oir"
done


for i in "$dir1"*/; do
  # echo "$i$name"
  # echo "$i"
  name1=${i#"$dir1"} #get name of folder
  name2=${name1%"$suffix"}
  # echo "$dir1${name2}.oir"
  # echo "${i#"$dir1"}.oir"
  # echo "$dir1${i#"$dir1/"}.oir"
  if [ -e "$i$name" ]
  then
    mv "$i$name" "$dir1${name2}.oir"
  fi
  if [ -e "$i$name_2" ]
  then
    mv "$i$name_2" "$dir1${name2}_2.oir"
  fi
  if [ -e "$i$name_3" ]
  then
    mv "$i$name_3" "$dir1${name2}_3.oir"
  fi
  if [ -e "$i$name_4" ]
  then
    mv "$i$name_4" "$dir1${name2}_4.oir"
  fi
  if [ -e "$i$name_5" ]
  then
    mv "$i$name_5" "$dir1${name2}_5.oir"
  fi
  if [ -e "$i$name_6" ]
  then
    mv "$i$name_6" "$dir1${name2}_6.oir"
  fi
  if [ -e "$i$name_7" ]
  then
    mv "$i$name_7" "$dir1${name2}_7.oir"
  fi
    if [ -e "$i$name_8" ]
  then
    mv "$i$name_8" "$dir1${name2}_8.oir"
  fi
  if [ -e "$i$name_9" ]
  then
    mv "$i$name_9" "$dir1${name2}_9.oir"
  fi
  
done


