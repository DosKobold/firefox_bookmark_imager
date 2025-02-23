#!/bin/bash

# This bash script images the content of your given root folder on you local 
# filesystem. For every folder there is a directory created. For every object
# there is a text file created. All slashed are replaced with an underscore.

# Loop over each line
while IFS= read -a oL ; do {
    
    # If the line is a folder
    if [[ $oL == ./* ]] ;
    then
        oL="$(echo $oL | cut -c3- | tr / _)" 
        mkdir "${oL}"
        cd "${oL}"

    # If the line is empty
    elif [[ $oL == "" ]] ;
    then
        cd ..

    # If the line is a object
    else
        touch "$(echo $oL | tr / _)"
    fi
};
done < <(./bin/main places.sqlite Root_Folder);
unset oL;
