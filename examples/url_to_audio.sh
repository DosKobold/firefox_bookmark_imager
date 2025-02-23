#!/bin/bash

# Music downloader
# ----------------
# This bash script images the content of your given music root folder on you local 
# filesystem. For every folder there is a directory created. For every object
# there is the audio file downloaded. It is only tested with youtube links. All 
# slashes are replaced with underscores.

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
        yt-dlp -f 251 "${oL}"
    fi
};
done < <(./bin/main places.sqlite Music_Folder);
unset oL;
