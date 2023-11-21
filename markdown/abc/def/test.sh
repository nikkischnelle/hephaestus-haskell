#!/bin/bash

# Directory where all the JPEGs will be saved
output_directory="output_images"

# Loop through all video files in the current directory with .mkv extension
for video_file in *.mkv; do
  # Define the output base name using parameter expansion
  base_name="${video_file%.*}"

  # Loop to capture frames every minute up to 10 minutes
  for ((minute = 1; minute <= 10; minute++)); do
    # Define the timestamp for the frame capture
    timestamp=$(printf "%02d:00" $minute)

    # Create a folder for the timestamp within the output directory
    timestamp_folder="$output_directory/${timestamp}"
    mkdir -p "$timestamp_folder"

    # Define the output JPEG file name
    output_file="${timestamp_folder}/${base_name}_minute_${minute}.jpg"

    echo "Capturing frame at $timestamp from $video_file and saving as $output_file"

    # Use ffmpeg to extract the frame and save it as a JPEG file
    ffmpeg -ss "$timestamp" -i "$video_file" -frames:v 1 -q:v 2 "$output_file"

    # Check if the ffmpeg command was successful
    if [ $? -eq 0 ]; then
      echo "Frame capture successful"
    else
      echo "Frame capture failed"
    fi
  done
done
