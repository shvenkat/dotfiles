# Do not index removable media.
sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array-add "/Volumes"

# Load new settings before rebuilding the index
# killall mds > /dev/null 2>&1
# Make sure indexing is enabled for the main volume
sudo mdutil -i on / > /dev/null
# Rebuild the index from scratch
sudo mdutil -E / > /dev/null
