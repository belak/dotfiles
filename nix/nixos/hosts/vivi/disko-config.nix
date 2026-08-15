{
  disko.devices = {
    disk = {
      main = {
        # By id rather than /dev/sda: this box has been reinstalled before with
        # USB media attached, which can shift the sd* names around.
        device = "/dev/disk/by-id/ata-WDC_WDS500G2B0B-00YS70_183794803606";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              # Explicit labels, so the generated fileSystems match the disk as
              # it is already partitioned rather than disko's derived
              # disk-main-* names.
              label = "boot";
              type = "EF00";
              size = "512M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              label = "root";
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
