{
  disko.devices = {
    disk = {
      main = {
        # By id rather than /dev/sda, so installing with USB media attached
        # can't shift the sd* names around.
        device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_M.2_250GB_S5GFNG0M905357B";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              label = "boot";
              type = "EF00";
              size = "1G";
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
