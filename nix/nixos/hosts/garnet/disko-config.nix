{
  disko.devices = {
    disk = {
      main = {
        # By id rather than /dev/sda, so installing with USB media attached
        # can't shift the sd* names around.
        device = "/dev/disk/by-id/ata-CT240BX500SSD1_1911E1783EE2";
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
