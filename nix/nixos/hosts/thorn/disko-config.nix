{
  disko.devices = {
    disk = {
      main = {
        device = "/dev/vda";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            grub = {
              size = "1M";
              type = "EF02"; # for grub MBR
            };
            boot = {
              size = "1G";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/boot";
              };
            };
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted-main";
                passwordFile = "/tmp/luks-key.txt";
                content = {
                  type = "lvm_pv";
                  vg = "vg-main";
                };
              };
            };
          };
        };
      };

      storage = {
        device = "/dev/vdb";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted-storage";
                passwordFile = "/tmp/luks-key.txt";
                content = {
                  type = "lvm_pv";
                  vg = "vg-storage";
                };
              };
            };
          };
        };
      };
    };

    lvm_vg = {
      vg-main = {
        type = "lvm_vg";

        lvs = {
          root = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
        };
      };

      vg-storage = {
        type = "lvm_vg";

        lvs = {
          data = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/mnt/storage";
            };
          };
        };
      };
    };
  };
}
