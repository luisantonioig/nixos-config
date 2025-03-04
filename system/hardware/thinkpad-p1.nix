{ ... }:

{
  services.xserver.enable = true;

  # Configurar GPUs (Intel + NVIDIA)
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.prime = {
    sync.enable = true;
    intelBusId = "PCI:0:2:0";     # Intel Iris Xe (i915)
    nvidiaBusId = "PCI:1:0:0";    # NVIDIA RTX 4090 Laptop
  };
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.latest;
  hardware.nvidia.open = false;

  boot.extraModprobeConfig = "options i915 enable_guc=3";

  # Activar Vulkan y OpenGL para ambas GPUs
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver  # Para Intel iGPU
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      vulkan-tools
    ];
  };

  # Parámetros del kernel para mejorar compatibilidad con Intel + NVIDIA
  boot.kernelParams = [
    "nvidia_drm.modeset=1"
    "i915.enable_fbc=1"
    "i915.enable_guc=3"
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "thunderbolt" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
}
