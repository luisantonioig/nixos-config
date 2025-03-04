{ config, pkgs, lib, ... }:

{
  # Habilitar Xorg y Wayland
  services.xserver.enable = true;

  # Configurar GPUs (Intel + NVIDIA)
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.prime = {
    sync.enable = true;
    intelBusId = "PCI:0:2:0";     # Intel Iris Xe (i915)
    nvidiaBusId = "PCI:1:0:0";    # NVIDIA RTX 4090 Laptop
  };
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.550;

  # Activar soporte gráfico (OpenGL, Vulkan, etc.)
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver  # Aceleración para Intel Iris Xe
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      vulkan-tools
    ];
  };

  # Configurar SSD NVMe para optimizar rendimiento
  services.fstrim.enable = true;
  boot.kernelParams = [
    "nvidia_drm.modeset=1"
    "i915.enable_fbc=1"
    "i915.enable_guc=3"
    "nvme.noacpi=1"
  ];

  # Configurar WiFi (Intel AX211)
  hardware.enableRedistributableFirmware = true;
  networking.wireless.enable = true;

  # Configurar sonido (Intel cAVS)
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  hardware.pulseaudio.support32Bit = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Configurar touchpad y TrackPoint
  services.libinput.enable = true;

  # Activar lector de tarjetas (RTS5261)
  hardware.rts5227.enable = true;

  # Configurar Thunderbolt 4
  services.hardware.bolt.enable = true;

  # Optimizar batería y sensores térmicos
  powerManagement.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;

  # Asegurar que el sistema use el kernel 6.11 (igual que en Ubuntu)
  boot.kernelPackages = pkgs.linuxPackages_6_11;
}
