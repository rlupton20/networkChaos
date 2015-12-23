#include "tuntap.h"

#include <sys/socket.h>
#include <linux/if.h>
#include <linux/if_tun.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/ioctl.h>

/* Lets define some useful flags (functionally) */
int tun(){ return IFF_TUN; }
int tap(){ return IFF_TAP; }
int no_pi(){ return IFF_NO_PI; }
int ifnamsiz(){ return IFNAMSIZ; }

/* The following function will attempt to create/attach
   to a TAP/TUN device */
int getTunTap(char *name, int flags){
  /* ifr contains the parameters for the ioctl */
  struct ifreq req;

  /* We open the device and get a file descriptor */
  int fd, err;

  /* /dev/net/tun is a clone device */
  char *device = "/dev/net/tun";

  /* Try and open the device, noting a negative
     file descriptor (once returned) indicate
     an error. */  
  if ( (fd = open(device, O_RDWR)) < 0 ) {
      return fd;
  }

  /* Now lets create the structure required for 
     ioctl, first zeroing its contents */
  memset(&req, 0, sizeof(req));

  /* Now lets populate the structure --- first
     the name (if supplied) and then the flags. */
  if ( *name ) {
    strncpy(req.ifr_name, name, IFNAMSIZ);
  }
  
  req.ifr_flags = flags;

  /* Now lets try and ioctl */
  if ( ( err = ioctl(fd, TUNSETIFF, (void *) &req) ) < 0) {
    close(fd);
    return err;
  }

  /* Copy the name that was actually used for the device
     into the name string (the ioctl will write it to the
     request structure we passed it) */
  strcpy(name, req.ifr_name);
  return fd;
}
