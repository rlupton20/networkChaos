/* C code for controlling TUN/TAP devices */
#ifndef _HSK_TUNTAP_H
#define _HSK_TUNTAP_H

/* We bind constants to functions, to make them easier
   to interface with Haskell */
int tun();
int tap();
int no_pi();
int ifnamsiz();

int getTunTap(char *name, int flags);

#endif
