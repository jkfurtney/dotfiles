
Raspberry PI Time machine server setup

See:
http://www.administeria.com/raspberry-pi-time-machine-server/

http://www.andadapt.com/2012/09/06/raspberry-pi-raspbian-hfs-afp-and-time-machine/

http://garmoncheg.blogspot.com.au/2012/11/time-capsule-for-25.html

http://garmoncheg.blogspot.com/2012/11/raspberry-pi-first-steps-and-basic.html

$ sudo blkid

$ sudo apt-get install hfsplus hfsutils hfsprogs

$ sudo mkdir /mnt/TimeMachine

$ sudo chown -R pi /mnt/TimeMachine

$ sudo apt-get install avahi-daemon libavahi-client-dev libdb5.3-dev db-util db5.3-util libgcrypt11 libgcrypt11-dev

install netatalk 3.0.2 worked for me

$ ./configure --with-init-style=debian --with-zeroconf
$ make
$ sudo make install

$ sudo nano /usr/local/etc/afp.conf
$ sudo /etc/init.d/netatalk start
$ sudo /etc/init.d/avahi-daemon start

start on boot

$ cd /etc/init.d/
$ sudo update-rc.d netatalk defaults

On a mac:
Go > Connect:
afp://XXX.XXX.XXX.XXX

Drive should show-up in Time Machine.
