#!/bin/sh

# we should email this stuff off:

cat <<EOF | sed 's/^\./ ./' | /usr/bin/mail -s "clc system failure during rebuilding" root -e
Hello

This is the clc-send-command reporting a system failure.

I tried to contact  the clc-build-daemon that should be listed
in /etc/inetd.conf as a daemon for port 8990/tcp, but this failed.

This indicates a general system failure of the clc build system,
I will now execute some commands that might help solve this problem.

As the package did not declare a dependency on all those utilities some
might fail. This is no problem as other queries should return redundant 
information. We took care in protecting your privicy but understand
that you might be unwilling to forward us this email in case it reveals
sensitive information about your system. So if you doubt it is a good
idea to send us this email, please don't. 

Of course if you do we might be able to help you :-)

Thanks in advance, the clc maintainers.

Start of report.

ls -l /etc/inetd.conf:
`ls -l /etc/inetd.conf 2>&1`

dpkg -l netkit-inetd
`dpkg -l netkit-inetd 2>&1`

grep 8990 /etc/inetd.conf:
`grep 8990 /etc/inetd.conf 2>&1`

netstat -nvlpt | grep 8990:
`netstat -nvlpt | grep 8990 2>&1`

cat /proc/net/tcp:
`cat /proc/net/tcp | head -n 1 2>&1`
`cat /proc/net/tcp | grep -i 231e 2>&1`

lsof -i :8990 -n
`lsof -i :8990 -n 2>&1`

grep 8990 /etc/inetd.conf | cut -f 6 | xargs ls -l
`grep 8990 /etc/inetd.conf | cut -f 6 | xargs ls -l 2>&1`

grep 8990 /etc/inetd.conf | cut -f 6 | xargs stat
`grep 8990 /etc/inetd.conf | cut -f 6 | xargs stat 2>&1`

ls -l /usr/sbin/clc-build-daemon
`ls -l /usr/sbin/clc-build-daemon 2>&1`

stat /usr/sbin/clc-build-daemon
`stat /usr/sbin/clc-build-daemon 2>&1`

md5sum /usr/sbin/clc-build-daemon
`md5sum /usr/sbin/clc-build-daemon 2>&1`

echo -e "QUIT\n" | nc -vvv 127.0.0.1 8990:
`echo -e "QUIT\n\n" | nc -vvv 127.0.0.1 8990 2>&1`

EOF
