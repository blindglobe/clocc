all: clc-build-daemon clc-send-command

clc-send-command: clc-send-command.c
	gcc -o clc-send-command clc-send-command.c -O2 

clc-build-daemon: clc-build-daemon.c
	gcc -o clc-build-daemon clc-build-daemon.c -O2 

clean:
	rm -f *.o clc-send-command clc-build-daemon 2> /dev/null || true
