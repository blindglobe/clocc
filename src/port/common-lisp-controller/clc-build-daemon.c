/* clc-build-daemon  -*- Mode:C -*-

   written by Peter Van Eynde, copyright 2002

   license: GPL v2

*/

/* yeah, I will accept patches to remove this :-) */
#define _GNU_SOURCE
#include <ftw.h>
#include <pwd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>
#include <grp.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <syslog.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <dirent.h>
#include <netinet/in.h>

/* This daemon implements the common-lisp-controller build daemon.
It listens (via inetd) on a port for commands. It says hello with
100 hello

Possible commands are:

"QUIT" to exit
"SHOW-OUTPUT" to show the compilation output
"HIDE-OUTPUT" to hide the compilation output
"RECOMPILE <package> <common lisp implementation>" will recompile the package
  for the given implementation.
"REMOVE <package> <common lisp implementation" will remove the package for
  the given implementation

both names must consist only of the characters: [a-zA-Z0-9-]
Possibile replies are:

100 Hello
250 OK
251 Compilation OK
252 Removal OK
500 Syntax error
501 Build error
540 Cannot remove: not yet compiled
550 Cannot build: already compiled
200 Showing build output
201 Hiding build output
220 Bye
300 start build output
310 build output ....
331 end build output

*/

/* report a general timeout and quit */
void timeout(int dummy)
{
  syslog(LOG_ERR,"general timeout");
  exit(0);
}

/* report a pipe error and quit */
void pipeerror(int dummy)
{
  syslog(LOG_ERR,"general pipe error");
  exit(0);
}

#define MAX_LINES 30
char *line=NULL; /* read line */
char *lines[MAX_LINES+1];
int  current_line=0;

int linelength=MAX_LINES; /* length of the line read */

int showoutput=1; /* do we show the compilation output? */

/* characters allowed in the package and compiler names: */
char allowedcharacters[]="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-";
/* delimiters: */
char delim[]=" \t\n\r";

/* report a error and die */
void reporterror(const char *where)
{
  int old_errno;
  
  old_errno=errno;
  syslog(LOG_ERR,"error: %s: %s",
         where,
         strerror(errno));
  errno=old_errno;
  perror(where);
  exit(1);
}

/* reads line from stdin */
void readaline (void)
{
  ssize_t length;
  size_t max_length;

  max_length = 4096;
  if (line == (char *) NULL)
    {
      line = (char *) malloc( sizeof(char)*4097);
      if (line == (char *) NULL)
        reporterror("Could not allocate space for the line");
    }

  length = getline( &line, &max_length, stdin );

  if (length == -1)
    reporterror("eof on read");

  if (max_length != 4096)
    reporterror("Line read was too long, possible attack?");

  return;
}

/* delete the files */
int delete_stuff(const char *filename, const struct stat *stat, int flag)
{
  /*   just a normal file? */
  if ( (flag == FTW_F) ||
       /* or a symbolic link? */
       (flag == FTW_SL))
    {
      if (unlink(filename) != 0)
        {
          char command[4097];

          snprintf(command,4096,"While deleting the file: %s with %i: %i %i",filename,flag,(flag & FTW_F),(flag & FTW_SL));
          command[4096]=(char)0;
          reporterror(command);
          return 1;
        }
    }
  return 0;
}

/* delete the directory */
int delete_directories(const char *filename, const struct stat *stat, int flag)
{
  /* just a directory? */
  if (flag == FTW_D)
    {
      if ((rmdir(filename) != 0) &&
          (errno != ENOTEMPTY))
        {
          char command[4097];

          snprintf(command,4096,"While deleting the directory: %s",filename);
          command[4096]=(char)0;
          reporterror(command);
          return 1;
        }
    }
  return 0;
}

int probe_directory(char *pathname)
{
  /* returns 1 if directory exists */
  DIR *dir;

  dir = opendir(pathname);
  if (dir == (DIR *) NULL)
    return 0;
  else
    {
      if (closedir(dir) != 0)
        reporterror("Could not close directory stream!?");
      return 1;
    }
}

void  nuke_package(char *package,char *compiler)
{
  char command[4097];


  snprintf(command,4096,"/usr/lib/common-lisp/%s/%s",
           compiler,package);
  command[4096]=(char)0;
  ftw(command, &delete_stuff, 150);
  while (probe_directory(command)) {
      ftw(command, &delete_directories, 150);
   }
}

int main(int argc, char *argv[])
{
  int opt;
  int inputfd, outputfd; /* the input and output stream fd's */
  struct sockaddr_in peer;
  socklen_t socklen = sizeof(peer);

  openlog("clc-build-daemon",LOG_ODELAY,LOG_DAEMON);

  /* get the lock on /etc/lisp-config.lisp */
  {
    struct flock flock;
    int fd;

    if ((fd=open("/etc/lisp-config.lisp",O_RDWR)) == -1)
      reporterror("Cannot open /etc/lisp-config.lisp");

    flock.l_type = F_WRLCK;
    flock.l_whence=SEEK_SET;
    flock.l_start=0;
    flock.l_len=0;

    if (fcntl(fd, F_SETLKW, &flock) == -1)
      reporterror("Cannot lock /etc/lisp-config.lisp!");
  }


  for(inputfd=0;inputfd <= MAX_LINES;inputfd++)
    lines[inputfd]=NULL;

  /* timeout after 30 seconds */
  signal(SIGALRM, timeout);
  alarm(90);

  signal(SIGPIPE, pipeerror);

  syslog(LOG_NOTICE,"started");


  inputfd = fileno( stdin );
  outputfd = fileno( stdout );
  if (inputfd == -1)
    reporterror("Input stream not a stream?");

  /* tell us about errors */
  fcntl(inputfd,F_SETOWN);

  if (getpeername(inputfd, &peer, &socklen) != 0)
    reporterror("During getpeername");
  else
    {
      int source_ip;

      if (peer.sin_family != AF_INET)
        reporterror("sa_family is not AF_INET?");

      source_ip = ntohl(peer.sin_addr.s_addr);

      if ( (source_ip & 0xFF000000) != (127 << 24))
        {
          syslog(LOG_ERR,"Source is not localhost! %x",source_ip);
          exit(1);
        }
    }

  opt=1;
  if (setsockopt(inputfd, 6, SO_KEEPALIVE, &opt, sizeof(opt)) != 0)
    reporterror("Could not set option for inputfd");

  if (setvbuf(stdout, (char *) NULL, _IOLBF, BUFSIZ) != 0)
    reporterror("Could not make stdout line buffered");

  if (setvbuf(stderr, (char *) NULL, _IOLBF, BUFSIZ) != 0)
    reporterror("Could not make stderr line buffered");

  printf("100 common-lisp-controller build daemon version 1.0 at you service\n");

  /* main loop */
  for(;;)
    {
      char *command;

      alarm(30);
      readaline();
      alarm(0);

      command = strtok(line,delim);

      if ( command == (char *) NULL)
        {
          if (feof(stdin))
            {
              syslog(LOG_NOTICE,"client left us unexpect.");
              shutdown(inputfd,SHUT_RDWR);
              close(inputfd);
              close(outputfd);
              exit(0);
            }
        }
      else if (strcmp("QUIT",command) == 0)
        {
          printf("220 BYE\n");
          fflush(stdout);
          
          shutdown(inputfd,SHUT_WR);
          close(inputfd);
          close(outputfd);
          exit(0);
        }
      else if (strcmp("SHOW-OUTPUT",command) == 0)
        {
          showoutput=1;
          printf("200 SHOWING BUILD OUTPUT\n");
        }
      else if (strcmp("HIDE-OUTPUT",command) == 0)
        {
          showoutput=0;
          printf("201 HIDING BUILD OUTPUT\n");
        }
      else if (strcmp("REMOVE",command) == 0)
        {
          char *package,*compiler,*check;

          package = strtok(NULL,delim);
          compiler = strtok(NULL,delim);
          check = strtok(NULL,delim);

          if ((package == (char *) NULL) ||
              (compiler == (char *) NULL) ||
              (check != (char *) NULL) ||
              ( strlen(package) != strspn(package,allowedcharacters)) ||
              ( strlen(compiler) != strspn(compiler,allowedcharacters)))
            {
              printf("500 Syntax error in REMOVAL command: package: %s compiler: %s check: %s tests: %i %i %i %i %i\n",
                     package, compiler,check,
                     (package == (char *) NULL),
                     (compiler == (char *) NULL),
                     (check != (char *) NULL),
                     ( strlen(package) != strspn(package,allowedcharacters)),
                     ( strlen(compiler) != strspn(compiler,allowedcharacters)));
            }
          else
            {
              char command[4097];

              snprintf(command,4096,"/usr/lib/common-lisp/%s/%s",
                       compiler,package);
              command[4096]=(char)0;
              if (! probe_directory(command) )
                {
                  syslog(LOG_ERR,"Cannot remove  package %s for compiler %s",package,compiler);
                  printf("540 Cannot remove package %s for compiler %s\n", package, compiler);
                }
              else
                {
                  printf("250 removing package %s for compiler %s\n", package, compiler);
                  syslog(LOG_NOTICE,"Removing package %s for compiler %s",package,compiler);

                  nuke_package(package,compiler);
                }

              printf("252 DONE\n");
            }
        }
      else if (strcmp("RECOMPILE",command) == 0)
        {
          char *package,*compiler,*check;

          package = strtok(NULL,delim);
          compiler = strtok(NULL,delim);
          check = strtok(NULL,delim);

          if ((package == (char *) NULL) ||
              (compiler == (char *) NULL) ||
              (check != (char *) NULL) ||
              ( strlen(package) != strspn(package,allowedcharacters)) ||
              ( strlen(compiler) != strspn(compiler,allowedcharacters)))
            {
              printf("500 Syntax error in RECOMPILE command: package: %s compiler: %s check: %s tests: %i %i %i %i %i\n",
                     package, compiler,check,
                     (package == (char *) NULL),
                     (compiler == (char *) NULL),
                     (check != (char *) NULL),
                     ( strlen(package) != strspn(package,allowedcharacters)),
                     ( strlen(compiler) != strspn(compiler,allowedcharacters)));
            }
          else
            {
              pid_t child;
              int descriptors[2];
              char directory[4097];


              snprintf(directory,4096,"/usr/lib/common-lisp/%s/%s",
                       compiler,package);
              directory[4096]=(char)0;

              if (probe_directory(directory))
                {
                  printf("550 package %s for compiler %s already compiled!\n", package, compiler);
                }
              else
                {
                  printf("250 recompiling package %s for compiler %s with %i\n", package, compiler,showoutput);
                  syslog(LOG_NOTICE,"Recompiling package %s for compiler %s",package,compiler);

                  if (pipe(descriptors) != 0)
                    reporterror("while creating pipe");

                  /* [0] is for reading, [1] is for writing */
                  if ( (child = fork()) == 0)
                    {
                      struct passwd *login_data;

                      close(descriptors[0]);
                      dup2(descriptors[1],STDOUT_FILENO);
                      dup2(descriptors[1],STDERR_FILENO);
                      close(descriptors[1]);
                      
                      if (freopen("/dev/null","r+",stdin) == NULL)
                        reporterror("reopen stdin");

                      if ( mkdir(directory, S_IREAD | S_IWRITE | S_IEXEC | S_IXGRP | S_IRGRP | S_IROTH | S_IXOTH) != 0)
                        {
                          char command[4097];

                          snprintf(command,4096,"while creating directory: /usr/lib/common-lisp/%s/%s",
                                   compiler,package);
                          command[4096]=(char)0;
                          reporterror(command);
                        }

                      login_data = getpwnam("cl-builder");

                      if (login_data == NULL)
                        reporterror("Could not know who is cl-builder");

                      if ( chown(directory, login_data->pw_uid, login_data->pw_gid) != 0)
                        {
                          char command[4097];

                          snprintf(command,4096,"while changing owner of directory: /usr/lib/common-lisp/%s/%s",
                                   compiler,package);
                          command[4096]=(char)0;
                          reporterror(command);
                        }

                      if (setgid(login_data->pw_gid) != 0)
                        reporterror("could not become cl-builder group");

                      if (setgroups(0,NULL) != 0)
                        reporterror("Could not give up groups");

                      if (setuid(login_data->pw_uid) != 0)
                        reporterror("could not become  cl-builder");

                      if (setsid() == -1)
                        reporterror("could not create a session");
                      else
                        {
                          char command[4097];
                          char script[4097];
                          char *argv[4];
                          char *env[5];

                          snprintf(command,4096,
                                   "/usr/lib/common-lisp/bin/%s.sh",
                                   compiler);
                          snprintf(script,4096,
                                   "%s.sh",
                                   compiler);
                          argv[0] = script;
                          argv[1] = "rebuild";
                          argv[2] = package;
                          argv[3] = (char *) NULL;

                          env[0] = "PATH=/bin:/usr/bin:/usr/local/bin:";
                          env[1] = "HOME=/tmp";
                          env[2] = "USER=cl-builder";
                          env[3] = "TERM=vt100";
                          env[4] = (char *) NULL;

                          if (execve(command,argv,env) == -1)
                            reporterror("execve failed");
                          else
                            reporterror("cannot return from exec!");
                          exit(255);
                        }
                    }
                  else
                    {
                      int status;
                      pid_t pid;
                      FILE *child;

                      close(descriptors[1]);
                      
                      child = fdopen(descriptors[0], "r");
                      if (child == (FILE *) NULL)
                        reporterror("while opening child stream");
                      
                      for(;;)
                        {
                          size_t length;
                          ssize_t read;
                          
                          length = 0;
                          
                          current_line++;
                          if (current_line > MAX_LINES)
                            current_line = 0;
                          
                          read = getline(&(lines[current_line]), &length, child);
                          
                          if (read <= 0)
                            {
                              if (feof(child))
                                {
                                  free(lines[current_line]);
                                  lines[current_line] = NULL;
                                  break;
                                }
                              else
                                reporterror("while reading from the child pipe");
                            }
                          if (showoutput == 1)
                            printf("310 %s",lines[current_line]);
                        }

                      pid = wait(&status);

                      if ( WEXITSTATUS(status) != 0)
                        {
                          FILE *mail_pipe;

                          nuke_package(package,compiler);

                          printf("501 Compilation failed with code %i\n",
                                 WEXITSTATUS(status));
			  syslog(LOG_NOTICE,"Recompilation of package %s for compiler %s failed",package,compiler);
                          /* fork yourself and drop priv. to send email */
                          if ( (fork())  == 0)
                            {
                              struct passwd *login_data;

                              login_data = getpwnam("cl-builder");
                              
                              if (login_data == NULL)
                                reporterror("Could not know who is cl-builder");
                              
                              if (setgid(login_data->pw_gid) != 0)
                                reporterror("could not become cl-builder group");
                              
                              if (setgroups(0,NULL) != 0)
                                reporterror("Could not give up groups");
                              
                              if (setuid(login_data->pw_uid) != 0)
                                reporterror("could not become cl-builder");

                              if (setsid() == -1)
                                reporterror("could not create a session");

                               mail_pipe = popen("/usr/bin/mail -s \"clc build failure\" root -e ","w");

                              if (mail_pipe == NULL)
                                reporterror("Could not open a pipe to /usr/bin/mail");
                              else
                                {
                                  int line_offset;
                                  
                                  fprintf(mail_pipe,"
Hello

This is the clc-build-daemon reporting a build failure.


While recompiling package %s for compiler %s I got the error code %i
If you want to retry the compilation and possibly report this as a bug
then please read /usr/share/doc/common-lisp-controller/REPORTING-BUGS

The command to retry the recompilation is:
clc-send-command --verbose remove %s %s ; echo returns: $?
clc-send-command --verbose recompile %s %s ; echo returns: $?

After this message I will append the last few lines of output.

Thanks for your attention\n",
                                          package,compiler,WEXITSTATUS(status),
                                          package,compiler,
                                          package,compiler);
                                  fflush(mail_pipe);
                                  for(line_offset=0;line_offset <= MAX_LINES;line_offset++)
                                    {
				      char *line;

				      line=lines[(current_line+1+line_offset) % MAX_LINES];
                                      
				      if (line != (char *) NULL)
                                        fprintf(mail_pipe,"%s", line);
                                    }
                                  fflush(mail_pipe);
                                  pclose(mail_pipe);
                                }
                              exit(0);
                            }
                          else
                            printf("251 DONE\n");
                        }
                      else
                      if ( WIFEXITED(status) != 0)
                        printf("251 DONE\n");
                    }
                }
            }
        }
      else
        {
          printf("500 Syntax error (unknown command)\n");
        }
    }

  closelog();
  exit(0);
}
