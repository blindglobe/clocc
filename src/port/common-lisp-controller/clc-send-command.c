/* clc-send-command  -*- Mode:C -*-
 
   written by Peter Van Eynde, copyright 2002,2003

   license: GPL v2

*/

/* yeah, I will accept patches to remove this :-) */
#define _GNU_SOURCE
#include <ftw.h>
#include <stdio.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <netinet/in.h>
#include <unistd.h>
#include <grp.h>
#include <netdb.h>
#include <unistd.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <dirent.h>
#include <argp.h>

char *line=NULL; /* read line */
int linelength=0; /* length of the line read */

int showoutput=1; /* do we show the compilation output? */

/* report a error and die */
void reporterror(const char *where)
{
  int old_errno;

  old_errno=errno;
  fprintf(stderr,"\nerror: %s: %s\n",
         where,
         strerror(errno));
  exit(1);
}

void reportsystemerror(const char *where)
{
  int old_errno;

  old_errno=errno;
  fprintf(stderr,"\nerror: %s: %s\n",
         where,
         strerror(errno));
  errno=old_errno;
  perror(where);
  exit(1);
}

int showdaemonoutput = 0;

/* reads line from stream */
void readaline (FILE *stream,int closing_connection)
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

  line[0]=(char) 0;
  length = getline( &line, &max_length, stream );


  if ((length == -1) && 
      ((line == NULL) ||
       (line[0] == (char) 0)))
    {
      if (closing_connection == 1) 
        {
	  line = "220 Bye";
	}
      else
        reportsystemerror("eof on read");
    }
  else
    {
      if (max_length != 4096)
        reporterror("Line read was too long, possible attack?");
      
      if (showdaemonoutput)
        printf("\ndaemon: %s",line);
    }
  return;
}

const char *argp_program_version = 
  "version 1.5 for clc v3";
const char *argp_program_bug_address = 
 "debian bug database, package common-lisp-controller";
static char doc[] = "clc-send-command a program to send commands to the clc-build-daemon";

static char args_doc[] = "recompile <package> <implementation>\n"
"remove <package> <implementation>";

static struct argp_option options[] = {
  {"force-connect",  'F', 0,      0,  "Force the daemon to connect" },
  {"verbose",  'v', 0,      0,  "Produce verbose output" },
  {"quiet",    'q', 0,      0,  "Don't produce any output" },
  {"debug",    'd', 0,      0,  "Show daemon output" },
  { 0 }
};
     
struct arguments {
  char *args[3];
  int verbose, debug, forceconnect;
};
     
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
  /* Get the INPUT argument from `argp_parse', which we
     know is a pointer to our arguments structure. */
  struct arguments *arguments = state->input;
     
  if (arguments == NULL)
    {
      printf("no state?");
      return 0;
    }
  switch (key)
    {
    case 'q':
      arguments->verbose = 0;
      break;
    case 'v':
      arguments->verbose = 1;
      break;
    case 'd':
      arguments->debug = 1;
      break;
    case 'F':
      arguments->forceconnect = 1;
      break;
    case ARGP_KEY_ARG:
      if (state->arg_num >= 3)
        /* Too many arguments. */
        argp_usage (state);
      
      if (state->arg_num == 0)
        {
          if ( !( (strcmp("remove",arg) == 0) || 
                  (strcmp("recompile",arg) == 0)))
            argp_usage (state);
        }
      arguments->args[state->arg_num] = arg;
      
      break;
     
    case ARGP_KEY_END:
      if (state->arg_num < 3)
        /* Not enough arguments. */
        argp_usage (state);
      break;
     
    default:
      return ARGP_ERR_UNKNOWN;
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
	reportsystemerror("Could not close directory stream!?");
      return 1;
    }
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
          char errormessage[4097];

          snprintf(errormessage,4096,"While deleting the file: %s with %i: %i %i",filename,flag,(flag & FTW_F),(flag & FTW_SL));
          errormessage[4096]=(char)0;
          reportsystemerror(errormessage);
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
          char errormessage[4097];

          snprintf(errormessage,4096,"While deleting the directory: %s",filename);
          errormessage[4096]=(char)0;
          reportsystemerror(errormessage);
          return 1;
        }
    }
  return 0;
}

void  nuke_package(char *package,char *compiler)
{
  char path[4097];


  snprintf(path,4096,"/usr/lib/common-lisp/%s/%s",
           compiler,package);
  path[4096]=(char)0;
  ftw(path, &delete_stuff, 150);
  while (probe_directory(path)) {
      ftw(path, &delete_directories, 150);
   }
}
     
static struct argp argp = { options, parse_opt, args_doc, doc };

int main(int argc, char *argv[])
{
  struct arguments arguments;
  struct sockaddr_in addr;
  struct hostent *hostinfo;
  int socketfd;
  FILE *stream;
  int removep=-1;
  int succesp;
  int opt;
  int closing_connection=0;

  /* Default values. */
  arguments.verbose = 1;
  arguments.debug = 0;
  arguments.forceconnect = 0;
  arguments.args[0] = NULL;
  arguments.args[1] = NULL;
  arguments.args[2] = NULL;

  argp_parse( &argp, argc, argv, 0,0,&arguments);

  if (arguments.debug)
    showdaemonoutput = 1;

  if (strcmp("remove",arguments.args[0]) == 0)
    removep = 1;
  else 
    {
      if (strcmp("recompile",arguments.args[0]) == 0)
        removep = 0;
      else
        reporterror("internal error icecream");
    }
  
  socketfd = socket( PF_INET, SOCK_STREAM, 0);
  if (socketfd < 0)
    reportsystemerror("Could not open socketfd");

  addr.sin_family = AF_INET;
  addr.sin_port = htons( 8990 );

  hostinfo = gethostbyname( "localhost" );
  if (hostinfo == NULL)
    reportsystemerror("could not gethostbyname, as I ask for localhost this is not possible, please check /etc/hosts");

  addr.sin_addr = *(struct in_addr *) hostinfo->h_addr;

  if (connect( socketfd, &addr, sizeof(addr)) != 0)
    {
      /* We could not connect to the daemon. 
	 Can we handle this on our own?
	 Yes we can! */
      if (removep == 1) 
	{
	  /* code lifted and adapted from clc-build-daemon */
	  char *package=arguments.args[1];
	  char *compiler=arguments.args[2];
	  /* characters allowed in the package and compiler names: */
	  char allowedcharacters[]="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-";

	  printf("I cannot contact the clc-build-daemon. Possibly you are trying to upgrade it.\n");
	  printf("I will try to remove the package myself...\n");
	  
	  if ((package == (char *) NULL) ||
	      ( strlen(package) != strspn(package,allowedcharacters)))
            {
	      printf("The package %s is invalid!\n", package);
	      exit(2);
	    }
	  else
	    {
	    if ((compiler == (char *) NULL) ||
		( strlen(compiler) != strspn(compiler,allowedcharacters)))
	      {
		printf("The compiler %s is invalid!\n", compiler);
		exit(2);
	      }
	    else
	      {
		char command[4097];
		
		snprintf(command,4096,"/usr/lib/common-lisp/%s/%s",
			 compiler,package);
		command[4096]=(char)0;
		if (! probe_directory(command) )
		  {
		    printf("Cannot remove  package %s for compiler %s\n",package,compiler);
		  }
		else
		  {
		    nuke_package(package,compiler);
		  }
	      }
	    }
	  exit(0);
	}
      else
	{
	  char *package=arguments.args[1];
	  char *compiler=arguments.args[2];
	  /* characters allowed in the package and compiler names: */
	  char allowedcharacters[]="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-";


	  if (arguments.forceconnect == 1)
	    {
	      printf("I cannot reach the daemon and I have to. Quiting with an error!\n");
	      exit(1);
	    }
	  printf("I cannot contact the clc-build-daemon. Possibly you are trying to upgrade it.\n");
	  printf("I will leave a not to rebuild this package...\n");
	  
	  if ((package == (char *) NULL) ||
	      ( strlen(package) != strspn(package,allowedcharacters)))
            {
	      printf("The package %s is invalid!\n", package);
	      exit(2);
	    }
	  else
	    {
	      if ((compiler == (char *) NULL) ||
		  ( strlen(compiler) != strspn(compiler,allowedcharacters)))
		{
		  printf("The compiler %s is invalid!\n", compiler);
		  exit(2);
		}
	      else
		{
		  /* Everything looks valid, so let us leave a
		     message for the builder */
		  char filename[4097];
		  struct utsname buf;
		  FILE *f;
		  
		  if (uname(&buf)  != 0)
		    {
		      reportsystemerror("Could not get my hostname.");
		      exit(43);
		    }
		  snprintf(filename,4096,"/var/spool/common-lisp-controller/%s-%i",
			   buf.nodename, (int) getpid());
		  filename[4096]=(char)0;
		  
		  if ( (f=fopen(filename,"wx")) == NULL )
		    {
		      reportsystemerror("Could not open spool file");
		      exit(44);
		    }
		  
		  fprintf(f,"%s %s\n",package,compiler);
		  fclose(f);
		}
	    }
	  exit(0);
	}
    }

  stream = fdopen(socketfd, "r+");

  if (stream == NULL)
    reportsystemerror("Could not convert to a stream");

  opt=1;
  if (setsockopt(socketfd, SOL_SOCKET, SO_KEEPALIVE, &opt, sizeof(opt)) != 0)
    reportsystemerror("Could not set option for socket");

  if (setvbuf(stream, (char *) NULL, _IOLBF, BUFSIZ) != 0)
    reportsystemerror("Could not make stream line buffered");

  for(succesp=1;;)
    {
      char code[4];

      /* get input */
      fflush(stream);
      readaline(stream,closing_connection);

      strncpy(code,line,3);
      code[3]=(char) 0;
      
      if (strcmp("100",code) == 0) {
        /* hello */
        if (arguments.verbose)
          fprintf(stream,"SHOW-OUTPUT\n");
        else
          fprintf(stream,"HIDE-OUTPUT\n");
        if (arguments.debug)
          {
            if (arguments.verbose)
              printf("Sending: SHOW-OUTPUT\n");
            else
              printf("Sending HIDE-OUTPUT\n");
          }
        continue; 
      }
      if ((strcmp("200",code) == 0) ||
          (strcmp("201",code) == 0)) {
        /* we show or hide */
        if (removep == 1)
          fprintf(stream,"REMOVE %s %s\n",arguments.args[1],arguments.args[2]);
        else
          fprintf(stream,"RECOMPILE %s %s\n",arguments.args[1],arguments.args[2]);

        if (arguments.debug)
          {
            if (removep == 1)
              printf("Sending: REMOVE %s %s\n",arguments.args[1],arguments.args[2]);
            else
              printf("Sending: RECOMPILE %s %s\n",arguments.args[1],arguments.args[2]);

          }
        continue; 
      }
      if (strcmp("250",code) == 0) {
        /* operation started... */
        continue; 
      }
      if ((strcmp("251",code) == 0) ||
          (strcmp("252",code) == 0)) {
        /* ok finished */
        fprintf(stream,"QUIT\n");
        closing_connection=1;
        if (arguments.debug)
          {
            printf("Sending: QUIT\n");
          }
        continue; 
      }
      if (strcmp("540",code) == 0) {
        printf("\nCannot remove: not yet compiled library for implementation\n");
	/* Change this to a success since CLC tries to remove a
	   compiled library before recompiling. Some CLC Lisp
	   implementations die if this returns an error status */
	/* succesp = 0; */
        succesp = 1;
        continue; 
      }
      if (strcmp("550",code) == 0) {
        printf("\nCannot compile: library for implementation already compiled\n");
        succesp = 0;
        fprintf(stream,"QUIT\n");
        closing_connection=1;
        if (arguments.debug)
          {
            printf("Sending: QUIT\n");
          }
        continue; 
      }
      if (strcmp("501",code) == 0) {
        printf("\nCannot compile: compilation error\n");
        succesp = 0;
        continue; 
      }
      if (strcmp("500",code) == 0) {
        printf("\nParameter problem?\n");
        succesp = 0;
        fprintf(stream,"QUIT\n");
        closing_connection=1;
        if (arguments.debug)
          {
            printf("Sending: QUIT\n");
          }
        continue; 
      }
      if ((strcmp("300",code) == 0) ||
          /* start output */
          (strcmp("331",code) == 0)) {
        /* end build output */
        continue; 
      }
      if (strcmp("310",code) == 0) {
        /* build output */
        if (arguments.verbose)
          printf("%s",&(line[4]));
        continue; 
      }
      if (strcmp("220",code) == 0) {
        fclose(stream);
        shutdown(socketfd,2);
        if (succesp)
          exit(0);
        else
          exit(255);
      }
    }
}
