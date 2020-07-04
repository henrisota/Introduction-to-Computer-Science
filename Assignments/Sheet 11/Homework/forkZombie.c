#include <stdio.h>
#include <stdlib.h> 
#include <sys/types.h> 
#include <unistd.h>

int main(int argc, char *argv[])  {
    for (; argc > 1; argc--) {
        printf("Parent awake!\n");
        pid_t pid = fork();
        // Check if process is parent
        if (pid > 0) {
            printf("Parent put to sleep!\n");
            // Make parent sleep for 1 second therefore prolonging 
            // the wait() call of the parent to check on the child and
            // remove the child process entry from the process table
            sleep(1); 
        } else if (pid == 0) {
            // Child process will exit therefore terminated before the
            // parent process was able to reap the child using wait()
            printf("Child process turning into zombie!\n");
            exit(0);
            // After exiting, our child process becomes a zombie process
        }
    }
    sleep(20); // Place program to sleep in order to stop execution and document
    return 0; 
}
