#include <string.h>
#include <stdio.h>
#include <stdlib.h>
//prints n to stdout
void printInt(int n) {
   printf("%d\n", n);
   return;
}
//prints s to stdout
void printString(char* s) {
   printf("%s\n", s);
   return;
}
//prints 'runtime error' to stdout and terminates the program
void error() {
   printf("runtime error\n");
   exit(1);
}

//reads an integer from stdin
int readInt() {
   int n, result;
   //wczytaj wszystkie biale znaki do pierwszego innego znaku
   result = scanf("%d", &n);
   if (result == EOF) {
      error();
   }
   char c;
   while ((c = getc(stdin)) == ' ' || c == '\t' || c == '\n') {
   }
   ungetc(c, stdin);
   return n;
}

//reads one line from stdin and returns it
char* readString() {
   char *line = NULL;
   size_t size;
   if (getline(&line, &size, stdin) == -1) {
      error();
   }
   line[strlen(line)-1] = '\0';
   return line;
}

char* concat(char* s1,char* s2) {
   char* t = malloc(strlen(s1)+strlen(s2)+1);
   return strcat(strcpy(t,s1),s2);
}

/*
int main () {
   
     int x = readInt();
     char* y = readString();
     char* z = readString();

     printInt(x-5);
     printString(concat(y, z));
     return 0;
}*/
