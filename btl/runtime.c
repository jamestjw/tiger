#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int tigermain(int);

int main() {
  /* Call with static link */
  return tigermain(0);
}

void print(char *s) {
  printf("%s", s);
}

void printint(int64_t d) {
  printf("%lld", d);
}

int *initArray(int size, int init) {
  int i;
  int *a = (int *)malloc(size*sizeof(int));
  for(i=0;i<size;i++) a[i]=init;
  return a;
}

int *allocRecord(int size) {
  int i;
  int *p, *a;
  p = a = (int *) malloc(size);

  for(i=0;i<size;i+=sizeof(int)) *p++ = 0;

  return a;
}

int64_t ord(const char* const s) {
  if(strcmp(s, "") == 0) {
    return -1;
  } else {
    return s[0];
  }
}

const char* chr(int64_t i) {
  if (i<0 || i>255) {
    printf("Called chr with out of range argument: %lld", i);
    exit(1);
  }

  char *p = malloc(2);

  p[0] = i;
  p[1] = '\0';

  return p;
}

int not(int i) {
  return !i;
}

