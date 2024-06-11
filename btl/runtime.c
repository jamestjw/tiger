#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int tigermain(int);

int main() {
  /* Call with static link */
  return tigermain(0);
}

void print(char *s) { printf("%s", s); }

int64_t *initArray(int64_t size, int64_t init) {
  int i;
  int64_t *a = (int64_t *)malloc(size * sizeof(int64_t));

  for (int i = 0; i < size; i++)
    a[i] = init;

  return a;
}

int *allocRecord(int size) {
  int i;
  int *p, *a;
  p = a = (int *)malloc(size);

  for (i = 0; i < size; i += sizeof(int))
    *p++ = 0;

  return a;
}

int64_t ord(const char *const s) {
  if (strcmp(s, "") == 0) {
    return -1;
  } else {
    return s[0];
  }
}

const char *chr(int64_t i) {
  if (i < 0 || i > 255) {
    printf("Called chr with out of range argument: %lld", i);
    exit(1);
  }

  char *p = malloc(2);

  p[0] = i;
  p[1] = '\0';

  return p;
}

int not(int i) { return !i; }

/* String operations  */

/* Number of characters in s. */
int64_t size(char *s) { return strlen(s); }

/* Substring of string `s`, starting with character `first`, `n` characters
 * long. Characters are numbered starting at 0. */
char *substring(const char *s, int64_t first, int64_t n) {
  int substr_len;
  int s_len = strlen(s);

  // Ensure `first` is within bounds.
  if (first < 0 || first >= s_len) {
    fprintf(stderr, "`first` is out of bounds\n");
    exit(1);
  }

  // Calculate the substring length.
  if (n < s_len - first) {
    substr_len = n;
  } else {
    substr_len = s_len - first;
  }

  // Allocate memory for the substring including the null terminator.
  char *p = malloc(substr_len + 1);
  if (p == NULL) {
    fprintf(stderr, "Allocation failed in substring function.\n");
    exit(1);
  }

  // Copy the substring.
  for (int i = 0; i < substr_len; i++) {
    p[i] = s[first + i];
  }

  // Null-terminate the substring.
  p[substr_len] = '\0';

  return p;
}

/* Concatenation of `s1` and `s2`. */
char *concat(const char *s1, const char *s2) {
  // Get the lengths of the input strings
  int len1 = strlen(s1);
  int len2 = strlen(s2);

  // Allocate memory for the concatenated string including the null terminator
  char *result = malloc(len1 + len2 + 1);
  if (result == NULL) {
    return NULL; // Allocation failed
  }

  // Copy the first string into the result
  strcpy(result, s1);

  // Concatenate the second string
  strcat(result, s2);

  return result;
}
