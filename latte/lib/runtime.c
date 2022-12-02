#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int x) {
	printf("%d\n", x);
}

void printString(char *s) {
	printf("%s\n", s);
}

void error() {
    fprintf(stderr, "runtime");
	exit(-1);
}

char *readString() {
    char* str = NULL;
    size_t size;
    getline(&str, &size, stdin);
    int i = 0;
    while(i < size) {
        if(str[i] == '\n'){
            str[i] = 0;
            break;
        }
        i++;
    }
    if(i == size) {
        str[size - 1] = 0;
    }
    return str;
}

int readInt() {
    int i;
    scanf("%d\n", &i);

    return i;
}

char *__concatString__(char* s1, char* s2) {
    char* t = malloc(strlen(s1) + strlen(s2) + 1);

    return strcat(strcpy(t, s1), s2);
}

int __cmpString__(char* s1, char* s2) {
    return strcmp(s1, s2);
}

char *__llvmMemcpy__(int mem) {
    char *res = calloc(1, mem);
    return res;
}
