#include <stdlib.h>
#include <stdio.h>

void swap(char** a, char** b) {
    char * tmp = *a;
    *a = *b;
    *b = tmp;
}

int main() {

    char * data[4];
    char ** x;
    
    data[0] = "Hello";
    data[1] = "there";
    data[2] = "What";
    data[3] = "up";

    x = data;

    printf("%c\n", (*x)[2]);

    printf("%s, %s\n", *data, *(data + 2));

    swap(data, data + 2);

    printf("%s, %s\n", *data, *(data + 2));

    return 0;
}
