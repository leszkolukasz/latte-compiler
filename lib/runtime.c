#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>

void error() {
    fprintf(stderr, "runtime_error\n");
    exit(1);
}

void printInt(int64_t x) {
    printf("%lld\n", x);
}

void printChar(char c) {
    putchar(c);
    putchar('\n');
}

int64_t readInt() {
    int64_t x;
    scanf("%lld", &x);

    // Read the newline character so that getline() doesn't read it
    int c = getchar();
    if (c != '\n' && c != EOF) {
        error();
    }

    return x;
}

void printString(void* s, int64_t len) {
    for (int64_t i = 0; i < len; i++) {
        putchar(((char*)s)[i]);
    }
    putchar('\n');
}

void* readString() {
    char* buffer = NULL;
    size_t buffer_size = 0;
    ssize_t input_len = getline(&buffer, &buffer_size, stdin);

    if (buffer[input_len - 1] == '\n') {
        input_len--;
    }

    // vtable ptr + data ptr
    void** string_obj = calloc(2, sizeof(void*));

    asm("lea String_vtable(%%rip), %%rax ;"
        "mov %%rax, %0 ;"
        : "=m"(string_obj[0])
        :
        : "rax");


    // array length + array data
    void* boolean_array = calloc(1, sizeof(int64_t) + input_len * sizeof(char));
    *(int64_t*)boolean_array = (int64_t)input_len;

    char* data_array = (char*)boolean_array + sizeof(int64_t);
    memcpy(data_array, buffer, input_len);

    string_obj[1] = boolean_array;

    free(buffer);

    return string_obj;
}
