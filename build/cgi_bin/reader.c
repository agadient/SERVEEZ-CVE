#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
int main() {
    char buf[0x10000];
    FILE * f = NULL;
    memset(buf, 0, sizeof(buf));
    read(0, buf, sizeof(buf));
    write(1, buf, sizeof(buf));
    f = fopen("f", "w");
    fwrite(buf, 1, sizeof(buf), f);
    fclose(f);
    exit(0);

}
