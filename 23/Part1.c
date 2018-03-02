#include <stdio.h>
int main(void) {
        long muls = 0;
        long a = 0;
        long b = 0;
        long c = 0;
        long d = 0;
        long e = 0;
        long f = 0;
        long g = 0;
        long h = 0;
        a = 0;
        b = 99;
        c = b;
        if (a != 0) {
                b *= 100;
                muls++;
                b += 100000;
                c = b;
                c += 17000;
        }
        do {
                f = 1;
                d = 2;
                do {
                        e = 2;
                        do {
                                g = d;
                                g *= e;
                                muls++;
                                g -= b;
                                if (g == 0) {
                                        f = 0;
                                }
                                e++;
                                g = e;
                                g -= b;
                        } while (g != 0);
                        d++;
                        g = d;
                        g -= b;
                } while (g != 0);
                if (f == 0) {
                        h++;
                }
                g = b;
                g -= c;
                if (g != 0) {
                        b += 17;
                } else {
                        break;
                }
        } while (g != 0);
        printf("%ld\n", muls);
        return 0;
}
