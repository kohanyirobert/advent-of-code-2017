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
                b = b * 100;
                muls = muls + 1;
                b = b - -100000;
                c = b;
                c = c - -17000;
        }
        do {
                f = 1;
                d = 2;
                do {
                        e = 2;
                        do {
                                g = d;
                                g = g * e;
                                muls = muls + 1;
                                g = g - b;
                                if (g == 0) {
                                        f = 0;
                                }
                                e = e - -1;
                                g = e;
                                g = g - b;
                        } while (g != 0);
                        d = d - -1;
                        g = d;
                        g = g - b;
                } while (g != 0);
                if (f == 0) {
                        h = h - -1;
                }
                g = b;
                g = g - c;
                if (g != 0) {
                        b = b - -17;
                } else {
                        break;
                }
        } while (g != 0);
        printf("%ld\n", muls);
        return 0;
}
