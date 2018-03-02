#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

int main(void) {
        bool debug = false;
        int start = 0;
        int end = 0;
        int composites = 0;
        if (debug) {
                start = 99;
                end = 99;
        } else {
                start = 109900;
                end = 126900;
        }
        do {
                bool prime = true;
                // unoptimized
                /*
                for (int i = 2; i < start; i++) {
                        for (int j = 2; j < start; j++) {
                                if ((i * j) - start == 0) {
                                        prime = false;
                                }
                        }
                }
                */
                // optimized
                for (int i = 2; i < floor(sqrt(start)) + 1; i++) {
                        if (start % i == 0) {
                                prime = false;
                                break;
                        }
                }
                if (!prime) {
                        composites++;
                }
                if (start - end == 0) {
                        break;
                } else {
                        start += 17;
                }
        } while (true);
        printf("%d\n", composites);
        return 0;
}
