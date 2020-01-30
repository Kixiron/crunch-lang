#include <windows.h>
#include <stdio.h>
#include <intrin.h>

void StartTimer(long long *pt1) {
   QueryPerformanceCounter((LARGE_INTEGER*)pt1);
}

double StopTimer(long long t1) {
   long long t2, ldFreq;

   QueryPerformanceCounter((LARGE_INTEGER*)&t2);
   QueryPerformanceFrequency((LARGE_INTEGER*)&ldFreq);
   return ((double)(t2 - t1) / (double)ldFreq) * 1000.0;
}

int fibonacci(int n) {
    if (n < 2) {
        return 1;
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}

void main() {
    long long time;

    StartTimer(&time);

    fibonacci(20);

    printf("Time = %f\n", StopTimer(time));
}
