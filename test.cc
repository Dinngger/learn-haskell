#include <iostream>
#include <HsFFI.h>
#include <chrono>

class TicToc {
private:
    std::chrono::_V2::system_clock::time_point start;
public:
    void tic() {
        start = std::chrono::system_clock::now();
    }
    // return in milliseconds
    double toc() {
        auto end = std::chrono::system_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
        return static_cast<double>(duration.count()) / 1000.0;
    }
};


extern "C" {
extern void hello(void);
}

void hello2(void) {
    long long sum = 0;
    long long i2 = 0;
    for (long long i=1; i < 2000000; i++) {
        i2 = i * i;
        sum += i2;
    }
    printf("%lld\n", sum);
}

int main(int argc, char* argv[]) {
    printf("hello world!\n");
    TicToc tt;
    hs_init(&argc, &argv);
    tt.tic();
    hello();
    printf("%f\n", tt.toc());
    tt.tic();
    hello2();
    printf("%f\n", tt.toc());
    hs_exit();
    return 0;
}
