#include "Complex.h"
using namespace std;

template<typename T>
T square(T x)
{
    return x*x;
}

int main() {
    cout << square<int>(5) << endl;
    cout << square<double>(5.3) << endl;
    Complex z=Complex(5,3);
    cout << square<Complex>(z) << endl;
    return 0;
}
