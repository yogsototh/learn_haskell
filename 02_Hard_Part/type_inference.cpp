#include <iostream>
using namespace std;

class Complex
{
public:
    double real_; 
    double img_; 

    Complex(double real, double img) : real_(real), img_(img) {} 

    // overloaded the multiplication operator
    Complex operator*(Complex rhs)
    {
        return Complex(real_*rhs.real_ - img_*rhs.img_,
            img_*rhs.real_ + real_*rhs.img_);
    }
};

// No overload needed. This will work for numeric types and Complex<>.
template<typename T>
T square(T x)
{
    return x*x;
}

ostream& operator<<(ostream& os, const Complex& z) {
    os << z.real_ << " + " << z.img_ << "i";
    return os;
}

int main() {
    cout << square<int>(5) << endl;
    cout << square<double>(5.3) << endl;
    Complex z=Complex(5,3);
    cout << square<Complex>(z) << endl;
    return 0;
}
