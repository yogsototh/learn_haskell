#include<iostream>
using namespace std;
class Complex
{
public:
    double real_; 
    double img_; 

    Complex(double real, double img) : real_(real), img_(img) {} 

    // overloaded the multiplication operator
    Complex operator*(Complex z)
    {
        return Complex(real_*z.real_ - img_*z.img_,
            img_*z.real_ + real_*z.img_);
    }
};
// to print Complexes
ostream& operator<<(ostream& os, const Complex& z) {
    os << z.real_ << " + " << z.img_ << "i";
    return os;
}

