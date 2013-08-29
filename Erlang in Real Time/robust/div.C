#include <iostream.h>

class DivZeroErr
{
    public:
        DivZeroErr() {}
};

int div(int a, int b) 
{
    if (b == 0)
        throw DivZeroErr();
    return(a / b);
}

int main()
{
    int a, b, r;
    cin >> a >> b;
    try 
    {
        r = div(a, b);
        cout << r << endl;
    }
    catch (DivZeroErr error)
    {
        cout << "Attempt to Divide by Zero" << endl;
    }
}
