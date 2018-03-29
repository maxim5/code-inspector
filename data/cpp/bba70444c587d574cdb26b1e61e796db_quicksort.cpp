//
// Algorithm Practice - Quick Sort
//
// Mingjie Li (limingjie0719@gmail.com)
// Mar 11, 2014
//
// Compiled with
// - MinGW g++ 4.8.2
// - Visual Studio Express 2013
//

#include <iostream> // cout & endl
#include <iomanip>  // setw
#include <random>   // minstd_rand0 & uniform_real_distribution
#include <chrono>   // high_resolution_clock

template <typename T>
inline void swap(T &a, T &b)
{
    T temp = a;
    a = b;
    b = temp;
}

// Find median of three, and swap it with right most element
template <typename T>
void median_of_three(T *arr, int left, int right)
{
    // (left + right) / 2 may exceed INT_MAX
    int mid = left + (right - left) / 2;
    T l = arr[left], m = arr[mid], r = arr[right];

    if (l < r)
    {
        if (m < l)
            swap(arr[left], arr[right]);
        else if (m < r)
            swap(arr[mid], arr[right]);
        // else use right as median
    }
    else
    {
        if (l < m)
            swap(arr[left], arr[right]);
        else if (r < m)
            swap(arr[mid], arr[right]);
        // else use right as median
    }
}

template <typename T>
int partition(T *arr, int left, int right)
{
    T pivot = arr[right];
    int i = left - 1;

    for (int j = left; j < right; j++)
    {
        if (arr[j] <= pivot)
        {
            swap(arr[++i], arr[j]);
        }
    }
    swap(arr[++i], arr[right]);

    return i;
}

template <typename T>
void quicksort(T *arr, int left, int right)
{
    int pivot;

    if (left < right)
    {
        median_of_three(arr, left, right);
        pivot = partition(arr, left, right);
        quicksort(arr, left, pivot - 1);
        quicksort(arr, pivot + 1, right);
    }
}

int main()
{
    const int ArraySize  = 10000000;
    double *arr = new double[ArraySize];

    auto seed = std::chrono::high_resolution_clock::
                now().time_since_epoch().count();
    std::minstd_rand0 generator(seed);
    std::uniform_real_distribution<double> distribution(100.00, 999.99);

    for (int i = 0; i < ArraySize; i++)
    {
        arr[i] = distribution(generator);
    }

    std::cout.precision(10);

    std::cout << "Quick Sort " << ArraySize << " double numbers.\n"
                 "Print 100 samples before sorting :\n";

    for (int i = 0; i < ArraySize; i += ArraySize / 100)
    {
        std::cout << std::setw(16) << arr[i];
    }

    std::cout << "\nSorting...\n\n";

    quicksort(arr, 0, ArraySize - 1);

    std::cout << "Print 100 samples after sorting :\n";

    for (int i = 0; i < ArraySize; i += ArraySize / 100)
    {
        std::cout << std::setw(16) << arr[i];
    }

    return 0;
}
