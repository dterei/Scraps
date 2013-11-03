#include <queue> // functional,iostream,ctime,cstdlib
#include <iostream>
using namespace std;

int main(int argc, char* argv[]) {
	priority_queue<int,vector<int>,greater<int> > q;
	srand(time(0));

	cout << "Generating 100 random numbers and taking 10 largest.";
	for( int i = 0; i != 100; ++i ) {
		if (q.size() >= 10) q.pop();
		q.push(rand()%100);
	}

	cout << "Min-heap, popped one by one: ";
	while( ! q.empty() ) {
		cout << q.top() << ' ';
		q.pop();
	}
	cout << endl;

	return 0;
}

