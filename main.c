#include <stdio.h>

int main() {
	int x = 0;
	while (x < 1000000) {
		x += 1;
	}
	printf("x: %d", x);
}
