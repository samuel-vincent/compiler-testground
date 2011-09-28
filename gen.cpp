
#include <iostream>
#include <vector>
#include <Windows.h>

typedef unsigned char byte;

typedef int (*pfn)(void);

extern "C" int fn();

int main (int argc, char** argv) {
	
	std::cout << fn() << std::endl;

	// Allocate memory

	byte* mem = (byte*) VirtualAllocEx(GetCurrentProcess(), 
									   NULL, 
									   1<<16, 
									   MEM_COMMIT|MEM_RESERVE, 
									   PAGE_EXECUTE_READWRITE
									   );

	if (mem == 0) return 1;
	
	byte* i = mem;

	// Write instructions

	*i++ = 0x48; *i++ = 0xb8; // mov rax, 
	*i++ = 0x02; *i++ = 0x00; *i++ = 0x00; *i++ = 0x00; 
	*i++ = 0x00; *i++ = 0x00; *i++ = 0x00; *i++ = 0x00; // 0x00000002
	*i++ = 0xc3; // ret

	// Execute instructions

	pfn f = (pfn) mem;
	std::cout << f() << std::endl;

	// Delay

	int in = 0;
	std::cin >> in;
	
	return 0;
}
