#include <stdio.h>
#include <stdlib.h>
#define/*

f = ARGV[0]
system("wasm2c", f, "-o", f + ".c", exception: true)
system("gcc", "-DWASM_RT_MODULE_PREFIX=Z_" + File.basename(f, ".wasm").gsub(".", "Z2E"), "-o", "dummy-wasi-exec", f + ".c", __FILE__, "-include", f + ".h", "-lwasm-rt-impl", "-I.", exception: true)
exec("./dummy-wasi-exec")
__END__
*/DUMMY

#ifndef WASM_RT_ADD_PREFIX
#define WASM_RT_PASTE_(x, y) x ## y
#define WASM_RT_PASTE(x, y) WASM_RT_PASTE_(x, y)
#define WASM_RT_ADD_PREFIX(x) WASM_RT_PASTE(WASM_RT_MODULE_PREFIX, x)
#endif

// This is a dummy WASI implementation based on wasm2c.
// It supports only "wasi_snapshot_preview1.fd_write" for stdout.
// This should be replaced if a proper WASI implementation
// (such as wasmtime) becomes available.
//
// Usage:
//   ruby dummy-wasi-runtime.c foo.wasm

u32 fd_write(u32 fd, u32 iovs, u32 iovsLen, u32 size) {
	if (fd != 1) abort();

	u32 total_len = 0;
	u8 *mem = WASM_RT_ADD_PREFIX(Z_memory)->data;
	for (; iovsLen--; iovs += 8) {
		u32 ptr = *(u32*)&mem[iovs];
		u32 len = *(u32*)&mem[iovs + 4];
		fwrite(&mem[ptr], 1, len, stdout);
		total_len += len;
	}
	*(u32*)&mem[size] = total_len;

	return 0;
}

u32 (*Z_wasi_snapshot_preview1Z_fd_write)(u32, u32, u32, u32) = fd_write;

int main() {
	WASM_RT_ADD_PREFIX(_init)();
	WASM_RT_ADD_PREFIX(Z__start)();
	WASM_RT_ADD_PREFIX(_free)();
	return 0;
}
