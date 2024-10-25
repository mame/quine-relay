'use strict';

const fs = require('fs');
const { WASI } = require('wasi');
const wasi = new WASI({ version: "preview1" });
const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

(async () => {
  const wasm = await WebAssembly.compile(fs.readFileSync(process.argv[2]));
  const instance = await WebAssembly.instantiate(wasm, importObject);
  wasi.start(instance);
})();
