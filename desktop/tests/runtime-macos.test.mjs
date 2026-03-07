import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import {
  compareMacOSVersions,
  findDisallowedAbsoluteReferences,
  isMachOBinary,
  isAllowedAbsoluteSystemReference,
  normalizeMacOSVersion,
  parseOtoolInstallName,
  parseOtoolLinkedLibraries,
  parseOtoolMinimumMacOSVersions
} from "../scripts/runtime-macos-utils.mjs";

function run() {
  assert.equal(normalizeMacOSVersion("13.0"), "13.0");
  assert.equal(normalizeMacOSVersion("13.0.1"), "13.0.1");
  assert.throws(() => normalizeMacOSVersion("13"), /invalid macOS version/);

  assert.equal(compareMacOSVersions("13.0", "13.0.0"), 0);
  assert.equal(compareMacOSVersions("13.1", "13.0.9"), 1);
  assert.equal(compareMacOSVersions("12.6", "13.0"), -1);

  const linkedOutput = `example.so:
\t@loader_path/../lib/libR.dylib (compatibility version 4.0.0, current version 4.0.0)
\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1292.100.5)
\t/opt/homebrew/opt/freetype/lib/libfreetype.6.dylib (compatibility version 28.0.0, current version 28.4.0)
`;
  assert.deepEqual(parseOtoolLinkedLibraries(linkedOutput), [
    "@loader_path/../lib/libR.dylib",
    "/usr/lib/libSystem.B.dylib",
    "/opt/homebrew/opt/freetype/lib/libfreetype.6.dylib"
  ]);

  const installNameOutput = `example.dylib:
\t@loader_path/example.dylib
`;
  assert.equal(parseOtoolInstallName(installNameOutput), "@loader_path/example.dylib");

  const loadOutput = `
Load command 12
      cmd LC_BUILD_VERSION
  cmdsize 32
 platform 1
    minos 13.0
      sdk 14.4
Load command 13
      cmd LC_VERSION_MIN_MACOSX
  cmdsize 16
  version 12.6
      sdk 13.3
`;
  assert.deepEqual(parseOtoolMinimumMacOSVersions(loadOutput), ["13.0", "12.6"]);

  assert.equal(isAllowedAbsoluteSystemReference("/usr/lib/libSystem.B.dylib"), true);
  assert.equal(isAllowedAbsoluteSystemReference("/System/Library/Frameworks/CoreText.framework/CoreText"), true);
  assert.equal(isAllowedAbsoluteSystemReference("/opt/homebrew/lib/libfreetype.6.dylib"), false);

  assert.deepEqual(
    findDisallowedAbsoluteReferences([
      "@loader_path/libR.dylib",
      "/usr/lib/libSystem.B.dylib",
      "/opt/homebrew/lib/libfreetype.6.dylib",
      "/Users/test/runtime/lib/libfoo.dylib"
    ]),
    [
      "/opt/homebrew/lib/libfreetype.6.dylib",
      "/Users/test/runtime/lib/libfoo.dylib"
    ]
  );

  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "runtime-macos-test-"));
  const javaClassPath = path.join(tmpDir, "Example.class");
  fs.writeFileSync(javaClassPath, Buffer.from([0xca, 0xfe, 0xba, 0xbe, 0x00, 0x00, 0x00, 0x3d]));
  assert.equal(isMachOBinary(javaClassPath), false);
  fs.rmSync(tmpDir, { recursive: true, force: true });
}

run();
console.log("runtime-macos.test.mjs: OK");
