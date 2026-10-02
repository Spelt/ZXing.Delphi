# zxing-cpp black box test samples

These images and their expected results are a copy of the `test/samples`
folder of [zxing-cpp](https://github.com/zxing-cpp/zxing-cpp), commit
[`2ecec3f`](https://github.com/zxing-cpp/zxing-cpp/tree/2ecec3f/test/samples)
(2026-09-30). zxing-cpp is licensed under the Apache License 2.0, like
ZXing.Delphi.

They are used by:

- `unitTest\ZXingCppSamplesTest.pas`: checks per folder that ZXing.Delphi
  does not read fewer symbols than before (regression test).
- `benchmark\ZXingBenchmark.dpr` (build with `benchmark\build.cmd`): shows per
  folder how many symbols ZXing.Delphi reads next to zxing-cpp, and how fast.

Every folder holds the images of one format, with the expected content in a
`.txt` or `.toml` file per image and folder defaults in `!defaults.toml`.
The keys `find` and `missing` tell in which modes (slow, fast, pure) and
rotations zxing-cpp reads the symbol. See
`test/blackbox/BlackboxTestRunner.cpp` in zxing-cpp for the exact rules.

Do not edit the files: `.gitattributes` turns off line ending conversion,
because some expected texts contain CR LF on purpose.

To update to a newer zxing-cpp version, replace the folder contents with the
`test/samples` folder of that version and update the commit above.
