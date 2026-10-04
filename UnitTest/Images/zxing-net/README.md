# ZXing.Net black box test samples

The images and their expected texts in `msi-1` are a copy of the folder
`Source/test/data/blackbox/msi-1` of [ZXing.Net](https://github.com/micjahn/ZXing.Net)
(2026-10-04). ZXing.Net is licensed under the Apache License 2.0, like
ZXing.Delphi. Its MSI test requires 5 of the 6 images, also turned 180 degrees;
`06.png` has no standard MSI stop pattern.

They are used by `OneDTest.MSISamples`.

The images and texts in `imb-1` are a copy of `Source/test/data/blackbox/imb-1`
of ZXing.Net (2026-10-04); its IMb test requires 1 of them (7 with TRY_HARDER).
`05.txt` is corrected: the image holds `0004000015800000004075201313699`
(valid CRC and characters, ZIP 75201-3136-99), not the text of `01.txt`; and
`10.txt`: the photo holds `00708901047725000000397039900` (ZIP 39703-9900, as
the address next to it). The text of `08.txt` is the same as `01.txt` too and
probably wrong as well; that photo is not read. Used by `PostalTest.IMbSamples`.
