(:schema-version 1
 :pdfium-version "151.0.7881.0"
 :pdfium-api "7881"
 :release "chromium/7881"
 :assets
 ((:system windows-nt
   :architecture x64
   :file "pdfium-win-x64.tgz"
   :library "bin/pdfium.dll"
   :sha256
   "73cc0de638ac2095e7445bf56a38200a5b7c7ca0e9f4ba144598f2457377ac08")
  (:system windows-nt
   :architecture arm64
   :file "pdfium-win-arm64.tgz"
   :library "bin/pdfium.dll"
   :sha256
   "d3035d4d2cacac6ecd1a2ece197a3d702a1b2a58466276b9f870b8cb278a9d84")
  (:system gnu/linux
   :architecture x64
   :file "pdfium-linux-x64.tgz"
   :library "bin/libpdfium.so"
   :sha256
   "1470e21b8b4a3b4ad7f85684e2da11d94f3b69a86d81dee11b9b6709d927ac1d")
  (:system gnu/linux
   :architecture arm64
   :file "pdfium-linux-arm64.tgz"
   :library "bin/libpdfium.so"
   :sha256
   "ee7f7b7d5468958336a818c1cd580bdd20972846b7377b13f9a923d92d1d4674")
  (:system darwin
   :architecture x64
   :file "pdfium-mac-x64.tgz"
   :library "bin/libpdfium.dylib"
   :sha256
   "6dedf83990e0e3d6b7c93c9e7589c5a126b0ae14b7464d76120cff7a26afb18b")
  (:system darwin
   :architecture arm64
   :file "pdfium-mac-arm64.tgz"
   :library "bin/libpdfium.dylib"
   :sha256
   "52e94ca5aa8847934330daf3f8150c190682c5ca93831468794f8b90d4392e40")))
