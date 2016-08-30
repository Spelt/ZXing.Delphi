This app demonstrates how to compile ZXIng in order to use VCL.Graphics TBitmap,
instead of FMX.Graphics TBitmap.

This will allow you to use ZXIng library in a conventional VCL application,
instead of in a FireMonkey application.

all you have to do is to define the USE_VCL_BITMAP conditional define when
compiling your project.

Note: It is technically possible to include the FMX bitmap support in a VCL
 application and not use this define, but this leads to a bigger executable
 and if you deploy your application with runtime packages this would force you
 to deploy also the firemonkey runtime BPLs.

