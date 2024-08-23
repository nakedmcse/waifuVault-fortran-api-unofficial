#!/bin/sh
# Build libcurl
cd libcurl
gcc -c curl_macro.c
gfortran -c curl_util.f90
gfortran -c curl_easy.f90
gfortran -c curl_multi.f90
gfortran -c curl_urlapi.f90
gfortran -c curl.f90
ar rcs libfortran-curl.a curl.o curl_easy.o curl_multi.o curl_urlapi.o curl_util.o curl_macro.o
cp *.mod ../
# Build waifuvault
cd ..
gfortran -c httpcallbackModule.f90
gfortran -c waifuModelsModule.f90
gfortran -c waifuUtilsModule.f90
gfortran -cpp -c waifuAPIModule.f90
ar rcs lib-waifuvault.a httpcallbackModule.o waifuModelsModule.o waifuUtilsModule.o waifuAPIModule.o
gfortran -o waifutest httpcallbackModule.o waifuModelsModule.o waifuUtilsModule.o waifuAPIModule.o libcurl/libfortran-curl.a waifutest.f90 -lcurl
gfortran -o waifubuckettest httpcallbackModule.o waifuModelsModule.o waifuUtilsModule.o waifuAPIModule.o libcurl/libfortran-curl.a waifubuckettest.f90 -lcurl