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
gfortran -cpp -c waifuAPIModule.f90
gfortran -o waifutest httpcallbackModule.o waifuModelsModule.o waifuAPIModule.o libcurl/libfortran-curl.a waifutest.f90 -lcurl