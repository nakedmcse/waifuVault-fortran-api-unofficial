all: curl sdk tests

build: curl sdk

curl: libcurl/curl.f90 libcurl/curl_easy.f90 libcurl/curl_multi.f90 libcurl/curl_urlapi.f90 libcurl/curl_util.f90 libcurl/curl_macro.c
	gcc -o libcurl/curl_macro.o -c libcurl/curl_macro.c
	gfortran -o libcurl/curl_util.o -c libcurl/curl_util.f90
	gfortran -o libcurl/curl_easy.o -c libcurl/curl_easy.f90
	gfortran -o libcurl/curl_multi.o -c libcurl/curl_multi.f90
	gfortran -o libcurl/curl_urlapi.o -c libcurl/curl_urlapi.f90
	gfortran -o libcurl/curl.o -c libcurl/curl.f90
	ar rcs libcurl/libfortran-curl.a libcurl/curl.o libcurl/curl_easy.o libcurl/curl_multi.o libcurl/curl_urlapi.o libcurl/curl_util.o libcurl/curl_macro.o
	rm -f libcurl/*.o

sdk: httpcallbackModule.f90 waifuModelsModule.f90 waifuUtilsModule.f90 waifuAPIModule.f90
	gfortran -c httpcallbackModule.f90
	gfortran -c waifuModelsModule.f90
	gfortran -c waifuUtilsModule.f90
	gfortran -cpp -c waifuAPIModule.f90
	ar rcs lib-waifuvault.a httpcallbackModule.o waifuModelsModule.o waifuUtilsModule.o waifuAPIModule.o
	rm -f *.o

tests: lib-waifuvault.a libcurl/libfortran-curl.a waifutest.f90 waifubuckettest.f90 waifurestrictiontest.f90
	gfortran -o waifutest lib-waifuvault.a libcurl/libfortran-curl.a waifutest.f90 -lcurl
	gfortran -o waifubuckettest lib-waifuvault.a libcurl/libfortran-curl.a waifubuckettest.f90 -lcurl
	gfortran -o waifurestrictiontest lib-waifuvault.a libcurl/libfortran-curl.a waifurestrictiontest.f90 -lcurl

clean:
	rm -f *.o
	rm -f *.a
	rm -f *.mod
	rm -f libcurl/*.o
	rm -f libcurl/*.a
	rm -f libcurl/*.mod
	rm -f waifutest
	rm -f waifubuckettest
	rm -f waifurestrictiontest
