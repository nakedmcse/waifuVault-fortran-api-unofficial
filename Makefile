all: curl fjson sdk samples

build: curl fjson sdk

buildunittests: curl fjson testingsdk unittests

fjson:
	git clone https://github.com/nakedmcse/fjson.git
	make -C fjson fjson
	cp fjson/fjson.a fjson.a
	cp fjson/fjson.mod fjson.mod
	rm -rf fjson

curl: libcurl/curl.f90 libcurl/curl_easy.f90 libcurl/curl_multi.f90 libcurl/curl_urlapi.f90 libcurl/curl_util.f90 libcurl/curl_macro.c
	gcc -o libcurl/curl_macro.o -c libcurl/curl_macro.c
	gfortran -o libcurl/curl_util.o -c libcurl/curl_util.f90
	gfortran -o libcurl/curl_easy.o -c libcurl/curl_easy.f90
	gfortran -o libcurl/curl_multi.o -c libcurl/curl_multi.f90
	gfortran -o libcurl/curl_urlapi.o -c libcurl/curl_urlapi.f90
	gfortran -o libcurl/curl.o -c libcurl/curl.f90
	ar rcs libcurl/libfortran-curl.a libcurl/curl.o libcurl/curl_easy.o libcurl/curl_multi.o libcurl/curl_urlapi.o libcurl/curl_util.o libcurl/curl_macro.o
	rm -f libcurl/*.o

sdk: httpcallbackModule.f90 waifuModelsModule.f90 waifuUtilsModule.f90 waifuDeserializersModule.f90 waifuAPIModule.f90
	gfortran -c httpcallbackModule.f90
	gfortran -c waifuModelsModule.f90
	gfortran -c waifuUtilsModule.f90
	gfortran -c waifuDeserializersModule.f90
	gfortran -cpp -c waifuAPIModule.f90
	ar rcs lib-waifuvault.a httpcallbackModule.o waifuModelsModule.o waifuUtilsModule.o waifuDeserializersModule.o waifuAPIModule.o
	rm -f *.o

testingsdk: httpcallbackModule.f90 waifuModelsModule.f90 waifuUtilsModule.f90 waifuDeserializersModule.f90 waifuAPIModule.f90 waifuMocksModule.f90
	gfortran -c httpcallbackModule.f90
	gfortran -c waifuModelsModule.f90
	gfortran -c waifuUtilsModule.f90
	gfortran -c waifuDeserializersModule.f90
	gfortran -ffree-line-length-0 -c waifuMocksModule.f90
	gfortran -DWAIFUVAULT_UNIT_TEST -cpp -c waifuAPIModule.f90
	ar rcs lib-waifuvault.a httpcallbackModule.o waifuModelsModule.o waifuUtilsModule.o waifuDeserializersModule.o waifuAPIModule.o waifuMocksModule.o
	rm -f *.o

unittests: lib-waifuvault.a libcurl/libfortran-curl.a fjson.a waifuUnitTests.f90
	gfortran -ffree-line-length-0 -o waifuunittests waifuUnitTests.f90 lib-waifuvault.a libcurl/libfortran-curl.a fjson.a -lcurl

samples: lib-waifuvault.a fjson.a libcurl/libfortran-curl.a samples/waifutest.f90 samples/waifubuckettest.f90 samples/waifurestrictiontest.f90 samples/waifualbumtest.f90
	gfortran -o samples/waifutest lib-waifuvault.a libcurl/libfortran-curl.a samples/waifutest.f90 fjson.a -lcurl
	gfortran -o samples/waifubuckettest lib-waifuvault.a libcurl/libfortran-curl.a samples/waifubuckettest.f90 fjson.a -lcurl
	gfortran -o samples/waifurestrictiontest lib-waifuvault.a libcurl/libfortran-curl.a samples/waifurestrictiontest.f90 fjson.a -lcurl
	gfortran -o samples/waifualbumtest lib-waifuvault.a libcurl/libfortran-curl.a samples/waifualbumtest.f90 fjson.a -lcurl

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
	rm -f waifualbumtest
	rm -f waifuunittests

