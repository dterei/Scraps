all:
	gcc -O2 time.c -o time -lrt
	gcc -O2 ftime.c -o ftime -lrt
	gcc -O2 gettimeofday.c -o gettimeofday -lrt
	gcc -O2 clock_gettime.c -o clock_gettime -lrt
	gcc -O2 cached_time.c -o cached_time -lrt

run:
	./time
	./ftime
	./gettimeofday
	./clock_gettime
	./cached_time

clean:
	rm -f time
	rm -f ftime
	rm -f gettimeofday
	rm -f clock_gettime
	rm -f cached_time

