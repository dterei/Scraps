CC=gcc
C_OPTS=-O2
LIBS=-lrt -lpthread

all:
	$(CC) $(C_OPTS) $(LIBS) time.c -o time
	$(CC) $(C_OPTS) $(LIBS) ftime.c -o ftime
	$(CC) $(C_OPTS) $(LIBS) gettimeofday.c -o gettimeofday
	$(CC) $(C_OPTS) $(LIBS) clock_gettime.c -o clock_gettime
	$(CC) $(C_OPTS) $(LIBS) cached_time.c -o cached_time

run:
	./time
	./ftime
	./gettimeofday
	./clock_gettime
	./cached_time

clean:
	rm -f *.o
	rm -f time
	rm -f ftime 
	rm -f gettimeofday
	rm -f clock_gettime
	rm -f cached_time

