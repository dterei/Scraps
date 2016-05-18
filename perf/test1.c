void work(int n)
{
  volatile int i=0; //don't optimize away
  while(i++ < n);
}

void easy()
{
	work(1000);
}

void hard()
{
	work(1000*1000*1000);
}

int main()
{
	easy();
	hard();
}
